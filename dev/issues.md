# Issues

## Darwin: use unsafe FFI calls for the constant getters

`src/Streamly/Internal/FS/Event/Darwin.hs` imports the FSEvents flag
constants, e.g. `kFSEventStreamCreateFlagNoDefer` and the
`FSEventStreamEventFlag*` functions, with `foreign import ccall safe`.
These C functions only return a constant, they do not block and do not
call back into Haskell. A `safe` call costs more than an `unsafe` call
in both the threaded and the non-threaded RTS. Import them with
`unsafe`.

## CI: run the non-threaded RTS tests on macOS

The `non-threaded` job in `.github/workflows/haskell.yml` builds with
`--flag -threaded` on `ubuntu-latest` only. The Darwin backend also has
code specific to the non-threaded RTS: `closeWatch` does not close the
read fd while a thread waits for it, because `select` in the
non-threaded RTS fails with `EBADF` on a closed fd and the RTS
terminates the program. No CI job tests this code. Add a `non-threaded`
job on `macos-latest`.

## Linux: isRootUnwatched is reported for a directory that stays watched

A new directory is created at `dir` and renamed to `dir-renamed`, then
the watched directory `dir1` is renamed to `dir`, all before the events
are read. The watch then reports `isRootUnwatched` for `dir` and
`dir/sub`, but the kernel watch of the directory is not removed and the
later events of `dir` and `dir/sub` are reported. The test
`recReusedPathMovedWithin` in `test/Streamly/Test/FileSystem/Event/Linux.hs`
asserts the current behavior.

`isRootUnwatched` should be reported only for a directory that is no
longer watched.

## macOS: a watch reports changes made before it was opened

On macOS a watch can report changes made shortly before `openWatch`,
for example the creation of the watch root. Linux and Windows report only
the changes made after the watch is opened. The common tests read and
discard these events on macOS, see `skipEarlier` in
`test/Streamly/Test/FileSystem/Event.hs`.

Proposed change: record `FSEventsGetCurrentEventId` when the watch is
opened and drop the events with an id that is not greater. To be checked:
FSEvents can report the flags of an earlier change, such as `isCreated`,
with a later event of the same path, and the id does not filter those.

## Windows: assert that a removed root stops reporting at once

`docs/robustness.md` states that on Windows no event of a root is reported
for a change made after `removeRoot` returns. The common tests
`removeStops` and `overlapRemoveInner` allow up to 100 rounds of changes
before the removed root stops reporting, on macOS and Windows alike. Run
the tests on Windows with one round and, if they pass, require it.

## Memory after the start of a watch with the listing through an fd

Listing a directory through its fd, with `fdopendir`, increased the live
heap after the start of a watch on 28277 directories from 5.3 MB to
5.9 MB, about 21 bytes per directory. The names are read by
`readDirStreamEither` in both versions, the cause is not known.
