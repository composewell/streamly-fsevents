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

## An event is lost when a read is interrupted by an async exception

A read of a watch that is interrupted by an async exception, for example
a timeout, can lose an event. The event is taken from the watch and the
exception arrives before the program receives the event.

On Linux `eventStream` runs each step of the stream with async exceptions
masked. An exception that arrives during a step that produces an event
is raised when the mask ends, and the event is not returned. The same
exception replaces an `IOException` of the step, so the failure is not
reported.

On macOS, 2000 reads interrupted after 1 to 200 microseconds lost 3 to
18 of the events of 300 files, with `eventStream` and with `readEvent`.
No event was lost in five runs when the caller masked async exceptions
around `readEvent` and the code that records the event. Windows is not
measured.

The test "does not lose an event when the reads time out repeatedly" in
`test/Streamly/Test/FileSystem/Event.hs` runs on all platforms and is
marked pending, with the number of events lost, instead of failing.

Proposed change: when an async exception arrives after an event is
taken from the watch, keep the event, or the `IOException`, in the watch
and return it from the next read. Then make the test fail when an event
is lost. The interval between the return of the read and the code of the
program that records the event remains; the docs should tell the program
to mask async exceptions around the two.

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

## References to dev documents that are not in the repository

The source, the tests and the docs refer to `dev/design.linux.md`,
`dev/design-issues.md` and `dev/testing.md`. These files are not in the
repository. The references are in
`src/Streamly/Internal/FS/Event/Linux.hs`,
`test/Streamly/Test/FileSystem/Event/Linux.hs`,
`test/Streamly/Test/FileSystem/Event/LinuxModel.hs`,
`test/Streamly/Test/FileSystem/Event/Behavior.hs` and
`docs/robustness.md`. Add the documents or remove the references.
