# Windows TODO

Open problems in `src/Streamly/Internal/FS/Event/Windows.hs`, and the steps
to test it on Windows. Delete an entry when it is fixed.

## API design

- The internal module exports the raw fields with `Event(..)`.
  `totalBytes` is the read buffer size and also marks an overflow. The
  overflow event puts the root in `eventRelPath` because an empty `Path`
  cannot be built; `getRelPath` returns `Nothing` for it. Making `Event`
  abstract or `eventRelPath` a `Maybe Path` would fix this, but changes an
  exported type.
- Renames are not paired. There is no cookie; `isMovedFrom` and
  `isMovedTo` are separate events that are only adjacent.
- If the system fails to report the changes of a root, the read raises the
  exception and `watchWith` ends the stream. There is no per-root event
  like Linux's `isRootUnwatched`.
- The 63 KiB read buffer is not configurable.
- The `isMovedFrom`/`isMovedTo` docs say "moved from under / into a
  monitored directory", but moves across the tree boundary are probably
  reported as removed/added.

## Event semantics

- Lost events: if the consumer is slow, the event queue of the watch
  fills at 1024 events, the thread of the watch stops reading and the OS
  buffer overflows. After `isEventsLost` the application has to rescan.
- The two halves of a rename may arrive in different reads.
- One write can produce an added event plus several modified events; in
  recursive mode the parent directory reports modified events too.

## Paths and names

- An event may carry the 8.3 short name or the long name; Windows does
  not say which.
- Names come in the on-disk case, so compare case-insensitively.
- A relative root is resolved against the current directory when the
  watch opens.
- Paths longer than `MAX_PATH` need `\\?\` or a long-path-aware process.
- `Path.toString` still raises an exception on unpaired surrogates.

## Environment

- Network shares: 63 KiB is under the 64 KiB limit, but redirectors that
  do not support the call fail with `ERROR_INVALID_FUNCTION` and end the
  stream.
- Deleting the watch root reports `isDeleted` for the root and ends the
  watch of the root. Removing the volume is not tested, the stream most
  likely ends with an exception.

## Unverified suspicions

- Volume removed: which error code the read reports.
- Handle closed externally: the read may return 0 bytes (reported as
  lost events) and the next call fails.
- `watchWith` ends the stream when it has read the `isDeleted` event of
  each root. A root removed because its read failed is not counted, if
  all the other roots are deleted the stream waits forever. Linux ends
  the stream when no path is watched, the same check would cover this.
- Not run on Windows: the changes of this branch to `Windows.hs` were
  typechecked on Linux only, with stub `System.Win32` modules. They are
  the absolute path of a relative root (`toAbsolute`), the item put back
  in the queue by an interrupted read, and a path listed twice watched
  once.
- Do alternate data stream names (`file:stream`) show up in events?
- Does `Path.fromString` accept `\\?\` or UNC roots?
- No bounds check of `NextEntryOffset` against the bytes returned.
- The import uses `ccall`, wrong only on 32-bit Windows, which GHC no
  longer targets.

## Testing on Windows

1. `cabal build`, then `cabal test` for `FileSystem.Event.Windows` and
   `FileSystem.Event`, with `+RTS -M1g`. Consider adding
   `-with-rtsopts=-M1g` to the test-suites.
2. Run the Windows suite ~20 times to catch flakiness, especially the
   overflow and missing-root tests.
3. Check each new test fails on the parent of its fix commit; the
   missing-root test should time out before the commit "Windows: make the
   ReadDirectoryChangesW call interruptible".
4. Manually: delete the watch root during a watch, force an overflow on
   an SMB share, unplug a USB volume, and check that a watch built with
   the non-threaded RTS raises an exception when it starts.
