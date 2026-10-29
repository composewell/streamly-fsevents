# Windows: open issues

## API design

- `Event(..)` exports its raw fields. `totalBytes` is the read buffer size
  and also marks an overflow. The overflow event puts the root in
  `eventRelPath` because an empty `Path` cannot be built. Making `Event`
  abstract or using `Maybe Path` would fix this, but changes an exported
  type.
- Renames are not paired. There is no cookie; `isMovedFrom` and
  `isMovedTo` are separate events that are only adjacent.
- If one root fails, the whole stream ends. There is no per-root event
  like Linux's `isRootUnwatched`.
- No explicit `openWatch`/`closeWatch` and no way to add or remove roots.
  Cleanup relies on exceptions or GC.
- The 63 KiB read buffer is not configurable.
- `getAbsPath` returns a relative path when the root is relative.
- The `isMovedFrom`/`isMovedTo` docs say "moved from under / into a
  monitored directory", but moves across the tree boundary are probably
  reported as removed/added.

## Event semantics

- Lost events: if the consumer is slow, the 1500-event stream buffer
  fills, the worker stops reading and the OS buffer overflows. After
  `isEventsLost` the application has to rescan.
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
- Deleting the watch root or removing the volume produces no event for
  the root; the stream most likely ends with an exception.
- With the non-threaded RTS each blocking call stalls all Haskell threads
  until the next change.

## Unverified suspicions

- Root deleted / volume removed: which error code, and whether a
  delete-pending directory still reports events.
- More roots than the `parConcatMap` thread limit: each root needs a
  blocked worker, so extra roots may never be read.
- Handle closed externally: the read may return 0 bytes (reported as
  lost events) and the next call fails.
- If `CancelSynchronousIo` lands just before the system call starts, the
  read still waits for the next change.
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
   missing-root test should time out before 21bfcfb.
4. Manually: delete the watch root during a watch, force an overflow on
   an SMB share, unplug a USB volume, and build with the non-threaded
   RTS.
