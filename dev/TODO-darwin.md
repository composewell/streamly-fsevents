# Darwin TODO

## API design

- **Resume from a past event ID.** `since` is hard-coded to `SinceNow`, so a
  watch cannot resume after a restart, and `setFullHistory`/`isHistoryDone`
  are useless. Add a `setSinceEventId` setting and an accessor for the latest
  event ID. Resuming also needs the volume UUID, because event IDs are valid
  only on one volume.
- **Prompt close.** Only the bracketed stream API is exported. If the stream
  is not fully drained (e.g. `take`, or a fold that stops early) the watch
  stays open until GC: one FSEvents stream (limit 512), a dispatch queue and
  two fds. A loop of short-lived watches can hit the limit. Add `withWatch`,
  or export `openWatch`/`closeWatch` as on Linux.
- **Error reporting.** Pipe EMFILE, a path CoreFoundation cannot convert,
  more than 4096 paths, and a start failure all give the same
  "openWatch: failed to create watch." with no errno or reason.
- `setAllEvents` only toggles two flags; the name suggests more.
- `getRoot` returns the OS path for events outside every root, so it has two
  meanings.

## Documentation / user pitfalls

- **512-stream limit.** Each watch uses one stream, two fds and a dispatch
  queue. Prefer one watch with many roots (up to 4096).
- **Slow consumers.** When the 64 KB pipe is full the callback blocks and
  fseventsd eventually drops events, reporting `isEventsLost` with
  `UserDropped`/`KernelDropped`. Users must rescan on that event.
- **Flags accumulate.** One event can carry Created, Modified and Removed
  together; users should stat the path.
- **SIGPIPE.** With the default SIGPIPE action any GHC program is fragile:
  the RTS uses SIGPIPE to interrupt interruptible foreign calls, so e.g.
  `killThread` on a thread inside `openFile` kills the process.
- `setIgnoreSelf` hides events caused by every thread of the process, not
  only the watcher.
- Path matching works for: `/tmp` → `/private/tmp`, wrong-case root on
  case-insensitive APFS, trailing slash, relative root. A symlink root is
  resolved once, at start. APFS rejects invalid UTF-8 names.

## Suspected bugs (not verified)

- `writeEvent` ignores short writes and EINTR. A partial write would break
  record framing for good. Probably unreachable since libdispatch threads
  block most signals, but a write-until-done loop is cheap.
- If `open("/dev/null")` fails at close (fd limit), the `dup2` step is
  skipped and close can hang when the read end is shared.
- A fork in another thread between `pipe()` and `fcntl()` leaks the pipe fds
  to the child (macOS has no `pipe2`). Close is still safe.

## Performance

- `setRoot` converts through `String` on every event. This is slow and would
  mangle non-UTF-8 names on non-APFS volumes. Match on bytes instead.

## Not tested

- TCC/sandbox behaviour (`~/Documents`, removable volumes).
- Mount/unmount events.
- Dropped-event flags.
- Whether the 512-stream limit is the same on other macOS versions.
