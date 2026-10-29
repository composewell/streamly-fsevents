1. API design issues

- No way to start from a past event ID. since is hard-coded to SinceNow, so a watch can't resume after a restart, and setFullHistory/isHistoryDone are dead API. Suggestions: a setSinceEventId setting and an accessor for the latest event ID. Resuming also needs the volume UUID, because event IDs are only valid for one volume.
- Closing isn't prompt. Only the bracketed stream API is exported. After take or a fold that stops early, the watch stays open until GC: the FSEvents stream (one of only 512), the dispatch queue and two fds. A loop of short-lived watches can hit the limit before GC runs. A withWatch, or exporting openWatch/closeWatch as Linux does, would fix this.
- One error for every failure. Pipe EMFILE, a path CoreFoundation can't convert, more than 4096 paths, and a start failure all give openWatch: failed to create watch., with no errno or reason.
- setAllEvents only toggles two flags, despite its name.
- getRoot returns the OS path for events outside every root, so it has two meanings.

2. Things that might bite users

- The 512-stream limit. Each watch uses one stream, two fds and a dispatch queue. It's better to use one watch with many roots (up to 4096).
- Slow consumers. The callback blocks once the 64 KB pipe is full, so pressure goes back into fseventsd. It will eventually drop events and send isEventsLost with UserDropped/KernelDropped, so users must rescan on that event.
- Flags accumulate. A single event can carry Created, Modified and Removed together; users should stat the path.
- SIGPIPE at its default action is fragile in any GHC program. The RTS sends SIGPIPE to interrupt interruptible foreign calls. For example, killThread on a thread inside openFile kills the process. My first SIGPIPE test was wrong for this reason, and I reworked it.
- Path matching works for the common cases I tried: /tmp → /private/tmp, a wrong-case root on case-insensitive APFS, a trailing slash, and a relative root. A symlink root is resolved once, at start. APFS rejects invalid UTF-8 names.
- setIgnoreSelf hides events from every thread of the process, not just the watcher.

3. Suspected, not verified, left unchanged

- writeEvent ignores short writes and EINTR. A partial write would permanently break the parser's record framing. It's probably unreachable, since libdispatch threads block most signals, but a write-until-done loop would be cheap insurance.
- Close can still hang in one corner. If open("/dev/null") fails at close (fd limit reached), the dup2 step is skipped and close can still hang when the read end is shared.
- A fork race can leak the fds. A fork in another thread between pipe() and fcntl() can still give a child the pipe; macOS has no pipe2. Close is safe in that case; only the fd leak into that child remains.
- setRoot goes through String on every event. That costs time on each event and would mangle non-UTF-8 names on non-APFS volumes. Matching on bytes would avoid both.
- Not exercised: TCC/sandbox behaviour (~/Documents, removable volumes), mount/unmount events, dropped-event flags, and whether the 512 limit is the same on other macOS versions.

