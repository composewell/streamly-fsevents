# Changelog

## Unreleased

### Breaking Changes

- macOS: `watch` and `watchWith` are now non-recursive by default,
  consistent with Linux and Windows. Use `watchRecursive` or
  `setRecursiveMode True` for recursive watching.
- macOS: when a watch root is a symbolic link, `getAbsPath` returns the path
  via the symbolic link instead of the resolved target, consistent with
  Linux and Windows.
- Windows: `getRelPath` returns `Maybe Path`, consistent with Linux.
- Linux: rename `setUnwatchMoved` to `setExcludeUnlinked` and enable it by
  default. The old name is a deprecated alias in the internal module. The
  setting stops the events of a file after it is unlinked; contrary to the
  earlier documentation, it has no effect on objects moved out of a watched
  directory.
- Linux: enable `setAttrsModified` by default, consistent with macOS. A
  `touch` or `chmod` on an existing file reports `isAttrsModified`.
- Linux: in recursive mode, `isRootDeleted` and `isRootMoved` are reported
  for the watch roots only, not for subdirectories. The parent of a
  subdirectory reports `isDeleted` or `isMovedFrom` for it.

### Bug Fixes

- Linux: in recursive mode, a directory moved out of the tree is no longer
  watched, a directory moved into the tree is watched, and a directory
  renamed within the tree reports events with its new path.
- Linux: in recursive mode, fix an exception when a subdirectory is deleted
  or moved before its create event is read.
- Linux: fix an exception raised by the event stream or by `getRoot` for
  events arriving after `removeFromWatch`.
- Linux: report the queue overflow event (`isEventsLost`) once per watch
  root; `getRoot` and `getAbsPath` raised an exception on it earlier.
- Linux: fix `setWhenExists FailIfExists`, adding a watch failed with it.
- Linux: close the inotify fd when starting a watch fails, e.g. on a
  missing path; the fd leaked until garbage collection.
- Linux: create the inotify fd close-on-exec; a child process kept the
  watch alive after it was closed.
- Linux: in recursive mode with `FailIfExists`, fix an `EEXIST` exception
  when a subdirectory is created while its parent is being scanned. The
  setting now applies to the watch roots only.
- Linux: in recursive mode, do not watch a file, or follow a symbolic link,
  that replaced a new directory before its create event was read.
- Linux: in recursive mode, do not skip the remaining subdirectories when an
  entry is deleted during a scan, on file systems that do not report the
  entry type.
- Linux: in recursive mode, scan the tree again after a queue overflow; a
  directory created during the overflow was never watched.
- Linux: after a queue overflow, report `isRootUnwatched` for the roots
  removed with `removeFromWatch`; the event was lost, and a removed root
  sharing a directory with another root kept reporting its events.
- Linux: a failed `removeFromWatch` of a root sharing a directory with
  another root can be retried; the root kept reporting events and the retry
  did nothing.
- Linux, internal module: `addToWatch`, `removeFromWatch` and `closeWatch`
  no longer block while another thread waits for events. `closeWatch` ends
  the event stream; with the non-threaded RTS it ends at the next event.
  `addToWatch` on a closed watch raises an exception, `removeFromWatch`
  does nothing.
- macOS: fix a link failure of `setFullHistory`. `setFullHistory`,
  `isCloned`, `isHardLink` and `isLastHardLink` are always exported; when
  the SDK lacks the flag, the setter is a no-op and the predicates return
  `False`.
- macOS: raise an exception when the event stream of a watch fails to
  start, e.g. beyond 512 watches in a process; the watch reported no
  events.
- macOS: closing a watch no longer terminates the process with SIGPIPE
  when SIGPIPE is not ignored.
- macOS: closing a watch no longer hangs when a child process holds the
  event pipe, and no longer frees the watch while its callback is running.
- macOS: create the event pipe close-on-exec; child processes inherited it.
- macOS: `setFullHistory` has no effect, a watch always starts from the
  current time; the documentation said it reports all the past events.

### Enhancements

- Add the public platform specific modules
  `Streamly.FileSystem.Event.Darwin`, `Streamly.FileSystem.Event.Linux` and
  `Streamly.FileSystem.Event.Windows`.
- Add `Config`, `setRecursiveMode`, `watchWith`, `watchRecursive`, `getRoot`
  and `getRelPath` to `Streamly.FileSystem.Event`.
- macOS: support non-recursive `watch`, it raised an exception earlier.
- macOS: add `setRecursiveMode`, `getRoot` and `getRelPath`.
- Linux: `FailIfExists` is always available.
- Linux: export the `Cookie` type.

## 0.1.1

- Fix a build issue due to configure script on macOS + nix
- Allow GHC-9.14

## 0.1.0

- Initial release
