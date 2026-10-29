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
- macOS: fix a link failure of `setFullHistory`. `setFullHistory`,
  `isCloned`, `isHardLink` and `isLastHardLink` are always exported; when
  the SDK lacks the flag, the setter is a no-op and the predicates return
  `False`.

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
