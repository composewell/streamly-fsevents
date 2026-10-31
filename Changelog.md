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
  default. The documentation of the earlier function was incorrect.
- Linux: enable `setAttrsModified` by default, consistent with macOS.
- Linux: in recursive mode, `isRootDeleted` and `isRootMoved` are reported
  for the watch roots only, not for subdirectories.

### Bug Fixes

- Linux: in recursive mode, fix the watching of subdirectories that are
  created, deleted, moved or renamed while the tree is watched. A directory
  moved into the tree is now watched, one moved out is no longer watched,
  and one renamed within the tree reports its new path. A subdirectory
  changing quickly after its creation no longer raises an exception or
  causes the wrong object, or no object, to be watched.
- Linux: fix the handling of an event queue overflow. The overflow event,
  `isEventsLost`, is reported once per watch root, and `getRoot` and
  `getAbsPath` no longer raise an exception on it. In recursive mode, the
  tree is scanned again, so that a directory created during the overflow is
  watched.
- Linux: close fd promptly on failure, make the fd close-on-exec.
- macOS: fix a link failure of `setFullHistory`.

### Enhancements

- Linux: in recursive mode, a newly created directory is scanned to report
  the entries created before its watch was added, as `isCreated` events.
  See `setScanNewDirs` and `isScanEvent`.
- Add the public platform specific modules
  `Streamly.FileSystem.Event.Darwin`, `Streamly.FileSystem.Event.Linux` and
  `Streamly.FileSystem.Event.Windows`.
- Add `Config`, `setRecursiveMode`, `watchWith`, `watchRecursive`, `getRoot`
  and `getRelPath` to `Streamly.FileSystem.Event`.
- Linux: `addToWatch`, `removeFromWatch` and `closeWatch` in
  `Streamly.Internal.FS.Event.Linux` no longer wait for the next event.
  Earlier they could block indefinitely on a watched tree with no activity.
  They can be called from any thread while another thread reads the events;
  `closeWatch` ends that thread's event stream.
- macOS: support non-recursive `watch`, it raised an exception earlier.
- macOS: add `setRecursiveMode`, `getRoot` and `getRelPath`.

## 0.1.1

- Fix a build issue due to configure script on macOS + nix
- Allow GHC-9.14

## 0.1.0

- Initial release
