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

- Linux: in recursive mode, a subdirectory without read or search
  permission no longer raises an exception that ends the watch. The
  subdirectory is skipped and reported by the new event `isAccessDenied`.
- Linux: in recursive mode, fix the watching of subdirectories that are
  created, deleted, moved or renamed while the tree is watched. A directory
  moved into the tree is now watched, one moved out is no longer watched,
  and one renamed within the tree reports its new path. A subdirectory
  changing quickly after its creation no longer raises an exception or
  causes the wrong object, or no object, to be watched.
- Linux: fix the handling of an event queue overflow. The overflow event,
  `isEventsLost`, is reported once per watch root, and `getRoot` and
  `getAbsPath` no longer raise an exception on it. In recursive mode,
  `setRestartOnOverflow` adds the tree again, so that a directory created
  during the overflow is watched.
- Linux: in recursive mode, adding a tree no longer reports the open, access
  and close events of its own scan. These events overflowed the event queue
  at the start of a watch on a large tree.
- Linux: close fd promptly on failure, make the fd close-on-exec.
- Linux: in recursive mode, a subdirectory without read permission is no
  longer added to the watch by its path. The path could be changed to go
  through a symbolic link to a directory outside the watched tree.
- Linux: in recursive mode with `setFollowSymLinks False`, the directory
  tree of the target of a symbolic link root is no longer watched.
- macOS: fix a link failure of `setFullHistory`.

### Enhancements

- Linux: in recursive mode, a newly created directory, a directory moved
  into the tree and a directory renamed within it are scanned to report their
  entries as `isCreated` events. A rename is reported as the tree leaving and
  coming back in. See `setScanNewDirs` and `isScanEvent`.
- Linux: a directory renamed within a recursive watch keeps its watches, the
  events generated during the rename are no longer lost and report the new
  path.
- Linux: add `setRestartOnOverflow`, enabled by default, to restart a
  recursive watch automatically after an event queue overflow.
- Add the public platform specific modules
  `Streamly.FileSystem.Event.Darwin`, `Streamly.FileSystem.Event.Linux` and
  `Streamly.FileSystem.Event.Windows`.
- Add `Config`, `setRecursiveMode`, `watchWith`, `watchRecursive`, `getRoot`
  and `getRelPath` to `Streamly.FileSystem.Event`.
- Add `showEvent` to `Streamly.FileSystem.Event` and the platform specific
  modules, for debugging. Its output format can change at any time.
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
