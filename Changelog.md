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
- Linux: after an event queue overflow the watch is closed and the stream
  ends, in both modes. Earlier the stream continued, and the events after
  the overflow could have gaps that no event reported. Open a new watch and
  scan the tree to continue.

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
  `getAbsPath` no longer raise an exception on it.
- Linux: in recursive mode, adding a tree no longer reports the open, access
  and close events of its own scan. These events overflowed the event queue
  at the start of a watch on a large tree.
- Linux: close fd promptly on failure, make the fd close-on-exec.
- Linux: in recursive mode, the time taken to handle a rename, a move or a
  removal of a directory no longer grows with the number of directories
  watched. On a watch of 24000 directories, 2000 renames of a directory took
  27 seconds, during which the kernel event queue was not read.
- Linux: in recursive mode, the number of file descriptors used to add or
  scan a tree no longer grows with the depth of the tree. A deep chain of
  directories created in the tree or moved into it could exhaust the file
  descriptors of the process and end the watch with an exception.
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
- Add the public platform specific modules
  `Streamly.FileSystem.Event.Darwin`, `Streamly.FileSystem.Event.Linux` and
  `Streamly.FileSystem.Event.Windows`.
- Add `Config`, `setRecursiveMode`, `watchWith`, `watchRecursive`, `getRoot`
  and `getRelPath` to `Streamly.FileSystem.Event`.
- Add `showEvent` to `Streamly.FileSystem.Event` and the platform specific
  modules, for debugging. Its output format can change at any time.
- macOS: support non-recursive `watch`, it raised an exception earlier.
- macOS: add `setRecursiveMode`, `getRoot` and `getRelPath`.

### Internal API

These changes are to the handle API in `Streamly.Internal.FS.Event.Linux`.
Internal modules can change in any release, these changes do not affect the
public modules.

- `watchToStream`, `addToWatch` and `removeFromWatch` are renamed to
  `eventStream`, `addRoot` and `removeRoot`. The `Config` is passed only to
  `openWatch`, `eventStream` and `addRoot` do not take one. `addRoot` takes
  a `Path` instead of a `PathRep`.
- Add `readEvent` to read one event from a `Watch`.
- `eventStream` can be read again after a read is interrupted by an async
  exception, for example, a timeout, or after the stream raises an
  exception. Earlier the next read could use a directory that was already
  closed, and the events already read from the kernel could be lost.
- `addRoot`, `removeRoot` and `closeWatch` no longer wait for the next
  event. Earlier they could block indefinitely on a watched tree with no
  activity. They can be called from any thread while another thread reads
  the events; `closeWatch` ends that thread's event stream.

## 0.1.1

- Fix a build issue due to configure script on macOS + nix
- Allow GHC-9.14

## 0.1.0

- Initial release
