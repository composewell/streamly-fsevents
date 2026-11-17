# Changelog

## Unreleased

### Breaking Changes

- macOS: `watch` no longer watches subdirectories, the same as on Linux and
  Windows. Use `watchRecursive` to watch a whole directory tree.
- macOS: when a watched path is a symbolic link, `getAbsPath` returns paths
  through the symbolic link instead of through its target, the same as on
  Linux and Windows.
- Linux: changes to the metadata of a file, for example by `chmod`, `chown`
  or `touch`, are now reported, the same as on macOS.
- Linux: a file deleted from or moved out of a watched directory is no
  longer watched. Earlier, a process that still had the file open
  generated events for it.
- Linux: when a watch root is deleted, `isDeleted` is reported for the
  root, the same as on macOS and Windows. When a watch root is renamed,
  `isMoved` is reported for the root, the same as on macOS. Use
  `setRootDeleted False` and `setRootMoved False` to disable them.
- macOS: with a watch root inside another watch root, an event in the
  inner root is reported once for each root, the same as on Linux and
  Windows. Earlier it was reported for the inner root only.
- Windows: a path listed twice in the watch roots is watched once, its
  events are reported once, the same as on Linux and macOS.
- Linux: the stream ends when none of its roots is watched any more, for
  example, when all the roots are deleted or renamed, the same as on
  Windows when all the roots are deleted. Earlier the stream waited
  forever.
- Linux: when the kernel event queue overflows, the stream reports
  `isEventsLost` and ends. Earlier the stream continued, but events could be
  missing without any indication. To continue, open a new watch and scan the
  tree again.
- `getAbsPath` returns an absolute path when a watch root is supplied as a
  relative path. Earlier it returned the relative path. The root is taken
  relative to the working directory at the time the watch is started. Use
  `getRoot` and `getRelPath` to get the relative path.
- Users of the `Streamly.Internal.FS.Event.*` modules should move to the new
  public platform specific modules. In these, the Linux `setUnwatchMoved` is
  renamed to `setExcludeUnlinked`, the Linux `setFollowSymLinks` is renamed
  to `setFollowRootSymLinks`, and the Windows `getRelPath` returns
  `Maybe Path`.

### Enhancements

- FreeBSD support, using the inotify API of FreeBSD 14.5, 15.0 and later,
  in `Streamly.FileSystem.Event` and the new module
  `Streamly.FileSystem.Event.FreeBSD`. The API is the same as on Linux, see
  the module documentation for the differences.
- Recursive watching in the portable module `Streamly.FileSystem.Event` on
  all platforms: `watchRecursive`, `watchWith`, `Config` and
  `setRecursiveMode`.
- `getRoot` and `getRelPath` return the watched path an event belongs to,
  and the path of the event relative to it.
- `showEvent` shows an event, for debugging.
- New public platform specific modules `Streamly.FileSystem.Event.Darwin`,
  `Streamly.FileSystem.Event.Linux` and `Streamly.FileSystem.Event.Windows`.
- Linux: in a recursive watch, the entries of a directory created in or
  moved into the tree are reported as `isCreated` events. See
  `setScanNewDirs` and `isScanEvent`.
- Windows: a watch uses one thread for all its roots instead of one thread
  for each root, and no longer depends on the `streamly` package.
- Windows: a path that cannot be watched, for example a regular file,
  raises an exception when the stream starts.

### Bug Fixes

- Linux: recursive watching is more reliable. Directories created, moved or
  renamed in the tree are watched correctly and report the right paths. A
  subdirectory without read permission is skipped and reported by
  `isAccessDenied`, instead of ending the watch with an exception. Watching
  a large tree no longer overflows the event queue at the start.
- Linux: a recursive watch on a relative path keeps watching the new
  directories after the working directory of the process changes.
- macOS: raise an exception when a watch fails to start.
- macOS: closing a watch no longer hangs or crashes the process.
- Windows: a watched directory can be deleted or renamed while it is
  watched, and is no longer kept open after the watch ends. Deleting a
  watch root reports `isDeleted` for the root and ends the watch of the
  root.
- Windows: when events are lost because they were not consumed fast enough,
  the stream reports `isEventsLost` and continues, instead of raising an
  exception or reporting bogus events.
- The file descriptors of a watch are no longer inherited by child
  processes.

## 0.1.1

- Fix a build issue due to configure script on macOS + nix
- Allow GHC-9.14

## 0.1.0

- Initial release
