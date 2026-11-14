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
- Linux: when the kernel event queue overflows, the stream reports
  `isEventsLost` and ends. Earlier the stream continued, but events could be
  missing without any indication. To continue, open a new watch and scan the
  tree again.
- Users of the `Streamly.Internal.FS.Event.*` modules should move to the new
  public platform specific modules. In these, the Linux `setUnwatchMoved` is
  renamed to `setExcludeUnlinked`, and the Windows `getRelPath` returns
  `Maybe Path`.

### Enhancements

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

### Bug Fixes

- Linux: recursive watching is more reliable. Directories created, moved or
  renamed in the tree are watched correctly and report the right paths. A
  subdirectory without read permission is skipped and reported by
  `isAccessDenied`, instead of ending the watch with an exception. Watching
  a large tree no longer overflows the event queue at the start.
- macOS: raise an exception when a watch fails to start.
- macOS: closing a watch no longer hangs or crashes the process.
- The file descriptors of a watch are no longer inherited by child
  processes.

## 0.1.1

- Fix a build issue due to configure script on macOS + nix
- Allow GHC-9.14

## 0.1.0

- Initial release
