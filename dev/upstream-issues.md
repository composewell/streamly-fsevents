# Upstream Issues

## streamly-core

* `Streamly.Internal.Data.StreamK.concatIterateWith` (streamly-core 0.3.0)
  is not exported by a public module. It is used by `scanTree` in
  `src/Streamly/Internal/FS/Event/Linux.hs` for the depth first walk of a
  scan.
* No public API reads a directory from an open fd (streamly-core 0.3.0).
  `Streamly.Internal.FileSystem.DirIO.readEither` takes a path, the
  directory is opened again by that path. The library lists a directory
  through the fd it used to add the watch, so that a symbolic link that
  replaces the directory or a parent is not followed. It calls `fdopendir`
  itself and uses `DirStream(..)`, `readDirStreamEither` and
  `closeDirStream` from `Streamly.Internal.FileSystem.Posix.ReadDir`, which
  are not exported by a public module, in `openDirStream` and `readEntries`
  in `src/Streamly/Internal/FS/Event/Linux.hs`.

  Proposed upstream: a variant of `readEither` that takes an open directory
  fd. It can use `getdents64` on the fd directly: `fdopendir` in glibc makes
  two `fcntl` calls, and `fdopendir` and `opendir` make one `fstat` call,
  for each directory. On a walk of 28000 directories the `fcntl` calls made
  the listing through the fd slower than `opendir` by path, 0.59 s against
  0.54 s. An entry of type `DT_UNKNOWN` should be checked with `fstatat`
  relative to the fd and `AT_SYMLINK_NOFOLLOW`, `readDirStreamEither` checks
  it with `lstat` on its path. A prototype reader is on the branch
  `getdents64-reader`, it took 0.55 s on the same walk. Not reported yet.
* `Streamly.Internal.FileSystem.Path.splitPath_` (streamly-core 0.3.0) is
  not exported by a public module, the export is commented out in
  `Streamly.FileSystem.Path`. It is used by `subComponents` in
  `src/Streamly/Internal/FS/Event/Linux.hs` to split a subdirectory path
  into the components that `openSubdir` opens one at a time.
* No API opens a path without following a symbolic link in any of its
  components (streamly-core 0.3.0). `O_NOFOLLOW` applies only to the last
  component. `openSubdir` in `src/Streamly/Internal/FS/Event/Linux.hs`
  opens the root with `O_PATH` and `O_DIRECTORY`, then opens each
  component relative to the fd of its parent with `openat` and
  `O_NOFOLLOW`, closing the parent fd after each step. It raises an
  exception if a component is not a directory or is a symbolic link.

  Proposed upstream: a function in `Streamly.Internal.FileSystem.Posix`
  that opens a path below a root in this way and returns the fd, and the
  `O_PATH` and `O_DIRECTORY` flags in `Streamly.Internal.Syscall.Posix.File`,
  which has `openAt` and `setNoFollow` on master but not those two flags.
  On Linux 5.6 and later, `openat2` with `RESOLVE_NO_SYMLINKS` does the
  same in one system call. Not reported yet.
