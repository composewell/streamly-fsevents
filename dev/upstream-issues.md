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
  into the components that `openSubdir` opens one at a time, and by
  `normalizeRoot` in `src/Streamly/Internal/FS/Event/RootPath.hs`.
* `Streamly.Internal.FileSystem.Path.normalize` (streamly-core 0.3.3) is
  `undefined`. Master (0.4.0) implements it as `normalise`, exported only by
  the Internal module. `normalizeRoot` in
  `src/Streamly/Internal/FS/Event/RootPath.hs`, used on Linux and macOS,
  removes the redundant separators and the `.` components of a root path
  with `splitPath_` and `join` instead. Replace it with `Path.normalise`
  when 0.4 is released. `normalise` keeps a leading `./`
  (`allowRelativeEquality` does not change the normalization of the root),
  `normalizeRoot` drops it, to match `eqPath` with `allowRelativeEquality`
  on Windows.

  Plan for 0.4: implement `normalizeRoot` with `Path.normalise`, and use
  it on Windows too, in `addRoot`. Today `getRoot` reports the normalized
  path on Linux and macOS, `x/y` for `./x//y`, and the path as supplied on
  Windows, the first one if a root is supplied twice in different forms.
  With the plan `getRoot` reports the normalized path on all three. On
  Windows `normalise` also upper-cases the drive letter and converts `/`
  to `\`. A leading `./` still has to be dropped after `normalise`.
  Windows can then compare the normalized paths by their bytes instead of
  with `eqPath`.
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
* On Windows, `Streamly.FileSystem.Path.fromArray` (streamly-core 0.3.1 to
  0.3.3, and master) rejects a path that has a component whose base name
  is a reserved device name: `CON`, `PRN`, `AUX`, `NUL`, `CLOCK$`,
  `COM1`-`COM9` or `LPT1`-`LPT9`. The base name is the component without
  its last extension, so `aux.c` is rejected as well as `aux`. The file
  system can hold such names, they can be created using a `\\?\` path.
  From the code, the check also applies to the part after the root of a
  `\\?\` path, which does not interpret device names; this is not tested.
  `peekFNI` in `src/Streamly/Internal/FS/Event/Windows.hs` uses
  `unsafeFromArray` for the names reported by `ReadDirectoryChangesW`.
  That use is correct for names reported by the system and stays after an
  upstream change.

  Proposed upstream: skip the reserved name check for `\\?\` paths. Not
  reported yet.
