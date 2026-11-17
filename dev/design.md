# Design common to all platforms

The designs specific to a platform are in `design.<platform>.md`.

## Watch roots are identified by their normalized paths

A root is identified by the path supplied by the user, normalized:
redundant separators and `.` components are removed, `x//y`, `x/./y` and
`./x/y` become `x/y`. `..` components and a trailing separator are kept.
Symbolic links are not resolved, and case is significant.

* `addRoot` does not add a root whose normalized path is already a root,
  and `openWatch` adds a path listed twice once.
* `removeRoot` normalizes its argument and removes the root with that
  normalized path. Two roots never have the same normalized path, so it
  removes at most one root.
* `getRoot` reports the normalized path on Linux and macOS. On Windows it
  reports the path as supplied, the first one if a root is supplied twice
  in different forms, until streamly-core 0.4, see
  `dev/upstream-issues.md`.

The normalization is in `normalizeRoot` in
`src/Streamly/Internal/FS/Event/RootPath.hs` on Linux and macOS. Windows
compares the paths with `Path.eqPath` and `allowRelativeEquality`, which
ignores the same differences.

### Normalization, not canonicalization

Two paths that differ only in redundant separators and `.` components reach
the directory through the same components, a second root for such a path
watches the same thing again. Normalizing the path only cleans up how it is
written.

A canonical path resolves symbolic links and `..` components, it changes
the path through which the directory is reached. The library does not
canonicalize, and does not compare the file identity (device and inode,
volume serial number and file index):

* The user adds and removes a root by its path, the path is the key of a
  root. Two different paths are two roots for the user, even if they reach
  the same directory. A path the user supplied is watched and reported
  through that path, it is not dropped because another root reaches the
  same directory.
* Under the hood both roots may use the same watch, on Linux they share
  the inotify watch descriptor of the directory. The library still reports
  the events once for each root and removes the roots independently.
* Canonicalization does not find every case: a Linux bind mount, a
  directory hard link on HFS+ or a Windows volume mounted at two folders
  gives a directory two canonical paths, and `realpath` on Linux does not
  fold the case of a name in a case-insensitive directory. It also needs
  the path from the open handle or fd to avoid a race with a rename, on
  each platform.

Consequences: a directory reached via a symbolic link and via its target,
via paths that differ in case, or via a bind mount, is two roots, its
events are reported once for each root. Removing the duplicates, if
needed, is up to the user, for example by comparing the canonical paths of
the roots before adding them.
