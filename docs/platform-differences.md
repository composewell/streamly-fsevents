# Platform Differences

`Streamly.FileSystem.Event` has the same API on Linux, macOS and Windows. The
library implements a recursive watch such that Linux reports new and moved
directories in the same way as macOS and Windows do, with the exceptions listed
below. The other differences come from the event models of the operating
systems. The tables at the end list each difference as recorded by the test
suite `FileSystem.Event.Behavior`.

## New and moved directories

On macOS and Windows the operating system implements the recursive watch. A
new subdirectory is covered by the watch in the same operation that creates
it, so no change in it is lost. This follows from the design of the APIs,
neither platform documents it as a guarantee.

On Linux inotify watches one directory, and this library implements the
recursive watch in user space. It adds the watch of a new subdirectory after
it reads the create event of the subdirectory, and then scans the
subdirectory. The scan reports the entries already in it as `isCreated`
events and sets `isScanEvent` to true; for example,
`mkdir -p a/b; touch a/b/f` reports `a`, `a/b` and `a/b/f`. Creating the
subdirectory and adding its watch are two separate steps, and the kernel
does not report changes in the subdirectory in between. The remaining
differences follow from this:

* On Linux a file created and then deleted in a newly created directory,
  before the watch of the directory is added, is not reported at all. Writes
  to a file created in this directory, before the watch is added, are not
  reported as `isModified` events. After the directory watch is added, a
  create event is reported for such a file, and all later events for it are
  reported. macOS and Windows report these events.
* Entries already in a directory moved into the tree: Linux reports each
  entry, from the scan. macOS and Windows report only the directory, so a
  portable program scans a directory moved in.
* A directory renamed within the tree: on all platforms the directory stays
  watched and no events under it are lost. macOS and Windows report the
  directory only. Linux also reports a scan event for every entry under the
  new path, after its `isMovedTo` event, because the move events do not show
  whether the directory came from inside or outside the tree. A program can
  therefore apply `isMovedFrom` as a removal of the subtree and `isMovedTo` as
  an addition, without matching the two events.
* Repeated creates: on Linux an entry created while its directory is being
  scanned is reported by the scan and by the kernel. On macOS a later event
  for a path can repeat `isCreated`. Windows does not repeat it. A consumer
  applies a create event as an update of what it knows.
* Cost: on Linux starting a watch takes time and memory proportional to the
  number of directories in the tree, and each directory uses one inotify
  watch, see [watch-limits.md](watch-limits.md). On macOS and Windows the
  size of the tree does not matter.

A directory moved out of the tree reports one event for the directory on all
platforms, the entries under it are not reported. The consumer removes the
directory and every path under it. `setScanNewDirs False` disables the scan
on Linux.

## Event reporting

* Moves: Linux and Windows report the source as `isMovedFrom` and the
  destination as `isMovedTo`. macOS reports `isMoved` for both, and the two
  cannot be told apart.
* `isModified` for a directory: Linux and macOS do not report it when an entry
  is created in or deleted from the directory. Windows does not report it for
  a file created in the directory. For a file deleted from a directory in a
  recursive watch, Windows Server 2025 reports it and Windows Server 2012 R2
  does not.
* Metadata changes: Linux reports them as `isAttrsModified`, unless
  `setAttrsModified` is `False`. macOS always reports them, as
  `isAttrsModified`, `isSecurityModified`, `isFinderInfoModified` and
  `isXAttrsModified`. Windows reports them as `isModified`, only if
  `setAttrsModified` is `True`, the default is `False`.
* An open file after it is deleted: on Linux, reads and writes through the
  open file are reported only if `setExcludeUnlinked` is `False`, the default
  is `True`. On macOS they are reported. On Windows the delete of an open file
  raises an exception.
* Hard links: creating a hard link reports `isCreated` on all platforms, and
  on macOS also `isAttrsModified` on the directory. Deleting a hard link
  reports `isDeleted` on all platforms.
* A deleted directory: deleting a directory in a recursive watch, as with
  `rm -rf dir`, reports `isDeleted` for each entry in it on all platforms.
  Deleting a watch root reports `isDeleted` for each entry in the root on
  Linux. On macOS FSEvents reports one event, for the root only, and does
  not report the entries in the root, in recursive and non-recursive
  watches. On Windows deleting a watch root raises an exception, because the
  watch keeps the root open.
* A non-recursive watch on macOS: FSEvents can only watch a tree. The library
  starts a recursive watch and drops the events more than one level below a
  watch root, the process still receives the events of the whole tree.

## Event coalescing

A program cannot count changes by counting events, on any platform.

More than one predicate can be true for one event. A portable consumer
must test every predicate it is interested in; a chain that stops at the
first true predicate misses the other changes reported by the event.

* Linux: inotify merges two identical events that are adjacent in the kernel
  queue, if the older one has not been read. Two consecutive writes to `foo`
  can report one `isModified`. In the sequence modify `foo`, modify `bar`,
  modify `foo`, all three are reported. Events of different types are never
  merged, so each event has one type, and the order of the events is the
  order of the operations.
* macOS: FSEvents can merge events of different types for a path into one
  event. A file created and deleted can report one event for which both
  `isCreated` and `isDeleted` are true, and the event does not give the order
  of the two operations. A file created and renamed reports one event for
  the old path for which both `isCreated` and `isMoved` are true. Events in
  a directory and its subdirectory can be coalesced into one event for which
  `isEventsLost` is true, without an overflow.
* Windows: the documentation does not say whether identical changes are
  merged. Each event has one type. One save of a file can report several
  `isModified` events, because the write and the truncate are separate
  changes.

An event reports what changed, not the state of the path after the
change. A consumer that needs the state, for example whether a path
reported with both `isCreated` and `isDeleted` exists, calls `stat` on the
path. The coalescing of FSEvents is described in the "Handling Events"
section of Apple's
[File System Events Programming Guide](https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html)
and in
[this Stack Overflow question](https://stackoverflow.com/questions/18415285/osx-fseventstreameventflags-not-working-correctly).

## Watch roots

On Linux the watch of a root is attached to its inode, on macOS to its path.
On Windows the watch keeps the root open.

* A renamed root: on Linux and macOS it is no longer watched. On Linux the
  library reports `isRootUnwatched`, because the kernel does not report the
  new path. On Windows renaming the root raises an exception.
* A deleted root: macOS reports `isDeleted`, unless the root is a symbolic
  link. FSEvents does not report the entries in the root. Linux reports
  `isDeleted` for each entry in the root and then `isRootDeleted`. On
  Windows deleting the root raises an exception.
* A new object at the path of a deleted or renamed root: macOS watches it.
  Linux does not; the consumer must add the path to the watch again after
  `isRootUnwatched`. A root renamed back, as in `mv d d.bak; mv d.bak d`,
  stays watched on Linux if both renames happen before the rename event is
  read, and is unwatched otherwise.
* A root that does not exist: on Linux and Windows the watch raises an
  exception. On macOS the path is watched when it is created.
* A root without read permission: on Linux the watch raises an exception, on
  Windows it is expected to, this is not tested. On macOS the events in the
  root are reported.
* A subdirectory without read or search permission in a recursive watch: on
  Linux the subdirectory is not watched and is reported as `isAccessDenied`,
  the rest of the tree stays watched. On macOS the watch continues. On
  Windows it is expected to continue, this is not tested.
* A regular file as a root: allowed on Linux and macOS, not on Windows.
* A root listed twice: Linux and macOS report a change in it once, Windows
  reports it twice.
* Nested roots, a root and a subdirectory of it, in a recursive watch: Linux
  and Windows report a change in the subdirectory once for each root. macOS
  reports it once, for the innermost root that contains the path.

## Platform specific features

### macOS

* Clones: on APFS a file can be copied by cloning it, as `cp -c` and the
  Finder's "Duplicate" command do. The source and the clone report
  `isCloned`, and the clone also reports `isCreated`. `isCloned` is in
  `Streamly.FileSystem.Event.Darwin` only. Through the portable module, the
  event of the source matches no predicate, and the clone is reported as a
  created file, as a copy is on all platforms.

## Behavior common to all platforms

The test suite `FileSystem.Event.Behavior` also checks these behaviors, which
are the same on Linux, macOS and Windows:

* A file deleted and written again in a watched directory reports
  `isDeleted` and `isModified` for the file.
* In a recursive watch, a subdirectory created in a new directory before the
  consumer reads the create event of the directory is watched.
* A file created in a directory moved into the tree, before the consumer
  reads the move event, is reported.
* A file in a directory moved out of the tree is not reported.
* A rename of a file reports `isMoved` for both the source and the
  destination.
* Deleting a directory in a recursive watch reports `isDeleted` for a file
  in it.
* A copy of a file reports `isCreated` for the copy.
* Creating a hard link reports `isCreated`, and deleting one reports
  `isDeleted`.
* A non-recursive watch does not report a file created in a subdirectory.

## Behavior tables

Each entry is the result of the test suite `FileSystem.Event.Behavior` on that
platform, except where noted below the table. The results were recorded on
Ubuntu 24.04, macOS 26 (arm64), Windows Server 2025 and Windows Server
2012 R2. _Exception_ means that the file system operation raises an exception,
because the watch keeps the root open or, in the row for an open file, because
the file is open.

### Watch roots

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A deleted watch root reports `isDeleted` | No | Yes | Exception |
| A deleted watch root reports `isDeleted` for a file in it | Yes | No | Exception |
| A watch root deleted and created again is watched | No | Yes | Exception |
| A renamed watch root is watched | No | No | Exception |
| A renamed watch root reports `isMoved` | No | Yes | Exception |
| A root renamed back before the rename event is read is watched | Yes | Yes | Exception |
| A root renamed back after the rename event is read is watched | No | Yes | Exception |
| A watch of a path that does not exist raises an exception | Yes | No | Yes |
| A path created after the start of its watch is watched | No, the watch raises an exception | Yes | No, the watch raises an exception |
| A watch of a path without read permission raises an exception | Yes | No | Not tested |
| A regular file can be a watch root | Yes | Yes | No |
| A root listed twice reports a change in it twice | No | No | Yes |
| Nested roots report a change in the inner root for each root | Yes | No | Yes |

### Recursive watches

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A file created in a new directory can report `isCreated` twice | Yes | Yes | No |
| A file already in a directory moved in is reported | Yes | No | No |
| A file in a directory renamed within the tree is reported | Yes | No | No |
| A file modified in a new directory, before the consumer reads the directory's create event, reports `isModified` | No | Yes | Yes |
| A file created and deleted in a new directory, before the consumer reads the directory's create event, is reported | No | Yes | Yes |
| A write to a file in a directory renamed twice before the consumer reads the first rename, the first new path taken by another directory, reports `isModified` at the path of the file | No | Yes | No |
| A recursive watch with a subdirectory without read permission is watched | Yes | Yes | Not tested |
| A directory without read permission moved into a recursive watch does not end the watch | Yes | Yes | Not tested |

### Event reporting

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A move reports the source and the destination as separate predicates | Yes | No | Yes |
| `isModified` is reported for a directory | No | No | Sometimes, in a recursive watch |
| Writes to an open file after it is deleted are reported | Only if `setExcludeUnlinked` is `False` | Yes | Exception |
| Metadata changes are reported by default | Yes | Yes | No |
| Metadata change reporting can be disabled | Yes | No | Yes |
| Creating a hard link reports a metadata change on the directory | No | Yes | No |
| Two writes to a file before the consumer reads the first report one `isModified` | Yes | Yes | No |
| Interleaved writes to two files report each `isModified` | Yes | No | Yes |
| One overwrite of a file reports several `isModified` | No | No | Yes |
| One event reports both `isCreated` and `isDeleted` for a file | No | Yes | No |
| One event reports both `isCreated` and `isMoved` for a file | No | Yes | No |
| A file deleted after its create event is read reports `isCreated` again | No | Yes | No |
| A non-recursive watch delivers the events of the whole tree to the process | No | Yes, the library drops them | No |

On Windows the test suite does not remove the read permission of a path, so
the three tests that need it are skipped there. The row for the delivery
of events in a non-recursive watch has no test, it is based on the platform
documentation and the library code. The coalescing of the events of a directory
and its subdirectory into `isEventsLost` on macOS is also based on the platform
documentation.

Lost events and the recovery are described in
[robustness.md](robustness.md), the limits of each platform in
[watch-limits.md](watch-limits.md).
