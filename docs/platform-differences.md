# Platform Differences

`Streamly.FileSystem.Event` has the same API on Linux, macOS, Windows and
FreeBSD. The library implements a recursive watch such that Linux reports new
and moved directories in the same way as macOS and Windows do, with the
exceptions listed below. The other differences come from the event models of the operating
systems. The tables at the end list each difference as recorded by the test
suite `FileSystem.Event.Behavior`.

## Behavior tests

Each entry is the result of the test suite `FileSystem.Event.Behavior` on that
platform, except where noted below the table. The results were recorded on
Ubuntu 24.04, macOS 26 (arm64) and Windows Server 2025. _Exception_ means that the file system operation raises an exception
because the file is open.

## Directory permissions

On POSIX a read permission is needed to list a directory's entries,
and a search (execute) permission is needed to reach the paths under
it. The Windows rights that correspond to them are "List folder"
(`FILE_LIST_DIRECTORY`) and "Traverse folder" (`FILE_TRAVERSE`).  In the
following discussion we call these permissions as "READ" and "SEARCH"
for all platforms.

The CI runner on Windows is an administrator, and the test suite disables
its backup and restore privileges so that a permission removed from a
directory applies to the test process. The tests of a directory without
any permission write to a file opened before the permissions are removed,
and close the file after the write.

## Watch roots

On Linux the watch of a root is attached to its inode, on macOS to its path.
On Windows the watch keeps the root open.

* A deleted root: a root that is a symbolic link does not report
  `isDeleted` when the link is deleted.
* A root listed twice: paths
  that differ only in redundant separators and `.` components, for
  example, `x//y`, `x/./y` and `./x/y`, are the same root. On Linux and
  macOS `getRoot` reports the path with these removed, on Windows it
  reports the path as supplied.
* A directory reached via two paths that differ otherwise, for example, a
  symbolic link and its target, or paths that differ in case: all
  platforms watch it as two roots and report a change in it once for each
  root.
* Nested roots, a root and a subdirectory of it, in a recursive watch: all
  platforms report a change in the subdirectory once for each root.
* A root supplied as a relative path: on all platforms it is relative to
  the working directory at the time the watch is started, a later change
  of the working directory does not affect the watch. `getRoot` returns
  the relative path, `getAbsPath` returns an absolute path.

### Watch root behavior tests

Tested behavior common to all platforms:

* A root listed twice with the same path reports a change in it once.

**Non directory watch roots** are allowed on Linux and macOS. A watch does
not affect regular operations on the file being watched e.g. a test
shows that the watch does not call "open" on a FIFO, therefore it does
not count as a reader or writer.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A regular file can be watched | Yes | Yes | No |
| A special file (e.g. FIFO) can be watched | Yes | Yes | No |

**Watching non-existing paths:** macOS watches the path of a root, Linux
and Windows watch the object at the path and not the path itself. The
following tests are a direct consequence of that difference. macOS can
watch a missing root, therefore, it also reports the creation of the
root, which is an event that occurs even before the watch of that root
started. This is in fact an event on the the parent directory of the
missing path component.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A non-existing object path is watched | No, the watch raises an exception | Yes | No, the watch raises an exception |
| A watch root deleted and created again is watched | No | Yes | No |

**Delete of a root**: On Linux and Windows the watch of a deleted root
is removed. The watch ends with the `isRootUnwatched` event that follows
the `isDeleted` event of the root. macOS watches the path not the
object, after the `isDeleted` event the path is still watched and events
start if a new object is created on that path. Therefore, on macOS there
is no `isRootUnwatched` event generated on deletion of a root.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A deleted watch root reports `isDeleted` for the root | Yes | Yes | Yes |
| A deleted watch root reports `isDeleted` for each file in it | Yes | No | Yes |

On macOS, it has been tested that fseventsd does not report delete
events for the files inside the root when the root is deleted (e.g. with
rm -rf), whether it is a recursive or a non-recursive watch. fseventsd
possibly drops the delete events under the deleted root. When the root
itself is not deleted macOS does report the delete events for the files
deleted in it. So **if a parent is deleted, applications should remove all
paths under it automatically.**

**Rename of a root**: On both Windows and Linux the kernel watch is on
the object and not on the path, so a rename carries the watch along with
it. The library still maps the object to the old path for the kernel
events generated for the renamed object.  The old path may no longer exist
or may refer to another object.

On Linux, the library detects a rename and ends the watch with
`isRootUnwatched`.

On Windows the rename is not detected and the watch continues on the
renamed root, and the events continue reporting the old path.

On macOS the watch is on the path and not on the object, so the renamed
root is not watched, and a root renamed back to the original path is
watched again.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A renamed watch root remains watched | No | No | Yes |
| A renamed watch root reports `isMoved` | Yes | Yes | No |
| A renamed watch root reports `isRootUnwatched` | Yes | Not applicable | Not applicable |
| A root renamed back to original is watched | No | Yes | Yes |
| A root renamed back quickly (before the rename event is read) is watched | No, a file root is | Yes | Yes |
| A root whose parent is renamed remains watched under the old path | Yes | Not tested | Not tested |

**End of watch stream**: On macOS a watch stream never ends even if you
delete all the watched paths because new objects on those paths can
be created and watched.  On Linux a deleted or renamed root is gone
forever therefore the watch stream ends when all the roots are deleted
or renamed. On Windows a renamed root remains watched, but deleted root
is gone forever, if all the roots are deleted then the watch stream
ends.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| The stream ends when all the watch roots are deleted | Yes | No | Yes |

### Watch root permissions

Static permission checks: permissions are checked once before the watch
start and remain effective throughout. Any changes to permissions are
not seen until the object is unwatched and watched again.

Dynamic permission checks: permissions are checked every time an event is
delivered, changes are effective immediately.

Linux and windows require READ permission to start a watch whereas macOS
requires SEARCH permission.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A root without READ and SEARCH reports its events | Exception | No | Exception |
| A root with only SEARCH reports its events | Exception | Yes | Exception |
| A root with only READ reports its events | Yes | No | Yes |

On Windows the test of a root with only READ disables the "Bypass traverse
checking" privilege. With the privilege, which a process has by default,
SEARCH is not checked and the root reports its events.

Linux and Windows check the permissions of a watch root statically, macOS
dynamically.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A root stops reporting events after the required permission is removed | No | Yes | No |
| A root starts reporting events after the required permission is enabled | NA | Yes | NA |

## Event reporting

* Moves: Linux and Windows report the source as `isMovedFrom` and the
  destination as `isMovedTo`. macOS reports `isMoved` for both, and the two
  cannot be told apart.
* `isModified` for a directory: Linux and macOS do not report it when an entry
  is created in or deleted from the directory. Windows does not report it for
  a file created in the directory. For a file deleted from a directory in a
  recursive watch, Windows reports it in some runs.
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
  Deleting a watch root reports `isDeleted` for the root on all platforms.
  Linux and Windows also report `isDeleted` for each entry in the root. On
  macOS FSEvents reports one event, for the root only, and does not report
  the entries in the root, in recursive and non-recursive watches. On
  Windows `ReadDirectoryChangesW` does not report the deletion of the watch
  root, the library reports `isDeleted` for the root.
* A write to a file that stays open: Linux reports the write when it is
  made. macOS and Windows may not report it until the file is closed, they
  report it after the file is closed. On Windows `ReadDirectoryChangesW`
  detects a change of the size or of the last write time of a file when it
  is written to the disk, with caching this can be when the cache is
  flushed or the file is closed.
* A non-recursive watch on macOS: FSEvents can only watch a tree. The library
  starts a recursive watch and drops the events more than one level below a
  watch root, the process still receives the events of the whole tree.

### Event reporting behavior tests

Tested behavior common to all platforms:

* A file deleted and written again in a watched directory reports
  `isDeleted` and `isModified` for the file.
* A rename of a file reports `isMoved` for both the source and the
  destination.
* A copy of a file reports `isCreated` for the copy.
* Creating a hard link reports `isCreated`, and deleting one reports
  `isDeleted`.
* A non-recursive watch does not report a file created in a subdirectory.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A move reports the source and the destination as separate predicates | Yes | No | Yes |
| `isModified` is reported for a directory | No | No | Sometimes, in a recursive watch |
| A write to a file is reported before the file is closed | Yes | Usually not | No |
| A write to a file is reported after the file is closed | Yes | Yes | Yes |
| Writes to an open file after it is deleted are reported | Only if `setExcludeUnlinked` is `False` | Yes | Exception |
| Metadata changes are reported by default | Yes | Yes | No |
| Metadata change reporting can be disabled | Yes | No | Yes |
| Creating a hard link reports a metadata change on the directory | No | Yes | No |
| Two writes to a file before the consumer reads the first report one `isModified` | Yes | Usually | No |
| Interleaved writes to two files report each `isModified` | Yes | Usually not | Yes |
| One overwrite of a file reports several `isModified` | No | No | Yes |
| One event reports both `isCreated` and `isDeleted` for a file | No | Yes | No |
| One event reports both `isCreated` and `isMoved` for a file | No | Yes | No |
| A file deleted after its create event is read reports `isCreated` again | No | Usually | No |
| A non-recursive watch delivers the events of the whole tree to the process | No | Yes, the library drops them | No |

The rows for the end of the stream and for a FIFO root are from the test
suite `FileSystem.Event`. The row for the delivery of events in a
non-recursive watch has no test, it is based on the platform
documentation and the library code. The coalescing of the events of a directory
and its subdirectory into `isEventsLost` on macOS is also based on the platform
documentation.

On macOS two writes to a file are merged into one event only if FSEvents
reports them in the same batch. The macOS entries of the two rows for writes
are from several runs in the CI, the test suite does not assert them. The
same holds for the macOS entry of the row for a file deleted after its
create event is read: FSEvents can report the flags of earlier changes to a
path with a later event of the path, and the delete reported `isCreated` in
most CI runs but not in all of them.

Lost events and the recovery are described in
[robustness.md](robustness.md), the limits of each platform in
[watch-limits.md](watch-limits.md).

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

## Recursive Watches

### New and moved directories

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

### Recursive watch behavior Tests

Tested behavior common to all platforms:

* In a recursive watch, a subdirectory created in a new directory before the
  consumer reads the create event of the directory is watched.
* A file created in a directory moved into the tree, before the consumer
  reads the move event, is reported.
* A file in a directory moved out of the tree is not reported.
* Deleting a directory in a recursive watch reports `isDeleted` for each
  file in it.
* Nested roots in a recursive watch report a change in the inner root once
  for each root.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A file created in a new directory can report `isCreated` twice | Yes | Yes | No |
| A file already in a directory moved in is reported | Yes | No | No |
| A file in a directory renamed within the tree is reported | Yes | No | No |
| A file modified in a new directory, before the consumer reads the directory's create event, reports `isModified` | No | Yes | Yes |
| A file created and deleted in a new directory, before the consumer reads the directory's create event, is reported | No | Yes | Yes |
| A write to a file in a directory renamed twice before the consumer reads the first rename, the first new path taken by another directory, reports `isModified` at the path of the file | No | Yes | No |

### Subdirectory permissions

Linux:

* In a recursive watch, a subdirectory is added only if it has the read and
  the search permissions. A subdirectory without either permission is not
  watched and is reported as `isAccessDenied`, the rest of the tree stays
  watched.
* A permission removed after the watch of a directory is added does not
  affect the watch, the events of the directory are still reported. A
  permission restored later is not detected, a subdirectory reported as
  `isAccessDenied` stays unwatched until it is moved.

macOS:

* FSEvents reports the events of a directory only to a process that can
  reach the directory through the file system permissions. A process
  running as root receives all events. A subdirectory with the
  search permission even without the read permission reports its events. A
  subdirectory without any permission does not report its events.
* The access is checked for each event, not when the watch starts. A
  subdirectory whose permissions are all removed during the watch
  stops reporting its events. A subdirectory without any
  permission at the start of the watch starts reporting its events after its
  permissions are restored.

Windows:

* `ReadDirectoryChangesW` reports the changes of the whole tree through the
  handle of the root, the permissions of the subdirectories are not checked
  if the process has the "Bypass traverse checking" privilege,
  `SeChangeNotifyPrivilege`. Windows grants this privilege to all users by
  default. Without it, NTFS checks the traverse permission of the watcher
  to the subdirectory for each change in it, and does not report the change
  if the check fails.
* A process with the backup and restore privileges enabled,
  `SeBackupPrivilege` and `SeRestorePrivilege`, opens a directory
  regardless of its permissions. An administrator has these privileges.

Subdirectories:

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A recursive watch with a subdirectory without READ is watched | Yes | Yes | Yes |
| A subdirectory without READ, with SEARCH reports its events | No, it is reported as `isAccessDenied` | Yes | Yes |
| A subdirectory without READ and SEARCH reports its events | No, it is reported as `isAccessDenied` | No | Yes |
| A directory without READ moved into a recursive watch does not end the watch | Yes | Yes | Yes |
| A subdirectory reports its events after its READ is removed | Yes | Yes | Yes |
| A subdirectory reports its events after its READ and SEARCH are removed | Yes | No | Yes |
| A subdirectory without READ and SEARCH reports its events after they are restored | No | Yes | Yes |
| A subdirectory of a root with only READ reports its events | No | No | Yes |
| Without the "Bypass traverse checking" privilege, a subdirectory reports its events after its READ and SEARCH are removed | Not applicable | Not applicable | No |

### Missing, out of order events

TBD: move this section to robustness doc.

* On macOS, when a root is deleted the deletion events of its children may be
  missed.  In general, if a parent is deleted, applications should
  remove all paths under it automatically.
* Overflow can cause events to be missed.
* On macOS events can be batched, so create and delete of the same file can be
  one event and we do not know the order.

## FreeBSD vs Linux

FreeBSD 14.5, 15.0 and later releases have an inotify API compatible with
the Linux API. On FreeBSD the library uses the same implementation as on
Linux, and everything stated for Linux in this document holds for FreeBSD,
except for the following differences:

* An open file after it is deleted: reads and writes through the open file
  are never reported, `setExcludeUnlinked False` has no effect.
* Link count changes: `unlink`, `link` and a rename over an existing file
  report `isAttrsModified` only to a watch on the file itself, not to a
  watch on its directory. Linux reports `isAttrsModified` for the entry to
  the watch on the directory. `rmdir` does not report `isAttrsModified`.
* Hard links: an access to a file through any of its hard links reports an
  event in each watched directory that has a link to the file, even if the
  access used a link in an unwatched directory. A watched directory with two
  links to one file reports each event twice, once for each name.
* Adding the watch of a directory reads the directory and looks up each of
  its entries, the time taken is proportional to the number of entries. The
  read reports `isAccessed` for the directory to the watch on its parent. In
  a recursive watch, each subdirectory added generates such an event.
* A FIFO as a root: the watch is added and does not open the FIFO, but a
  write to the FIFO is not reported.
* A directory that is readable but not searchable, mode `r--`, cannot be
  watched. As a root the watch raises an exception, in a recursive watch the
  directory is reported as `isAccessDenied`. Linux watches such a directory.
* Rename cookies on FreeBSD 15.0: the kernel reports the `isMovedFrom` event
  of a directory with cookie 0 if any inotify watch watches the directory
  itself, as a recursive watch does for each subdirectory. The library gives
  such an event the cookie of the `isMovedTo` event that follows it. The
  `isMovedFrom` event of a directory moved out of the tree keeps cookie 0.
  FreeBSD 14.5 and 15.1 report the cookie of the rename.
* File systems: devfs does not report events. File systems that do not use
  the kernel name cache, for example tmpfs mounted with `nonc`, may not
  report all the events.
* Limits: `vfs.inotify.max_user_instances`, 256 by default, limits the
  inotify watches of a user, Linux allows 128. `vfs.inotify.max_user_watches`
  limits the watched directories and files of a user, as `max_user_watches`
  does on Linux.

## macOS specific features

* Clones: on APFS a file can be copied by cloning it, as `cp -c` and the
  Finder's "Duplicate" command do. The source and the clone report
  `isCloned`, and the clone also reports `isCreated`. `isCloned` is in
  `Streamly.FileSystem.Event.Darwin` only. Through the portable module, the
  event of the source matches no predicate, and the clone is reported as a
  created file, as a copy is on all platforms.
