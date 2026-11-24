# Platform Differences

`Streamly.FileSystem.Event` has the same API on Linux, macOS,
Windows and FreeBSD. The library implements a recursive watch such
that Linux reports new and moved directories in the same way as
macOS and Windows do, with the exceptions listed below. The other
differences come from the event models of the operating systems. The
tables at the end list each difference as recorded by the test suite
`FileSystem.Event.Behavior`.

Related documents: Lost events and the recovery are described in
[robust-applications.md](robust-applications.md), the limits of each platform in
[watch-limits.md](watch-limits.md).

<!--
This document is kept in sync with the test suite FileSystem.Event.Behavior
(test/Streamly/Test/FileSystem/Event/Behavior.hs):

* The Section and Subsection entries of the test suite are the headings of
  this document.
* The title of a behavior is the text of its table row. In a table of
  operations and events, the title is the operation and the event, e.g.
  "Copying a file reports `isCreated` for the copy".
* A row tested in another test suite has a placeholder comment under its
  heading in the test suite.

When a row is added, changed or removed here, change the test suite too, and
the converse.
-->

## Architectural Differences

Architectural differences across operating systems make it difficult to create
a library which works the same way across all of them.

### Watch on path vs object

On Linux and Windows the watch of a root is attached to its inode, on
macOS to its path. Therefore, on Linux and Windows, natively, the watch
follows the object, if the object is moved to another path the watch
stays. This is a fundamental difference in implementations.  Since macOS
watches the path, and whatever comes on that path, it allows watching
non-existing paths, whatever and whenever something lives on that path
it is watched.

Watch following the object is usually not desired, therefore, this
library breaks the watch if the object moves to an unwatched location.

To watch non-existing paths like macOS would be a significantly complex
to implement in the library, therefore in the portable implementation we
try to make macOS behave like Linux rather than the opposite.

On all platforms a root is bound to the object that its path refers to
when the root is added. When the path no longer refers to that object,
because the root or a parent directory is renamed, the root is deleted, or
a symbolic link in the path is changed, the watch of the root ends with
`isRootUnwatched` and `isRootStale`. The stale root check,
`enableStaleRootCheck`, on by default, checks the root path when the events
of a root are read. A deleted root, and on Linux and macOS a renamed root,
ends at its deletion or rename event. A root whose path changes without
any event of the root, e.g. after a parent rename on Linux or a symbolic
link change, ends at its next event. With the check off the root reports
the events of the watched object under the old path. On macOS `allowMissingRootPath True` gives the
behavior of FSEvents instead: the path is watched, whatever object is at
the path.

### Permissions Checks

On macOS the permission checking is dynamic i.e. if the permissions of a path
change the events can stop or start under that path. On Windows the permission
checks happen only at the start of watch. In our Linux implementation as well
the permission checks are static, though it is possible to implement dynamic
checks it would make the implementation more complex.

### Recursive watches

Linux provides primitives to build recursive watches which leaves gaps
between creation of a directory and adding a watch requiring us to
scan it for pre-existing entries. There are some difficult (though
rare) rename edge cases which are complicated to fix. macOS and Windows
provide a recursive watch natively.

### Event mapping

Which actions collapse into the same event.  On macOS and Linux report
file modification and attribute changes as separate events. On Windows
the same events reports everything and what is included in it is
controlled by flags passed to the kernel.

### Event coalescing

macOS can combine completely different type of events together in the
same message making it impossible to determine the ordering e.g. an
event may have a create and delete and we do not know whether it was
create after delete or vice-versa.

## Behavior tests

Each assertion in this document is tested by the test suite
`FileSystem.Event.Behavior`, except where noted explicitly.
The results were recorded on Ubuntu 24.04, macOS 26 (arm64),
Windows Server 2025, and FreeBSD 14.5 and 15.0.

The temporary directories of the tests were on ext4 on Linux, APFS on
macOS, NTFS on Windows and UFS on FreeBSD. Some results depend on the
file system, for example the permission checks on Windows are those of
NTFS. On a network or FUSE file system a change made by another machine
may not generate any event.

_Operation fails_ means that the file system operation of the test, for
example a delete or a rename, raises an exception, so there is no event to
report. _Watch fails_ means that the watch raises an exception.

## Directory permissions

On POSIX a read permission is needed to list a directory's entries,
and a search (execute) permission is needed to reach the paths under
it. The Windows rights that correspond to them are "List folder"
(`FILE_LIST_DIRECTORY`) and "Traverse folder" (`FILE_TRAVERSE`).  In
this document we call these permissions as "READ" and "SEARCH" for all
platforms.

<!--
The CI runner on Windows is an administrator, and the test suite disables
its backup and restore privileges so that a permission removed from a
directory applies to the test process. The tests of a directory without
any permission write to a file opened before the permissions are removed,
and close the file after the write.
-->

## Watch roots

### Non directory watch roots

Non directory watch roots are allowed on Linux and macOS. A watch does
not affect regular operations on the file being watched e.g. a test
shows that the watch does not call "open" on a FIFO, therefore it does
not count as a reader or writer.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A regular file can be watched | Yes | Yes | No |
| A special file (e.g. FIFO) can be watched | Yes | Yes | No |
| A symbolic link itself, not its target, can be watched | Yes, with `setFollowRootSymLinks False` | No | No |

On FreeBSD a FIFO root is watched, but a write to the FIFO is not reported.

### Watching non-existing paths

A root that does not exist raises an exception on all platforms, a
deleted root is not watched again.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A non-existing object path is watched | No, the watch raises an exception | No, the watch raises an exception | No, the watch raises an exception |
| A watch root deleted and created again is watched | No | No | No |

macOS watches the path of a root, Linux and Windows watch the object at
the path and not the path itself. With `allowMissingRootPath True` from
`Streamly.FileSystem.Event.Darwin` a missing root is watched. It also
reports the creation of the root, which is an event that occurs even
before the watch of that root started. This is in fact an event on the
the parent directory of the missing path component.

| Behavior | macOS |
|----------|-------|
| A non-existing object path is watched with `allowMissingRootPath True` | Yes |
| A watch root deleted and created again is watched with `allowMissingRootPath True` | Yes |

### Deletion of watch root

On all platforms the watch of a deleted root is removed. The watch ends
with the `isRootUnwatched` and `isRootStale` event that follows the
`isDeleted` event of the root. With `allowMissingRootPath True` macOS
watches the path not the object, after the `isDeleted` event the path is
still watched and events start if a new object is created on that path,
there is no `isRootUnwatched` event.

File roots, Windows does not support file roots:

| Behavior | Linux | macOS |
|----------|-------|-------|
| A deleted regular file root reports `isDeleted` for the root | Yes | Yes |
| A deleted regular file root with another hard link reports `isDeleted` for the root | No | Yes |
| A deleted FIFO root reports `isDeleted` for the root | Yes | Yes |

On Linux, a symlink deletion is reported if the symlink itself is watched and
not the target.

Directory roots:

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A deleted directory root reports `isDeleted` for the root | Yes | Yes | Yes |
| A deleted watch root reports `isRootUnwatched` for the root | Yes | Yes | Yes |
| A deleted watch root reports `isRootStale` for the root | Yes | Yes | Yes |
| A deleted directory root reports `isDeleted` for each file in it | No | No | Yes |
| A deleted directory root reports `isDeleted` for each file in it without the stale root check | Yes | No | Yes |
| A deleted symbolic link to a directory root reports `isDeleted` for the root | No | No | No |

| Behavior | macOS |
|----------|-------|
| A deleted watch root reports `isRootUnwatched` for the root with `allowMissingRootPath True` | No |

On Linux the stale root check drops the deletions of the files that are
read after the root is deleted, the test reads them after the deletion.
On Windows the read of the changes of the root completes before the root
is deleted.

Linux reports the deletion of a root when its inode is freed, after the
last link to it is removed and the last open reference to it is closed.
An open file, or a directory that is the working directory of a process,
reports `isDeleted` when it is closed. FreeBSD reports the deletion when
the last link to the root is removed, even if the root is still open. On
Windows a directory in use by
a process cannot be deleted. On Windows `ReadDirectoryChangesW` does not
report the deletion of the watch root, the library reports `isDeleted`
for the root.

On macOS, it has been tested that fseventsd does not report delete
events for the files inside the root when the root is deleted (e.g. with
rm -rf), whether it is a recursive or a non-recursive watch. fseventsd
possibly drops the delete events under the deleted root. When the root
itself is not deleted macOS does report the delete events for the files
deleted in it. So **if a parent is deleted, applications should remove all
paths under it automatically.**

### Rename of a root

On both Windows and Linux the kernel watch is on the object and not on
the path, so a rename carries the watch along with it. The library still
maps the object to the old path for the kernel events generated for the
renamed object. The old path may no longer exist or may refer to another
object.

On Linux and macOS, the library detects a rename and ends the watch with
`isRootUnwatched` and `isRootStale`.

On Windows the rename does not generate any event. The stale root check
ends the watch of the root at its next change. Without the check the
watch continues on the renamed root, and the events continue reporting the
old path.

File roots, Windows does not support file roots:

| Behavior | Linux | macOS |
|----------|-------|-------|
| A renamed regular file root reports `isMoved` for the root | Yes | Yes |
| A renamed regular file root with another hard link reports `isMoved` for the root | Yes | Yes |
| A renamed FIFO root reports `isMoved` for the root | Yes | Yes |

On Linux, a file root renamed back quickly (before the rename event is
read) is watched, otherwise the watch is removed.

On Linux, a symlink move is reported if the symlink itself is watched and
not the target.

Directory roots:

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A renamed watch root remains watched | No | No | No |
| A renamed watch root reports `isRootStale` at its next event | Yes | Yes | Yes |
| A renamed watch root remains watched without the stale root check | No | No | Yes |
| A renamed watch root reports `isMoved` | Yes | Yes | No |
| A renamed watch root reports `isRootUnwatched` | Yes | Yes | No |
| A root renamed back to its original path is watched | No | No | Yes |
| A renamed directory root reports `isMoved` for each file in it | No | No | No |
| A renamed symbolic link to a directory root reports `isMoved` for the root | No | No | No |

<!--
| A root renamed back before the rename event is read is watched | No | Yes | Yes |
-->

### Renaming or deleting in-use objects

Windows processes open the directory without FILE_SHARE_DELETE, therefore, a
directory in-use by a process cannot be deleted or renamed.

macOS sends a delete event as soon as the path is unlinked from the
inode i.e. when the path is deleted, Linux sends a delete when all
references to the inode are gone i.e. when the inode is deleted.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A deleted open regular file root reports `isDeleted` for the root | No | Yes | Not Applicable |
| A deleted directory root in use by a process reports `isDeleted` for the root | No | Yes | Delete fails |

Linux watch is on the object, it reports the deletion when the deleted
but still in-use object is closed or the process ends:

| Behavior | Linux |
|----------|-------|
| A deleted open regular file root reports `isDeleted` for the root after the file is closed | Yes |
| A deleted directory root in use by a process reports `isDeleted` for the root after the process ends | Yes |

On FreeBSD a deleted open file root, or a deleted directory root that is
the working directory of a process, reports `isDeleted` immediately, as
on macOS.

Renaming has same behavior on Linux and macOS:

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A renamed open regular file root reports `isMoved` for the root | Yes | Yes | Not Applicable |
| A renamed directory root in use by a process reports `isMoved` for the root | Yes | Yes | Rename fails |

### Rename of a parent of a root

On Linux the rename of a parent directory of a root does not generate
any event for the root. The stale root check ends the watch of the root
with `isRootUnwatched` and `isRootStale` at its next event. Without the
check the root stays watched, and its events report the old path, until
the library finds that the root path no longer refers to the root, for
example, when it adds a directory created in a recursive root.

On macOS the events of the root at the new path of the parent are not
reported, the root ends only when `staleRoots` or a change of the roots of
the watch finds the change.

On Windows, the watch keeps an open handle to the root, and Windows does
not rename a directory while a file or a directory under it is open. The
root itself can be renamed because the library opens the root with the
share mode `FILE_SHARE_DELETE`, which allows other processes to delete
or rename the file of the handle. The share mode applies to the file of
the handle only, not to its parent directories.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A root whose parent is renamed remains watched under the old path | No | No | Operation fails |
| A root whose parent is renamed reports `isRootStale` at its next event | Yes | No | Operation fails |
| A root whose parent is renamed remains watched under the old path without the stale root check | Yes | No | Operation fails |
| A recursive root whose parent is renamed is unwatched on a directory create | Yes | No | Operation fails |

### Resolution of the root path

These tests change the resolution of a root path after the watch
starts, by creating a missing path component or by changing the target
of a symbolic link root.

A missing root raises an exception on all platforms. On macOS with
`allowMissingRootPath True`, FSEvents reports events only under the path
resolved when the root is added. A missing root, or a missing parent of a
root, created later as a symbolic link to a directory, does not report
any events in that directory. On a case-insensitive file system a missing
root `Missing` created later as `missing` does not report any events in it
either. With `setRootPathEvents True` from
`Streamly.FileSystem.Event.Darwin`, each of these three changes reports
`isRootPathEvent`.

On all platforms a symbolic link root, or a root under a symbolic link
parent, stays watched on the directory the link pointed to when the
watch started, after the link is changed to point to another directory.
No event reports the change, on macOS it does not report
`isRootPathEvent`. The stale root check ends the watch of the root at its
next event, without the check the events of the old directory are
reported under the root path.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A missing root created as a symbolic link to a directory reports its events | Watch fails | Watch fails | Watch fails |
| A root whose missing parent is created as a symbolic link to a directory reports its events | Watch fails | Watch fails | Watch fails |
| A missing root created with a different case reports its events | Watch fails | Watch fails | Watch fails |
| A symbolic link root changed to another directory reports the events of the new directory | No | No | No |
| A symbolic link root changed to another directory reports the events of the old directory | No | No | No |
| A symbolic link root changed to another directory reports the events of the old directory without the stale root check | Yes | Yes | Yes |
| A root whose symbolic link parent is changed to another directory reports the events of the new directory | No | No | No |
| A root whose symbolic link parent is changed to another directory reports the events of the old directory | No | No | No |
| A root whose symbolic link parent is changed to another directory reports the events of the old directory without the stale root check | Yes | Yes | Yes |
| A root whose symbolic link parent is changed to another directory reports `isRootStale` at its next event | Yes | Yes | Yes |

| Behavior | macOS |
|----------|-------|
| A missing root created as a symbolic link to a directory reports its events with `allowMissingRootPath True` | No |
| A root whose missing parent is created as a symbolic link to a directory reports its events with `allowMissingRootPath True` | No |
| A missing root created with a different case reports its events with `allowMissingRootPath True` | No |

On macOS `/Users` is a firmlink to `/System/Volumes/Data/Users`. A root
under `/Users` reports its events with the root path as supplied.

### Unmount of the file system of a root

On Linux a watch does not keep a file of the watched file system open,
the file system can be unmounted while it is watched. The kernel removes
the watches of the file system, the library reports `isRootUnmounted`
and `isRootUnwatched` for the root.

On macOS the watch is on the path, it does not keep a file of the watched
file system open.

On Windows the watch keeps an open handle to the root. A dismount that
first locks the volume fails while a handle on the volume is open. "Safely
Remove Hardware", ejecting a drive and detaching a virtual disk lock the
volume. A forced dismount, for example with `fsutil volume dismount`,
does not lock the volume, the volume is dismounted and the open handles
on it become invalid.

<!--
The tests mount a new file system at the watch root: a tmpfs on Linux and
FreeBSD, mounted as root or with `sudo -n`, a disk image attached with
`hdiutil` on macOS, and a virtual disk created with `diskpart` on
Windows, which needs an administrator. A test is skipped if the file
system cannot be mounted, or on Windows if the volume cannot be locked
before the watch is started. On Windows the unmount test locks the volume
and then dismounts it, the event tests dismount the volume forcibly,
without the lock.
-->

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A file system can be unmounted while a tree in it is watched | Yes | Yes | No, a dismount that locks the volume raises an exception |
| Unmount of a watched file system reports `isRootUnwatched` for the root | Yes | Yes | No |
| Unmount of a watched file system reports an unmount event | Yes | Yes | No |

On FreeBSD a file system cannot be unmounted while a tree in it is
watched, `umount` fails with `Device busy`.

### End of watch stream

The stream of `watchWith` ends after the `isRootUnwatched` event of the
last root, when all the roots are deleted or renamed. On Windows a renamed
root ends at its next change. On macOS with `allowMissingRootPath True`
the paths stay watched and the stream does not end. A stream read from a
watch opened with `openWatch` ends only when the watch is closed.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| The stream ends when all the watch roots are deleted | Yes | Yes | Yes |

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
| A root without READ and without SEARCH reports its events | Watch fails | No | Watch fails |
| A root with only SEARCH reports its events | Watch fails | Yes | Watch fails |
| A root with only READ reports its events | Yes | No | Yes |

On FreeBSD a root with only READ cannot be watched, the watch raises an
exception.

On Windows the test of a root with only READ disables the "Bypass traverse
checking" privilege. With the privilege, which a process has by default,
SEARCH is not checked and the root reports its events.

Linux and Windows check the permissions of a watch root statically, macOS
dynamically.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A root stops reporting events after the required permission is removed | No | Yes | No |
| A root starts reporting events after the required permission is enabled | NA | Yes | NA |
| A root stops reporting events after SEARCH is removed from its parent | No | Yes | No |
| A root starts reporting events after SEARCH is restored on its parent | NA | Watch fails | NA |

On macOS a root whose parent has no SEARCH permission cannot be resolved,
the watch raises an exception. With `allowMissingRootPath True` it is
watched:

| Behavior | macOS |
|----------|-------|
| A root starts reporting events after SEARCH is restored on its parent with `allowMissingRootPath True` | Yes |

On Windows the tests of the parent also disable the "Bypass traverse
checking" privilege.

### Nested roots

Nested roots in a recursive watch report a change in the inner root once
for each root, on all platforms.

<!--
Tested, without a claim above:

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A root listed twice reports a file created in it once | Yes | Yes | Yes |
| A root and a symbolic link to it report a file created in it for both | Yes | Yes | Yes |
| A root and its path in upper case report a file created in it for both | Not applicable, case sensitive | Yes | Yes |
-->

## Event reporting (Non recursive)

### Root Directory metadata changes

Events for the watch root directory metadata, not for objects contained in it.

| Operation | Event | Linux | macOS |
|-----------|-------|-------|-------|
| Change the permissions of the directory | `isAttrsModified` for the directory | Yes | No |
| Set the access and modification times of the directory | `isAttrsModified` for the directory | Yes | Yes |

macOS reports a change of the permissions of the directory as
`isSecurityModified` for the directory.

On Windows these changes are reported under `isModified`:

| Operation | Event | Config | Status |
|-----------|-------|--------|--------|
| Set the hidden attribute of the directory | `isModified` for the directory | `setAllEvents True` | No |
| Set the access and modification times of the directory | `isModified` for the directory | `setAllEvents True` | No |

### Root Directory content changes

Events for the watch root directory data, due to directory operations e.g.
creation and deletion of objects in it.

File create/delete events due to directory operations:

| Operation | Event | Linux | macOS | Windows |
|-----------|-------|-------|-------|---------|
| Create a file | `isCreated` for the new file | Yes | Yes | Yes |
| Delete a file | `isDeleted` for the file | Yes | Yes | Yes |
| Create a hard link | `isCreated` for the file | Yes | Yes | Yes |
| Delete a hard link | `isDeleted` for the file | Yes | Yes | Yes |
| Copy a file | `isCreated` for the copy | Yes | Yes | Yes |

On APFS a file can be copied by cloning it, as `cp -c` and the Finder's
"Duplicate" command do. The source and the clone report `isCloned`, and
the clone also reports `isCreated`. `isCloned` is in
`Streamly.FileSystem.Event.Darwin` only, the portable module does not have
a predicate for the clone flag of the source.

File move events due to directory operations:

| Operation | Event | Linux | macOS | Windows |
|-----------|-------|-------|-------|---------|
| Rename a file | `isMovedFrom` for the source, `isMovedTo` for the destination | Yes | No | Yes |
| Rename a file | `isMoved` for the source and the destination | Yes | Yes | Yes |

Directory metadata events due to directory operations on Linux/macOS:

| Operation | Event | Linux | macOS |
|-----------|-------|-------|-------|
| Create a file | `isAttrsModified` for the directory | No | No |
| Delete a file | `isAttrsModified` for the directory | No | No |
| Rename a file | `isAttrsModified` for the directory | No | No |
| Create a subdirectory | `isAttrsModified` for the directory | No | No |
| Delete a subdirectory | `isAttrsModified` for the directory | No | No |
| Create a symbolic link | `isAttrsModified` for the directory | No | No |
| Create a hard link | `isAttrsModified` for the directory | No | Yes |
| Delete a hard link | `isAttrsModified` for the directory | No | No |

Directory metadata events due to directory operations on Windows are
reported under `isModified` enabled by appropriate config options:

| Operation | Event | Config | Status |
|-----------|-------|--------|--------|
| Create a file | `isModified` for the directory | `setAllEvents True` | No |
| Delete a file | `isModified` for the directory | `setAllEvents True` | No |
| Rename a file | `isModified` for the directory | `setAllEvents True` | No |
| Create a subdirectory | `isModified` for the directory | `setAllEvents True` | No |
| Delete a subdirectory | `isModified` for the directory | `setAllEvents True` | No |
| Create a hard link | `isModified` for the directory | `setAllEvents True` | No |
| Delete a hard link | `isModified` for the directory | `setAllEvents True` | No |

<!--
Tested, without a claim above:

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A file deleted and created again is reported | Yes | Yes | Yes |
-->

### File/subdirectory metadata changes

Events for the metadata modification of objects contained in the watch
root directory.

All the events reported for a file or subdirectory by each platform; the
event is reported for that item:

| Operation | Linux | macOS | Windows |
|-----------|-------|-------|---------|
| Set the modification time only | `isModified` | `isAttrsModified` | `isModified` |
| Set the access and modification times | `isAttrsModified` | `isAttrsModified` | `isModified` |
| Change the permissions of an item | `isAttrsModified` | `isSecurityModified` | `isModified` |

<!--
On Linux setting only modification time is reported as `isModified`
but if access time is set in the same system call it is reported as
`isAttrsModified`.
On Windows a write is same as modification time
update.
-->

On FreeBSD setting the modification time only reports `isAttrsModified`,
not `isModified`.

macOS reports additional metadata changes as `isFinderInfoModified` and
`isXAttrsModified`.

Windows reports file content modification as well as metadata changes as
`isModified` event. Which actions trigger this event is controlled by
config options.

| Operation | Reported by config |
|-----------|-------------|
| Make an item read-only | `setAttrsModified` |
| Set the hidden attribute of an item | `setAttrsModified` |
| Set the access time of an item | `setLastAccessTimeModified` |
| Set the modification time of an item | `setLastWriteTimeModified` |
| Set the creation time of an item | `setCreationTimeModified` |
| Truncate a file to zero size, without a write | `setSizeModified`, `setLastWriteTimeModified`, `setLastAccessTimeModified` |

Creating or deleting a hardlink does not generate any metadata change
event for the file on any platform.

### File content changes

Events for the content modification of non-directory objects contained
in the watch root directory.

All platforms except macOS report an event as soon as data is flushed to
disk after a write, macOS reports when the file handle is closed after a
write.  All the events reported for the file by each platform:

| Operation | Linux | macOS | Windows |
|-----------|-------|-------|---------|
| Write to an open file, before it is closed | `isModified` | Usually none | `isModified` |
| Write to an open file, after it is closed | `isModified` | `isModified` | `isModified` |
| Truncate a file to zero size, without a write | `isModified` | `isModified`, `isAttrsModified` | `isModified` |

On Windows `isModified` indicates a file content modification if only
`setSizeModified` and `setLastWriteTimeModified` are enabled. With other
configs enabled it includes other attribute changes as well.

On FreeBSD an access to a file through any of its hard links reports an
event in each watched directory that has a link to the file, even if the
access used a link in an unwatched directory. A watched directory with two
links to one file reports each event twice, once for each name.

### Subdirectory content changes

In non-recursive mode, creation, deletion, modification of files inside a
subdirectory do not generate any events, neither for the file nor for the
subdirectory, though the modification time of the subdirectory changes.
On Windows the modification or deletion of a file in a subdirectory
reports `isModified` for the subdirectory in some runs. The modification
of a file two levels below the root also reports `isModified` for the
subdirectory in some runs.

| Operation | Event | Linux | macOS | Windows |
|-----------|-------|-------|-------|---------|
| Create a file in a subdirectory | Any | No | No | No |
| Modify a file in a subdirectory | Any | No | No | Sometimes |
| Delete a file in a subdirectory | Any | No | No | Sometimes |
| Modify a file two levels below the root | Any | No | No | Sometimes |
| Delete a file two levels below the root | Any | No | No | No |

<!--
Tested, without a claim above. It checks whether the Windows events in the
table are late events of the setup, which creates the file:

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A file created in a subdirectory before the watch reports an event | No | No | Sometimes |
-->

<!--
On macOS FSEvents can only watch a tree. The library
starts a recursive watch and drops the events more than one level below
a watch root, the process still receives the events of the whole tree.
-->

### Writes to a deleted open file

On Linux, reads and writes through an open file after it is deleted
are reported only if `setExcludeUnlinked` is `False`, the default
is `True`. On macOS a write through an open file after it is deleted is
reported as `isModified`, in the same event as the `isDeleted` of the file.
FSEvents coalesces the two, the event does not give their order. On Windows
and FreeBSD an open file is not watched after deletion.

<!--
The macOS outcome was checked with a CI run with a 30 second delay after
the setup of the test: the event had the flags Deleted and Modified, without
the Created and AttrsModified flags of the setup.
-->

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| Writing to an open file after its deletion reports `isModified` | No | Yes | No |
| Writing to an open file after its deletion reports `isModified` with `setExcludeUnlinked False` | Yes | Not applicable | Not applicable |

### Repeated events

| Operation | Event | Linux | macOS | Windows |
|-----------|-------|-------|-------|---------|
| Overwrite a file once | More than one `isModified` for the file | No | Sometimes | Yes |
| Delete a file after the consumer reads its create | `isCreated` again for the file | No | Usually | No |

On Windows one save of a file can report several `isModified` events,
because the write and the truncate are separate changes.

On macOS FSEvents can report the flags of earlier changes of a path with a
later event of the path, even after the consumer has read the events of the
earlier changes.

<!--
In the test suite, a file is created, the consumer reads
its `isCreated` event, and the file is deleted one second later. The event
of the delete has `isCreated` true in most CI runs but not in all of them,
so the test suite does not assert the macOS entry of this row.

The macOS test results could be confusing because of earlier events
getting combined with the one being tested.  Therefore, for macOS tests
we use a delay of 30 second or more to be sure, and watch the results
over many runs. If an event is reported sometimes, it is likely that
this is a repeated event from earlier operations.
-->

## Event coalescing

A program cannot count changes by counting events, on any platform. An
event reports what changed, not the state of the path after the change.
How an application handles merged events is described in
[robust-applications.md](robust-applications.md).

| Operation | Event | Linux | macOS | Windows |
|-----------|-------|-------|-------|---------|
| Write a file twice before the consumer reads the first write | One `isModified` for the file | Yes | Usually | No |
| Write `file`, write `other`, write `file` again, before the consumer reads them | Two `isModified` for `file` | Yes | Usually not | Yes |
| Create and delete a file before the consumer reads the create | One event with both `isCreated` and `isDeleted` for the file | No | Yes | No |
| Create and rename a file before the consumer reads the create | One event with both `isCreated` and `isMoved` for the old path | No | Yes | No |

### Linux

inotify merges two identical events that are adjacent in the kernel queue,
if the older one has not been read. Two consecutive writes to `foo` can
report one `isModified`. In the sequence modify `foo`, modify `bar`, modify
`foo`, all three are reported. Events of different types are never merged,
so each event has one type, and the order of the events is the order of the
operations.

### macOS

FSEvents can merge the events of a path into one event, including events of
different types:

* A file created and deleted can report one event for which both
  `isCreated` and `isDeleted` are true. The event does not give the order
  of the two operations.
* A file created and renamed reports one event for the old path for which
  both `isCreated` and `isMoved` are true.
* Two writes to a file are merged into one event only if FSEvents reports
  them in the same batch.
  <!-- The macOS entries of the two rows for writes are seen in several runs
  of the CI. -->
* Events in a directory and its subdirectory can be coalesced into one
  event for which `isEventsLost` is true, without an overflow. This is
  based on the platform documentation.

The coalescing of FSEvents is described in the "Handling Events" section of
Apple's
[File System Events Programming Guide](https://developer.apple.com/library/archive/documentation/Darwin/Conceptual/FSEvents_ProgGuide/UsingtheFSEventsFramework/UsingtheFSEventsFramework.html)
and in
[this Stack Overflow question](https://stackoverflow.com/questions/18415285/osx-fseventstreameventflags-not-working-correctly).

### Windows

The documentation does not say whether identical changes are merged. Each
event has one type.

## Recursive Watches

On Linux inotify watches only one level of a directory, and this library
implements the recursive watch in user space, by adding watches for
all subdirectories in the tree. It adds the watch of a newly created
subdirectory after it reads the create event of the subdirectory, and
then scans the subdirectory. The scan reports the entries already in
it as `isCreated` events and sets `isScanEvent` to true; for example,
`mkdir -p a/b; touch a/b/f` reports `a`, `a/b` and `a/b/f`. The kernel
does not report the changes in the subdirectory between its creation and
the addition of its watch.

On macOS and Windows the operating system implements the recursive
watch. A new subdirectory is covered by the watch in the same operation
that creates it, so no change in the new subdirectory is missed. This
follows from the design of the APIs, it is not a documented guarantee.

### New directories

"Immediately after creating" means before the consumer reads the create
event of the parent directory.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A file created immediately after creating its parent directory is reported | Yes | Yes | Yes |
| A subdirectory created immediately after creating its parent directory is watched | Yes | Yes | Yes |
| A file written immediately after creating its parent directory reports `isModified` | No | Yes | Yes |
| A file created and deleted immediately after creating its parent directory is reported | No | Yes | Yes |

On Linux these writes and deletes happen before the watch of the new
directory is added. The scan of the directory then reports the file with
`isCreated`, after the writes, so a consumer that reads the file on
`isCreated` gets its current content. A file deleted before the scan is not
reported, and it no longer exists.

### Moved directories

"Immediately after moving" means before the consumer reads the move event
of the parent directory.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A file created immediately after moving its parent directory into the tree is reported | Yes | Yes | Yes |
| A file in a directory moved out of the tree is reported | No | No | No |
| A file already in a directory moved into the tree is reported | Yes | No | No |
| A file in a directory renamed within the tree is reported | Yes | No | No |

macOS and Windows report only the directory moved into the tree, not its
contents. On Linux the moved directory is scanned and its entire tree is
reported.

A directory renamed within the tree stays watched on all platforms. On
Linux a directory is scanned on rename within the tree, and its contents
are reported.

On Linux a double rename with reuse of the intermediate path leaves a gap
in the watch of the directory. For example, `dir1` is renamed to `dir2` and
`dir2` to `dir3`, and a new `dir2` is created, before the consumer reads the
first rename. When the library reads the rename to `dir2`, `dir2` is the new
directory, so it removes the watch of the renamed directory, and adds it
again at `dir3` when it reads the second rename. Changes in the directory
between the removal and the re-add are not reported, and the scan at the
re-add reports the files in the directory again as `isCreated`.

### Subdirectory content changes

| Operation | Event | Linux | macOS | Windows |
|-----------|-------|-------|-------|---------|
| Create a file in a subdirectory | `isModified` for the subdirectory | No | No | No |
| Modify a file in a subdirectory | `isModified` for the subdirectory | No | No | Sometimes |
| Delete a file from a subdirectory | `isModified` for the subdirectory | No | No | Sometimes |

### Deletion of a subdirectory

| Operation | Event | Linux | macOS | Windows |
|-----------|-------|-------|-------|---------|
| Delete a directory recursively | `isDeleted` for each file | Yes | Yes | Yes |

### Repeated events

| Operation | Event | Linux | macOS | Windows |
|-----------|-------|-------|-------|---------|
| Create 20 directories with 200 files in each | `isCreated` twice for a file | Sometimes | Sometimes | No |

On Linux a file created after the watch of its directory is added, and
before the scan of the directory reads it, is reported by the scan and by
the kernel. On macOS a later event for a path can repeat `isCreated`.

### Subdirectory permissions

For permissions of the watch root and the path leading up to it, see the
"Watch roots" section above.  Windows by default grants change notify
privileges, so by default watching works irrespective of permissions.
The tests below are without those privileges.

Subdirectory parent permissions, up to the root, required to start the watch:

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A subdirectory of the root reports its events when the root has READ but not SEARCH | No | No | Yes |
| A subdirectory reports its events when its parent and the root have READ but not SEARCH | No | No | No |

Linux requires each parent of the subdirectory up to the root to have
READ and SEARCH.

macOS requires each parent of the subdirectory up to
the root to have only SEARCH, READ is not required.

Windows requires each parent of the subdirectory up to the root, not
including root, to have only SEARCH, READ is not required.  Watch root
directory requires READ, SEARCH is not required.

Subdirectory permissions required to start the watch:

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A subdirectory with READ and SEARCH reports its events | Yes | Yes | Yes |
| A subdirectory without READ and without SEARCH reports its events | No | No | No |
| A subdirectory with READ, without SEARCH reports its events | No | No | No |
| A subdirectory without READ, with SEARCH reports its events | No | Yes | Yes |

On Linux a subdirectory is listed when its watch is added, with
`setScanNewDirs False` as well. A subdirectory without SEARCH cannot be
listed, it is reported as `isAccessDenied` and is not watched.

Skipping permission errors:

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A subdirectory without READ does not fail the start of a recursive watch | Yes | Yes | Yes |
| A directory without READ moved into a recursive watch does not end the watch | Yes | Yes | Yes |

Permission changes during the watch:

Linux checks the permissions of a subdirectory statically, when it adds the
watch of the subdirectory. macOS and Windows check them dynamically, for
each event.

| Behavior | Linux | macOS | Windows |
|----------|-------|-------|---------|
| A subdirectory reports its events after its READ is removed | Yes | Yes | Yes |
| A subdirectory reports its events after its READ and SEARCH are both removed | Yes | No | No |
| A subdirectory without READ and without SEARCH reports its events after they are restored | No | Yes | Yes |

On Linux a subdirectory reported as `isAccessDenied` stays unwatched until
it is moved.

Permission error notifications: On Linux any permission errors are notified as
`isAccessDenied` events. On Windows and macOS you just don't receive events if
there are permission errors, the paths are not watched and there is no way to
know which paths are not watched. The applications can scan the tree for
permissions themselves.

## FreeBSD

FreeBSD 14.5, 15.0 and later releases have an inotify API compatible with
the Linux API. On FreeBSD the library uses the same implementation as on
Linux, and everything stated for Linux in this document holds for FreeBSD,
except for the differences shown in the table below.

Architectural difference: Adding the watch of a directory reads
the directory and looks up each of its entries, the time taken is
proportional to the number of entries. The read reports `isAccessed` for
the directory to the watch on its parent. In a recursive watch, each
subdirectory added generates such an event.

| Behavior | Linux | FreeBSD |
|----------|-------|---------|
| A FIFO as a root reports a write to the FIFO | Yes | No |
| A root with only READ permission reports its events | Yes | Watch fails |
| A file system can be unmounted while a tree in it is watched | Yes | No, `umount` fails with `Device busy` |
| Event reported when only the modification time of a file is set | `isModified` | `isAttrsModified` |
| Truncating a file to zero size, without a write, reports `isAttrsModified` | No | Yes |

Unlike Linux delete is prompt on FreeBSD, instead of following the object it is
reported as soon as the path is deleted:

| Behavior | Linux | FreeBSD |
|----------|-------|---------|
| A deleted open regular file root reports `isDeleted` for the root | No | Yes |
| A deleted directory root in use by a process reports `isDeleted` for the root | No | Yes |
| The `setExcludeUnlinked False` option, to report the events of a deleted open file, is available | Yes | No |

<!--
The FIFO row is tested by `FileSystem.Event`.
-->

An access to a file through any of its hard links reports an event in each
watched directory that has a link to the file, even if the access used a
link in an unwatched directory. A watched directory with two links to one
file reports each event twice, once for each name.

devfs does not report events. File systems that do not use the kernel name
cache, for example tmpfs mounted with `nonc`, may not report all the
events.
