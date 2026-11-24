# Writing Robust Applications

A watch can miss a change, report one change more than once, or report
several changes in one event. An application that keeps a model of the
tree from the events handles these cases as described below. How the
library itself handles large trees, overflows, changes during a scan and
exceptions is described in [robustness.md](robustness.md), the limits of
the operating systems in [watch-limits.md](watch-limits.md).

## Guidelines

* Apply each event as an update of the state the application keeps, not
  as a count of changes. One event can report two changes, and on macOS and
  Windows one change can be reported by more than one event. An `isCreated`
  event for a path already in the state updates the entry of the path.
* Test every predicate of an event that the application handles. More than
  one predicate can be true for one event, for example `isCreated` and
  `isDeleted` on macOS. A chain that stops at the first true predicate
  misses the other changes reported by the event.
* An event reports what changed, not the state of the path after the
  change. When the state matters, for example whether a path reported with
  both `isCreated` and `isDeleted` exists, call `stat` on the path.
* There is no guarantee that the path of an event, or the root of the
  event, is available when the application processes the event. The path
  or a parent directory can be deleted or renamed after the event is
  reported. An operation on the path, for example `stat`, can then fail or
  refer to another file.
* When a directory is reported deleted or moved out of the tree, remove the
  directory and every path under it from the state. On macOS the deletion
  of a watch root does not report the files in it, and on all platforms a
  directory moved out of the tree does not report the entries under it.
* Apply `isMovedFrom` of a directory as the removal of the directory and
  every path under it, and `isMovedTo` as the addition of the directory,
  without matching the two events of a rename.
* Scan a directory reported as moved into the tree. On macOS and Windows
  the entries already in the directory are not reported. On Linux the
  library scans the directory and reports its entries as `isCreated`
  events with `isScanEvent` true, unless `setScanNewDirs` is `False`.
* On an `isEventsLost` event, scan the tree under the path returned by
  `getAbsPath` for the event. On Linux the watch ends; open a new watch on
  the same roots before the scan. A program that runs on all platforms can
  use the Linux recovery on every platform.
* On Linux, a subdirectory reported as `isAccessDenied` is not watched. It
  is added when it is moved, or when the program opens a new watch.
* A root whose path no longer refers to the watched directory ends with
  `isRootUnwatched` and `isRootStale`, see `enableStaleRootCheck`. A
  deleted root, and on Linux and macOS a renamed root, ends at its
  deletion or rename event. A root whose path changes without an event of
  the root, e.g. after the rename of a parent directory on Linux, a change
  of a symbolic link in the path, or a rename of the root on Windows, ends
  at its next event. To watch the directory now at the root path, call
  `addRoot` and then scan the tree at the root path. `addRoot` does not
  report the entries already in the directory. A root that has no events
  is found by `staleRoots`, call it periodically and call `removeRoot`
  before `addRoot` for each root it returns. With the check off, until the
  scan, the state the application keeps may have entries from the events
  of the old directory, reported under the root path.

## Handling queue overflow

Events are lost on all platforms when they arrive faster than the program
reads them. An event for which `isEventsLost` is true reports the loss.

| | Linux | macOS | Windows |
|-|-------|-------|---------|
| Path of the `isEventsLost` event | Each watch root | A path, which can be a subdirectory of the root | The watch root |
| The stream continues after the event | No, the watch is closed and the stream ends | Yes | Yes |
| What the program does | Open a new watch on the roots, then scan the tree | Scan the tree under the path of the event | Scan the tree under the watch root |

A program that runs on all platforms can use one recovery: on an
`isEventsLost` event, close the watch, open a new watch on the same roots,
then scan the tree. On macOS and Windows this scans more than needed, the
tree under the path of the event is enough.

### Linux

If you receive an `isEventsLost`:

* Open a new watch on the same roots, for example by running
  `watchWith` again, and then scan the tree. Open the watch before the scan,
  so that a change made during the scan is reported by the watch, by the scan
  or by both. Opening a watch does not generate any events, on any size of
  tree. A program can instead exit and let its service manager, such as
  systemd with `Restart=on-failure`, start it again; the program then scans
  the tree at its start.
* Scan the tree in a thread other than the one reading the stream. If a
  single thread scans the tree and reads the stream, it reads the stream only
  after the scan, and by then the kernel queue may have overflowed again.
* With open, access and close events enabled, a scan of the tree
  generates 8 events per directory. A tree of more than
  `max_queued_events / 8` directories, 2048 by default, then overflows the
  queue. Do not enable these events on a recursive watch of a large tree, or
  raise `max_queued_events`.

Under sustained load a new watch can overflow again. We have a stress
test which overflows the queue and then changes a tree of 10000
directories for 10 seconds while the consumer opens a new watch after
each overflow; one run had 13 overflows. After the changes stopped, a
file created in every directory was always reported exactly once.

### macOS and Windows

If you receive an `isEventsLost`:

* Do not open a new watch, the watch continues after the event.
* Scan the tree under the path returned by `getAbsPath` for the event. On
  Windows this is the watch root, on macOS it can be a subdirectory of the
  root. A change made during the scan is reported by the watch, by the scan
  or by both.
* Scan the tree in a thread other than the one reading the stream. If a
  single thread scans the tree and reads the stream, it reads the stream only
  after the scan, and by then the system buffer may have overflowed again.
* Under sustained load an overflow can follow another, and each one is
  reported by its own `isEventsLost` event. If an `isEventsLost` event is
  read while a scan of the same tree is running, start one more scan after
  the running scan ends, instead of a scan for each event.

## Missed and out of order events

A consumer that applies the events in order to a model of the tree can miss
a change, or not know the order of two changes, in the cases listed below.
The details and the test results of each case are in
[platform-differences.md](platform-differences.md) and
[robustness.md](robustness.md).

### Changes not reported

All platforms:

* Queue overflow: events that arrive faster than the program reads them are
  lost. An `isEventsLost` event reports the loss.
* A directory moved out of the tree reports one event for the directory.
  The entries under it are not reported.

Linux:

* In a new directory, the changes made before the library adds the watch of
  the directory are not reported. A file created and deleted in that
  interval is not reported at all. A write to a file in that interval is not
  reported as `isModified`; the scan of the directory reports the file as
  `isCreated`.
* A change in a directory renamed twice, `dir1` to `dir2` to `dir3`, with
  a new `dir2` created before the first rename is read, can be missed. The
  scan of `dir3` reports its files as `isCreated`.
* A subdirectory without the read or the search permission is not watched,
  and is reported as `isAccessDenied`.
* The rename of a parent directory of a root is not reported. The root
  stays watched and its events report the old path.
* A mount or an unmount at the path of a root, or of a parent directory of
  a root, is not reported unless the file system of the root itself is
  unmounted.
* After `removeRoot`, the events of the root still in the kernel queue are
  not reported.
* A write to an open file after the file is deleted is reported on Linux
  only if `setExcludeUnlinked` is `False`, and never on FreeBSD.
* FreeBSD: devfs does not report events. File systems that do not use the
  kernel name cache, for example tmpfs mounted with `nonc`, may not report
  all the events. A write to a FIFO root is not reported.

macOS and Windows:

* A directory moved into the tree reports one event for the directory. The
  entries already in the directory are not reported.

macOS:

* The deletion of a watch root, for example with `rm -rf`, does not report
  `isDeleted` for the files in it.
* A subdirectory without any permission does not report its events. A
  subdirectory with the search permission and without the read permission
  reports its events.
* After the parent directory of a root is renamed, the events of the root
  at the new path are not reported.
* The events of a directory and its subdirectory can be coalesced into one
  `isEventsLost` event, without an overflow. This is based on the platform
  documentation.
* On a volume that does not keep a history of file system events, for
  example a read-only or a network volume, `addRoot` and `removeRoot` can
  lose the events of the other roots that occur during the call. An
  `isEventsLost` event may be reported in their place.
* After `removeRoot`, an event of the root that occurred shortly before
  the call may not be reported.
  <!--
  Tested by FileSystem.Event, by the --match path of the test:
  "/FS.Event/handle API/isRootUnwatched is the last event of a root removed with a change/"
  -->

Windows:

* Without the "Bypass traverse checking" privilege,
  `SeChangeNotifyPrivilege`, a change in a subdirectory is not reported if
  the watcher does not have the traverse permission to the subdirectory.
  Windows grants this privilege to all users by default.
* After `removeRoot`, the changes of the root that the watch has not yet
  read from the system are not reported.

### Order not known, or events reported again

On Linux each event reports one type of change, and the events are in the
order of the operations. The exceptions on Linux, and the cases on macOS
and Windows, are:

* All platforms: two changes can be reported by one event. On Linux two
  writes to a file, before the consumer reads the first one, can report one
  `isModified`.
* macOS: FSEvents can merge the changes of a path into one event. A file
  created and deleted can report one event with both `isCreated` and
  `isDeleted` true, and the event does not give the order of the two
  changes. A file created and renamed can report one event for the old path
  with both `isCreated` and `isMoved` true.
* macOS: a watch root, or a directory under the root of a recursive watch,
  deleted and created again can report one event for its path with both
  `isDeleted` and `isCreated` true. With `allowMissingRootPath True`
  macOS watches the path, so the root path then reports the events of the
  new object, and the event does not tell which change belongs to the
  deleted object and which to the new one.
* macOS: a later event of a path can repeat the flags of earlier changes of
  the path, even after the consumer has read the events of the earlier
  changes. A file created, with its `isCreated` event read, and deleted one
  second later usually reports `isCreated` again with `isDeleted`.
* macOS and Windows: one overwrite of a file can report more than one
  `isModified`. On Windows the write and the truncate of a save are
  separate changes.
* Linux: an entry created in a directory while the library scans the
  directory is reported twice, by the scan and by the kernel. A directory
  renamed twice before the first rename is read, with its first new path
  taken by another directory, is scanned again and its entries are
  reported again.
* Linux: some events of a deleted root, rarely, are reported between its
  `isDeleted` and its `isRootUnwatched` events.
