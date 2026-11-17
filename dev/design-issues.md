# Design Issues

## Scan of a new subdirectory

The watch of a new subdirectory is added first, and the subdirectory is then
scanned. An entry created between the two is found by the scan and is also
reported by the kernel, so it has two `isCreated` events. Today the consumer
removes the duplicates by remembering the paths of the `isScanEvent` events,
with no point at which it can forget them unless the path is deleted or moved.

### Option 1: compare timestamps

Compare the timestamp of each entry with the time the subdirectory was created,
and do not report an entry that the kernel will report.

* inotify events have no timestamp. The reference has to come from the file
  system, for example, the birth time of the subdirectory from `statx`, or from
  a clock read by the library when it adds the watch. What separates the entries
  is the time the watch was added, not the time of the create event.
* Each entry needs a `statx` call, the scan otherwise needs only `readdir`.
* Timestamps have a coarse resolution, entries created in the same clock tick
  cannot be ordered. An equal timestamp has to be treated as "report the entry",
  which leaves some duplicates. Dropping an entry would lose its only event.
* Birth time is not available on all file systems. The modification and status
  change times are updated by later changes. The file system clock of a network
  mount can differ from the local clock.

### Option 2: emit a scan done event

Emit an event for the subdirectory when its scan is done. A consumer, or a
user level combinator, removes the duplicate create events until it sees the
event, and then forgets the paths of that scan.

* The event must come after the kernel events generated during the scan. The
  scan results are emitted when the create event of the subdirectory is read,
  before those kernel events are read, so the event cannot simply follow the
  scan results. It has to be ordered by the kernel queue. The marker used for
  the removal of watches does this: add a watch and remove it after the scan,
  and report the scan done event when its `IN_IGNORED` event is read. The kernel
  queue is in order, all the events of the scan have been read by then.
* The cost is one marker for each scan. If the marker is lost in an overflow,
  the consumer needs `isEventsLost` to end all the pending scans.
* The memory of the consumer is bounded by the entries of the scans in
  progress, and no timestamp is involved. No duplicate is left, and no entry
  can be dropped by mistake.

Option 2 is preferred. It is exact, and it needs no system call per entry.

Status: open.

## Kernel queue overflow while the consumer is slow

The kernel queue of an inotify fd holds `max_queued_events` events, 16384 by
default. The queue is read only when the consumer reads the event stream. The
queue is not read while the stream reports the entries of a scan, so a
consumer that is slow to read a large scan increases the chance of an
`IN_Q_OVERFLOW` in that period.

`max_queued_events` is a system wide sysctl that needs root to change, and
there is no per fd setting. The library cannot raise it. Raising it is the
first thing a user can do, see `docs/watch-limits.md`.

Proposed change: a reader that drains the fd into a user space buffer,
independent of the consumer. The buffer absorbs bursts larger than the kernel
queue. It needs a size limit, and when the limit is reached it must report the
loss the same way as `IN_Q_OVERFLOW`, `isEventsLost`. A consumer slower than
the event rate overflows the buffer in the end, as it overflows the kernel
queue now.

Open questions:

* Whether the reader runs in its own thread, or fills the buffer only between
  the steps of the stream, for example, between the entries of a scan.

The events in the buffer must be parsed in the order they were read. Events
read during a scan are parsed after the scan ends, as they are now.

Status: open.

## Scan ahead of the consumer

The walk of a scan runs at the pace of the consumer. Walk eagerly into a
queue of up to N entries, N close to `max_queued_events`. A smaller tree is
walked in the step that parses its event: the `scanFlags` window is shorter,
and the races during the walk are narrower.

The kernel queue is still not read until the consumer has read the queued
entries. To reduce overflows, parse kernel events after the walk ends and
append their events to the same queue, up to N in total.

Alternative for renames: when the watches are kept, report only
`isMovedFrom` and `isMovedTo`, without a walk.

Status: open.

## Report moves as creates and deletes

The portable `isMoved` does not say which end of a rename an event is for, or
whether the path left the tree, came into it or was renamed within it. A
consumer cannot tell whether to add or to remove the path. On Linux a renamed
watch root reports `isMoved` and `isRootMoved`, and neither `isMovedFrom` nor
`isMovedTo`.

A rename tells the consumer that the new path is the same object as the old
one, for example, an indexer can move its entry for `a` to `b` without reading
`b`. A consumer that handles a rename as a delete of the old path and a create
of the new path is still correct, it reads the new path again. Renames are
often not reported as renames:

* `mv a b; mv b c` before the events are read: `c` is reported as moved in.
* `mkdir a; touch a/f; mv a b` before the events are read: `b` is reported as
  moved in.
* An overflow can drop one end of a rename.
* macOS has no cookie and does not say which end is which.
* On Linux, a directory renamed within the tree reports `isMovedTo` for
  itself but `isCreated` with `isScanEvent` true for its entries under the
  new path, because the scan can find entries never reported under the old
  path, as in `mkdir a/s; touch a/s/f; mv a b`. The entries under the old
  path report nothing.

Proposed change: on all platforms, and in the portable module, report a move
as `isCreated` or `isDeleted` when the platform says which end the path is,
and as `isMoved` only when it does not. `isMoved` then means that the path
may have been created or deleted, the consumer checks whether the path
exists.

* Linux and Windows: an event for the new path of a move has `isCreated`
  true, an event for the old path has `isDeleted` true. The platform modules
  also keep `isMovedTo`, `isMovedFrom` and, on Linux, the cookie true for
  these events, as a hint that the path may be one end of a rename. The
  portable `isMoved` is never true. On Linux, `isRootMoved` also reports
  `isDeleted` for the root, enabled by default in the portable `watchWith`.
* macOS: FSEvents numbers events in order, so the source of a rename may
  be the path with the lower event ID, `getEventId`, of the two `isMoved`
  events. This is not documented and not tested. Two renames reported in the
  same batch cannot be paired this way.
* macOS: an event with only one of `ItemCreated`, `ItemRemoved` and
  `ItemRenamed` is reported as `isCreated`, `isDeleted` and `isMoved`
  respectively. An event with two or more of them is reported as `isMoved`,
  because the order of the operations is not known, for example, a file
  deleted after its create was read reports `ItemCreated` and `ItemRemoved`
  in one event. The consumer does not need to check the path for a lone
  `isCreated` or `isDeleted`. This assumes that the events of a path are
  delivered in order, so that a stale `isCreated` is followed by an event
  that reports the deletion. Add a Behavior test for the assumption.

The consumer removes the subtree of a deleted directory. On macOS and
Windows a directory created in the tree reports the entries created in it,
but a directory moved in does not report the entries it already has. Only a
moved-in directory needs a scan, and a plain `isCreated` would make the
consumer scan every new directory. Two ways to keep the difference:

* The consumer scans on a hint: `isMovedTo` on Windows, and on macOS
  `isMoved` for a path that exists and is a directory, in and out cannot be
  told apart. On Linux the library scans.
* The library scans, as on Linux: the new path of `RENAMED_NEW_NAME` on
  Windows, and the path of `ItemRenamed` on macOS if it exists and is a
  directory. The entries are reported as `isCreated` with `isScanEvent`
  true, and the consumer never scans. This costs one `stat` per rename event
  on macOS. Preferred, it removes the platform difference.

With the library scan, a consumer that handles only `isCreated` and
`isDeleted` is correct on Linux and Windows, on macOS it also handles
`isMoved`. The example in
`src/Streamly/FileSystem/Event.hs` and the `setRecursiveMode` haddock, which
use `isMoved`, change.

A consumer that wants to detect moves needs the identity of the object, the
device and inode, which no event carries. The consumer can record the
device and inode of each path and `stat` the new path of a create, this also
detects the renames reported as moves in or as creates. The library would
need the same `stat`. macOS 10.13+ (`kFSEventStreamCreateFlagUseExtendedData`)
and Windows 10 1709+ (`ReadDirectoryChangesExW` with
`FILE_NOTIFY_EXTENDED_INFORMATION`) can report the file ID without a `stat`,
inotify cannot.

`isMoved` is then also reported for events that are not moves, a name such
as `isCreatedOrDeleted` may describe it better.

The consumer code for `isMoved` runs only on macOS, it is not tested by the
Linux and Windows CI.

Alternative: on macOS the library reports `isCreated` if the path exists when
the event of a rename or of combined flags is processed and `isDeleted`
otherwise, and `isMoved` is removed from the portable module. The path can
change after the check, and the check costs one `stat` per such event.

Status: open.

## macOS: report a clone as a copy

On APFS a clone, as with `cp -c` or the Finder's "Duplicate", reports
`isCloned` for the source and for the clone, and `isCreated` for the clone.
A copy on Linux and Windows reports `isCreated` and `isModified` for the
copy. `isCloned` is in the macOS module only, so in the portable module the
event of the source matches no predicate, and the clone matches
`isCreated`. FSEvents can also report `isCloned` more than once for one
clone.

Proposed change: in the portable module, drop an event whose only item change
flag is `ItemCloned`, and report the clone as `isCreated`, which it already is.
Check whether the clone reports `ItemModified` as well; a copy does on
Linux and Windows.

Status: open.

## macOS: start a watch from a past event ID

`since` is always `kFSEventStreamEventIdSinceNow`, so a watch cannot report
the events that happened while the program was not running, and
`setFullHistory` and `isHistoryDone` have no effect. A `setSinceEventId`
setting and an accessor for the latest event ID of a volume would allow it.
An event ID is valid for one volume only, so resuming also needs the volume
UUID (`FSEventsCopyUUIDForDevice`).

Status: open.

## Reporting existing entries when a watch starts

See `dev/design-issues/scan-roots.md`.

## Adding and removing watch roots dynamically

See `dev/design-issues/dynamic-watches.md`.

## Identify a watch root by file identity

`addRoot` skips a path that is already a root, and `removeRoot` finds the
root by its path. The paths are compared by their components, with case
significant (`Path.eqPath` on macOS and Windows, byte equality on Linux).
Case sensitivity is a property of the file system, or of a directory, not
of the OS: NTFS is case-insensitive by default but a directory can be made
case-sensitive (`fsutil file setCaseSensitiveInfo`), APFS and HFS+ can be
formatted either way, and ext4 and f2fs support case-insensitive (casefold)
directories. A path comparison cannot know which applies, so ignoring case
would treat two different directories on a case-sensitive file system as
one root.

With the case-significant comparison, the same directory added as
`C:\Foo` and `c:\foo`, or via a symbolic link, becomes two roots, and its
events are reported once for each root.

### Proposal: compare file identity

Identify a root by the identity of the directory: the volume serial number
and the file index from `GetFileInformationByHandle` on Windows, the device
and inode from `stat` on Linux and macOS. `addRoot` skips a directory that
is already a root under any path, and `removeRoot` removes the root that is
the same directory as the path.

* `removeRoot` changes from "the same path as used to add the root" to "the
  same directory". The root to remove has to exist to get its identity, a
  root that was deleted can only be removed by its path.
* On macOS a root may not exist when it is added, it has no identity until
  it is created.
* A directory deleted and created again at the same path has a new identity.
* Network file systems may not give stable inode numbers or file indexes.

Status: open.

## Linux: restart a watch on request

A directory that cannot be added, for example, at the `max_user_watches`
limit (`ENOSPC`) or without a free fd (`EMFILE`), is not watched, and
neither is the tree under it. The read of the stream raises the
`IOException`, and the operation is not tried again. After the user raises
the limit, nothing adds the missing directories:

* `addRoot` on a root that is already watched changes its flags only,
  the tree is walked only when the watch of the root is new.
* `removeRoot` followed by `addRoot` on the root adds the tree
  again, but the events between the two calls are lost and the root is
  reported as `isRootUnwatched`.

The proposal was `restartWatch :: Watch -> IO ()`, which would run the
restart of the tree after an overflow on request.

Status: rejected. The library no longer restarts a watch after an overflow,
the stream ends and the application opens a new watch, see "Overflow: the
stream ends" in `dev/design.linux.md`. The recovery from `ENOSPC` is the
same: raise `max_user_watches`, close the watch, open a new one and scan the
tree.

## Linux: follow symbolic links inside a recursive tree

A symbolic link inside a recursive tree is never followed. Following a link
to a directory needs:

* Cycles: ELOOP does not detect a link to an ancestor, `ln -s .. root/a/up`.
  If `inotify_add_watch` returns a wd already in the map for the root, add
  the path but do not descend into it.
* Duplicates: already supported, a wd can have more than one path.
* Retargeting: a link entry has no `IN_ISDIR`. On `IN_CREATE` and
  `IN_MOVED_TO` of a non-directory, stat through the link. Record which paths
  are links, so that `IN_DELETE` and `IN_MOVED_FROM` of a link remove its
  subtree.
* Changes on the target path: with `root/a/l -> ../b`, `mv root/b root/c`
  leaves `l` dangling, and `mkdir root/b` makes a dangling `l` valid. Neither
  generates an event on `l`. Inside the tree, keep an index from the
  directories on each target path to the links resolved through them, and
  resolve those links again when one of the directories changes. Outside the
  tree there are no events; the guarantee is the same as for a root, where a
  rename of `b` in `/a/b/c` is not detected.

Proposed options:

* A new option to follow links inside the tree, `setFollowSymLinks`.
  `setFollowRootSymLinks` applies only to roots.
* A new option to allow a followed link to lead outside the tree. When it is
  off, a link whose target is outside the root is not followed.

Status: open.

## Windows: support the non-threaded RTS

Each watch root is read by a thread blocked in a synchronous
`ReadDirectoryChangesW` call. With the non-threaded RTS the call blocks all
the Haskell threads until a change occurs under that root: the program does
not do anything else while waiting, a watch of several roots delays the
events of the other roots until a change occurs under the blocked root, and
a stopped watch is cleaned up only after the next change, because the RTS
cannot cancel the call with `CancelSynchronousIo` from another OS thread.
`watchWith` raises an exception when the stream starts if the program uses
the non-threaded RTS.

### Option 1: overlapped call with polling

Issue an overlapped `ReadDirectoryChangesW` call and poll the `OVERLAPPED`
structure with `HasOverlappedIoCompleted`, with a `threadDelay` between the
polls. A stopped watch cancels the call with `CancelIoEx`. This works with
both the RTS kinds, at the cost of the polling interval in latency and one
wakeup per interval for each root, even when nothing changes.

### Option 2: the native Windows I/O manager

GHC's native I/O manager (WinIO) completes overlapped I/O through an I/O
completion port without blocking the RTS. It is used only when the program
runs with `+RTS --io-manager=native`, and its API is in `GHC.Event.Windows`,
which is internal to `base` and has changed between GHC versions. The
library would still need the threaded RTS, or option 1, for programs that
use the default I/O manager.

Status: open.

## A renamed watch root

On Linux a renamed root is removed from the watch and reports `isMoved`
and `isRootUnwatched`; the kernel does not report the new path. On macOS the
watch is attached to the path, the renamed root reports `isMoved` and is no
longer watched, and an object created at the path later is watched. On Windows the watch keeps a
handle to the root open with `FILE_SHARE_DELETE`. After `mv d d.bak` the
watch continues, and the events report paths under `d`, for example, `d\x`
for the file `d.bak\x`. A consumer that opens, copies or deletes `d\x` acts
on a path that does not exist, or on another object created at `d` later.
`ReadDirectoryChangesW` does not report changes to the watched directory
itself, so the library does not know that the root was renamed.

### Option 1: remove the watch, as on Linux

End the watch of a renamed or deleted root with an event, `isRootUnwatched`
as on Linux. Windows has no such event yet.

* The user names a path, not an object. A directory replaced by renames, as
  in `mv d d.old; mv d.new d`, leaves a watch that follows the object on
  `d.old`.
* `getRoot` does not change during a watch, a consumer can keep its state
  by root.
* A deleted root ends the watch on all platforms, a rename then ends it the
  same way and the consumer handles both with one event.
* macOS can do the same, FSEvents watches a path.

To detect the rename on Windows:

* Record `GetFinalPathNameByHandleW` of the handle when the watch starts, and
  call it again after each `ReadDirectoryChangesW` call returns. If the path
  is different, report the event, close the handle and end the stream of the
  root. This costs one system call per read and no thread. The rename is
  detected only when the next event under the root arrives, never in an idle
  tree. A read cannot be split at the rename, the events of the read in
  which the rename is detected are all kept or all dropped. The final path is
  compared, not the user path, so a root reached through a symbolic link or
  a junction is not reported as renamed. This cannot tell a rename from a
  delete.
* Also watch the parent of the root, non-recursive with
  `FILE_NOTIFY_CHANGE_DIR_NAME`. `FILE_ACTION_RENAMED_OLD_NAME` or
  `FILE_ACTION_REMOVED` for the name of the root reports the change when it
  happens, and tells a rename from a delete. This needs a handle and a
  thread per root, fails if the parent cannot be read, and a drive root such
  as `C:\` has no parent.
* An overlapped call with a timeout, see "Windows: support the non-threaded
  RTS", lets the first check run in an idle tree too.

Optionally report the new path in the event on Linux and Windows, so that a
consumer can add the new path to follow the object. Linux can get the path
by keeping an `O_PATH` fd of the root and reading `/proc/self/fd/N`, Windows
from `GetFinalPathNameByHandleW`.

### Option 2: continue the watch, on all platforms

Continue the watch of a renamed root and report the events under the new
path. Linux keeps the inotify watch on the inode after `IN_MOVE_SELF`, and
can get the new path as in option 1. macOS has to start a new FSEvents
stream on the new path, `fcntl(F_GETPATH)` on an fd of the root gives it,
and the events between the rename and the new stream are lost. `getRoot`
changes during the watch, which needs a new event to tell the consumer the
new root.

### Option 3: watch the path, as on macOS, on all platforms

On Linux and Windows, also watch the parent of each root, non-recursive,
for name changes only, and keep only the events for the name of the root:

* The name deleted or moved away: report `isRootDeleted` or `isRootMoved`
  and remove the watch of the root.
* An object created or moved in at the name: add a watch on it and scan it.
  The scan finds the entries created before the watch was added, as in
  `mkdir d; touch d/f`, and the entries of a directory moved in. Linux
  already does this for a new subdirectory, see `setScanNewDirs`.
* A root that does not exist when the watch starts: watch its deepest
  existing ancestor and move the watch down as each component of the path
  is created. macOS watches such a root, Linux and Windows raise an
  exception today.

A watch on the parent covers a rename of the last component of the root path
only. FSEvents watches the whole path, `mv /a /a.old; mkdir -p /a/b/d`
watches the new `/a/b/d`. Doing the same needs a watch on each ancestor up
to the volume root, and a rename of an ancestor moves all the watches below
it.

Costs and limits:

* The parent watch receives the name changes of all the entries of the
  parent, for example, of a home directory or `/tmp`, and the reader drops
  all but those of the root. On Windows the filter can be
  `FILE_NOTIFY_CHANGE_DIR_NAME`, with `FILE_NOTIFY_CHANGE_FILE_NAME` for a
  file root.
* Linux: one more inotify watch per parent, counted in `max_user_watches`.
  Roots with the same parent can share it. Windows: one more handle and one
  more blocked thread per parent.
* The parent needs read permission on Linux and `FILE_LIST_DIRECTORY` on
  Windows. `/`, `C:\` and a UNC share root have no parent, and the parent of
  a mount point is on another file system. These roots need option 1.
* The events under a new object at the root path before its watch is added
  are reported only by the scan. macOS does not report the existing entries
  of a directory moved in at the root path either.
* A symbolic link root replaced, as in `ln -sfn new d`, changes the watched
  object, the watch of the root has to follow it.

The behavior of option 1 can then be a setting, for example,
`setFollowRootPath False`.

Option 1 is preferred. It is the simplest, and costs nothing per root.
Option 3 gives the behavior a user who names a path expects, including
for a directory replaced by renames, but it adds a watch, a thread on
Windows and a second event source per root, and is complete only with a
watch on every ancestor.

What a pending `ReadDirectoryChangesW` call returns when the root is deleted
is not tested, see `dev/todo.windows.md`.

Status: open.
