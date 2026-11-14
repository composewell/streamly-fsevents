# Reporting existing entries when a watch starts

Not implemented. This note records the design for reporting the entries that
already exist under a watch root when the watch starts.

A watch reports only changes made after it starts. A program that needs the
current state, or the changes made while it was not running, has to walk the
tree itself. To avoid a gap between the walk and the watch it has to start the
watch first, and the two then have to be merged by hand.

This is a different feature from the scan of new subdirectories in recursive
mode on Linux. That scan recovers the events lost before the watch of a new
subdirectory is added, and it is on by default. The scan described here is
for the user supplied watch roots and it is off by default.

## Portable design

### API

The setting is portable.

```haskell
data ScanMode
    = ScanNone            -- default
    | ScanAll
    | ScanSince POSIXTime

setScanRoots :: ScanMode -> Config -> Config
```

* `ScanNone` keeps the current behavior: monitoring starts from the current
  time.
* `ScanAll` reports every entry under the watch roots. It needs no stat calls
  when the file system provides the entry type in the directory listing.
* `ScanSince t` reports the entries changed at or after `t`. It needs the
  timestamps of each entry, which on Linux costs one stat call per entry.

The default is `ScanNone` because a scan changes the meaning of a watch from
"changes from now on" to "the state now, then the changes". A scan also costs
time proportional to the number of entries in the tree.

### Behavior

* The watch is added first, then the roots are scanned, then the kernel events
  are read. A change made during the scan is reported by the watch, so no
  change is lost. An entry changed during the scan may be reported twice.
* In recursive mode the scan walks the whole tree. Otherwise it reads only the
  watch root directory.
* The scan runs inside `watchWith`, after the watch is opened. It is not
  available via `addRoot`. `addRoot` can be called from any thread, its
  scan results would need a queue read by the stream, and a wake up of a
  reader that is waiting for events.
* The scan is a stream and does not build a list, a tree can be large.
* Each entry is reported as an event for which both `isCreated` and
  `isScanEvent` are true. `isCreated` is true so that a consumer that filters
  on `isCreated` receives these entries. `isScanEvent` tells the consumer that
  the entry exists and may not be new.
* The events are not read during the scan. A long scan can overflow the
  buffer that holds them, which is reported by `isEventsLost` in the usual
  way.

### Deletions

`ScanSince` reports additions and changes. A deleted entry has no timestamp, so
it cannot be reported. A program that catches up after a restart and needs the
deletions has to compare the result with a snapshot that it saved.

### Timestamps

`ScanSince t` selects an entry if its change time is greater than or equal to
`t`. The change time must be updated when an entry arrives at its path. The
`mtime` alone is not enough: `mv`, `cp -p`, `rsync -t` and `tar x` can leave
an old `mtime` on a file that is new at its path.

The comparison is `>=` and not `>` because the kernel takes file timestamps
from a coarse clock. A file written just after `t` can have a timestamp
slightly earlier than `t`, so callers should choose `t` with a margin. To be
tested: the size of the coarse clock error, and whether `>=` is sufficient. A
file server with a different clock has a larger error.

### Time type

`POSIXTime` from the `time` package. `time` is a GHC boot package, so the
dependency costs nothing. The time types of streamly-core,
`Streamly.Internal.Data.Time.Units`, are internal modules and should not be
part of a public API.

Alternative: take a `Double` of seconds since the epoch. No dependency, but the
type does not state the unit or the epoch.

### Alternatives

* **Separate combinator instead of a setting.** `scanRoots t paths` could
  be appended to the watch stream by the user. Rejected: the scan has to start
  after the watch is added and before its events are read, which a combinator
  outside the `bracket` of the watch cannot do.
* **Two settings, one for the scan and one for the filter.** Rejected:
  `ScanNone` with a time is meaningless, one type rules that out.
* **Native history.** FSEvents can replay events from a saved event id, and
  the NTFS USN journal keeps a change log. Both report deletions. Linux has no
  equivalent, so this cannot be the portable mechanism. It can be added to the
  platform specific modules.
* **Report a separate event type instead of `isCreated`.** A new predicate
  alone, for example `isExisting`, is more precise, but every consumer has to
  handle it to see these entries. `isCreated` with `isScanEvent` gives the same
  information and existing consumers keep working.

### Implementation notes

* `isScanEvent` is exported only by the Linux module now. This feature needs it
  in the portable module, with an implementation on macOS and Windows.
* The directory walk is not specific to inotify, one implementation can serve
  macOS and Windows.
* Add behavior tests for the scan in `FileSystem.Event.Behavior`, so that the
  platform differences of the timestamps are recorded.

## Linux

* `inotify_add_watch` starts recording at once, so the order watch, scan,
  read has no gap.
* The events of the scan period wait in the inotify queue,
  `max_queued_events`, 16384 by default.
* The recursive start already walks the tree with `scanFlags` to add the
  watches. That walk should produce the entries when a scan is requested, so
  that the tree is read once and the scan generates no events.
* `getdents` returns no timestamps, `ScanSince` needs a `statx` call per
  entry. The change time is the later of `mtime` and `ctime`. A program
  cannot set `ctime`, and a rename updates it on most file systems.

## macOS

* `createWatch` starts the FSEvents stream before it returns, so the order
  watch, scan, read has no gap with the current code.
* The events of the scan period wait in the pipe, then in fseventsd. When
  fseventsd drops events it sets `UserDropped` or `KernelDropped`, reported
  by `isEventsLost`.
* FSEvents always watches a tree. In non-recursive mode the scan reads only
  the root.
* `getattrlistbulk` returns the timestamps with the names, no stat call per
  entry is needed. The change time is the later of `mtime` and `ctime`, as on
  Linux.
* Platform specific option: `ScanSince` through `sinceWhen`, using
  `FSEventsGetLastEventIdForDeviceBeforeTime`. It reports deletions and ends
  with `isHistoryDone`, but event ids are per device, the history is missing
  on some volumes, and it reports events, not entries.

## Windows

* Changes are recorded only from the first `ReadDirectoryChangesW` call on a
  handle. The current code calls it synchronously in the reader thread, with
  no point at which recording is known to have started. The first call has
  to be issued with `OVERLAPPED` before the scan starts.
* The events of the scan period wait in a buffer sized by the first call,
  63 KB in the current code, about a thousand events. A long scan with
  concurrent changes is likely to overflow it. A reader that drains the
  handle during the scan avoids this.
* The recursive watch is native, the walk is new code. It must not follow
  junctions or symbolic links.
* `ctime` is not a change time. The directory listing returns the creation
  and last write times, and `FileIdBothDirectoryInfo` also returns
  `ChangeTime`, no call per entry is needed. To be tested: whether a rename
  updates `ChangeTime`, and the effect of file name tunnelling on the
  creation time.
* Platform specific option: the USN journal reports deletions, but it needs
  administrator rights to open the volume and works only on NTFS and ReFS.
