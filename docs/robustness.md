# Robustness and Scalability

The library is designed for long running watches on large trees, in programs
with many threads and child processes. Each item below is covered by the test
suite of that platform, unless noted. The limits of the operating systems
are listed in [watch-limits.md](watch-limits.md).

## Large trees on Linux

* A watch starts on a tree of any size without overflowing the event queue,
  with all event types enabled. Listing a directory generates open, access
  and close events; the library enables these three on a directory only after
  it has listed the directory and the tree under it. Create, delete, modify
  and move events are enabled from the start, so changes made during the
  start are reported. Open, access and close events of other processes during
  the start are not reported.
* Deep trees do not run out of file descriptors. A walk keeps at most 32
  levels of the tree open, so the number of open files does not grow with the
  depth of the tree. In a test with the open file limit at 256, a watch
  starts on a chain of 300 nested directories, and the scan of the same chain
  moved into the tree completes.
* Moving a large tree into the watch scans it without loading the whole tree
  into memory.

Measurements on Linux 6.12, 2 CPUs, warm caches, on a tree of 28259
directories, 10 in each directory. This is the largest tree that a
`max_user_watches` of 30300 allows. Each time is the average of three runs:

| File system | Listing the tree | Start of the watch | Move in or rename of 14129 directories |
|-------------|------------------|--------------------|----------------------------------------|
| tmpfs | 0.25 s | 0.78 s | 0.45 s |
| ext4 | 0.32 s | 0.82 s | 0.50 s |

The start time is measured to the first event read. The move time includes
the scan that reports every entry of the moved directories.

| Memory | Result |
|--------|--------|
| Live heap after the start of the watch | 8.7 MB, about 320 bytes per directory |
| Live heap after each of three moves out and back in, and renames and back | 8.7 to 8.8 MB |
| Peak, reached during the start of the watch | 9.8 MB |

Every directory moved in was reported by the scan, a file created in each
directory at the end was reported once, and no events were lost. On a watch
of 24200 directories, 2000 renames of an empty directory take 0.40 s, about
5000 renames per second. `dev/testing.md` describes how to run these
measurements.

## Event queue overflow

Events are lost on all platforms when they arrive faster than the program
reads them. An event for which `isEventsLost` is true reports the loss. The
program then scans the tree under the path returned by `getAbsPath` to learn
its current state.

| | Linux | macOS | Windows |
|-|-------|-------|---------|
| How the loss is reported | One `isEventsLost` for each watch root | `isEventsLost` for a path, which can be a subdirectory of the root | `isEventsLost` for the watch root |
| The stream continues after the loss | No, the watch is closed and the stream ends | Yes | Yes |
| What the program does | Open a new watch on the roots, then scan the tree | Scan the reported path | Scan the watch root |
| A scan of the tree by the program generates events | Yes, if open, access and close events are enabled | No | No |

On Linux the events read before the overflow are reported, and the events
queued after it are discarded with the watch. The kernel keeps one overflow
event in its queue at a time, and until that event is read it can drop more
events without another overflow event, so the events after an overflow do not
give a complete picture of the changes.

The Linux column is tested by the test suite `FileSystem.Event.Stress`. The
macOS and Windows columns are based on the platform documentation.

## Handling queue overflow on Linux

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

## Symlink safety on Linux

The tree can change while the library adds the watch of a directory and
reads the entries in it. A user with write access to the tree cannot
make the watch fail or follow a path outside the tree.

Symlinks for all components leading up to the watch root, except the
last directory component, are always followed. The last component
is followed only if the `setFollowSymLinks` option is enabled. Symlinks
in the tree under the watch root are never followed. Why is this
important? Let's say a daemon is watching the home directory of a user
for backups, the user creates a symlink to "/", the watch crashes in
trying to add recursive watches for everything under "/", and we do not
even want to watch those.

`inotify_add_watch` takes a path and follows symbolic links in all but
its last component. A subdirectory added by its path could therefore
reach a directory outside the tree, through a parent replaced by a
symbolic link, and we guarantee not to watch anything outside the tree.

To solve this, subdirectories are opened one component at a time with
`O_NOFOLLOW` to ensure we are not following a symlink, even then we
do not add the watch by the path because the path may change between
the two calls, so we add the watch via the path of the fd in `/proc`. If
`/proc` is not mounted or access to it is denied, a parent replaced by
a symbolic link in the short interval between the check and the add is
followed.

A symbolic link loop is treated as a directory that no longer exists.

## Changes in the tree during a scan on Linux

In a changing file system:

* A directory replaced by a file is not watched.
* A directory deleted or moved, and then created again at the same path,
  is watched as expected.
* A subdirectory without read or search permission is not watched and is
  reported as `isAccessDenied`, the rest of the tree is watched. Thus, a user
  who creates a directory with mode 000 cannot end the watch.
* Changing the permissions of a subdirectory after adding it does not affect
  the watch, it stays watched. A directory skipped as `isAccessDenied` is
  not added when its permissions change later; it is added when it is
  moved, or when the program opens a new watch.
* A directory renamed within the tree stays watched through the rename
  and events are not lost during the rename.  A directory renamed
  twice before the first rename is read, as in `mv a b; mv b c`, stays
  watched through the rename. If the first new path is taken by another
  directory by the time the rename is read, as in `mv a b; mv b c; mkdir
  b`, the directory is added again and scanned, and its entries are
  reported again.
* A directory reached by two paths, for example after a move, keeps only
  one watch, and each event is reported once.

We have a stress test which runs every sequence of up to two `mkdir`,
`rmdir`, `touch`, `rm`, `ln -s`, `rename` and write operations, on a
small set of paths that includes one outside the tree, before the events
are read. It checks that a consumer applying the events in order knows
every entry of the final tree, that a file created in each directory is
reported once for each root at its path, and that a write to a file is
reported at its path unless its directory was created or moved into the
tree during the sequence. CI runs 2157 sequences. 51436 sequences of up
to three operations, and of up to four on fewer paths, are run manually
after a change in the handling of moves or scans.

## Overlapping roots on Linux

* A root listed twice reports each event once.
* Two roots that reach the same directory, for example a recursive root
  and a root inside it, report each event once for each root.
* Removing one of the two roots keeps the other one watching.

## Exceptions and resource cleanup

All platforms:

* The streaming API opens the watch in a bracket around the
  stream. The watch is closed when the stream ends, when it raises an
  exception, or on garbage collection if the stream is abandoned.

Linux:

* A watch that fails to start closes its inotify fd.
* Killing a thread waiting for events closes the watch at once. An
  exception that arrives during the start of the watch or in the
  consumer of the stream, or a stream terminated by `take`, leaves the
  watch to be closed by garbage collection. In a kill test, 2000 kills at
  varying points leave no inotify fd open and no thread blocked, with
  the threaded and the non-threaded RTS.
* The inotify fd is close-on-exec, so a child process does not keep a
  closed watch alive.
* Handle API: `addRoot`, `removeRoot` and `closeWatch` return without
  waiting for the thread reading the stream, and can be called from any
  thread while another thread reads the watch. `closeWatch` ends the
  stream of that thread. With the non-threaded RTS, and no free fd or
  no `/proc`, the stream ends when the next event arrives.
* Handle API: after `removeRoot`, the events queued for the root before
  the removal are reported, up to its `isRootUnwatched` event. For a
  directory that the root shares with another root, this requires `/proc`
  and a free fd. If `/proc` is not mounted or access to it is denied, the
  root stops reporting that directory at once, and its `isRootUnwatched`
  event is reported before the next event. If no fd is free, `removeRoot`
  throws an exception, the directory stays watched for the root, and the
  call can be retried.
* Handle API: a stream returned by `eventStream` does not close the
  watch when it ends. After a timeout, or after an `IOException` from
  adding or reading a directory, for example when no fd is free, the
  next stream on the watch continues with the remaining events. The
  directory that failed is not watched. An event is lost if the timeout
  arrives during the step that produces it. Any other exception is
  raised again by every later read.

macOS:

* The event pipe is close-on-exec, so a child process does not keep it
  alive.
* A watch beyond the 512 event streams of a process raises an
  exception, instead of starting a watch that reports no events.
* Closing a watch waits for a running callback to finish. A callback
  blocked on a full event pipe does not raise `SIGPIPE`, and closing the
  watch does not hang if a child process holds the pipe.

Windows:

* Stopping a stream interrupts the blocked `ReadDirectoryChangesW`
  call, so the stream does not wait for the next change. This requires the
  threaded RTS.

## Unusual names on Windows

* A file name such as `aux.c` or `con`, which the file system can hold, does
  not end the watch with an `InvalidPath` exception.
* `showEvent` accepts a name with an unpaired UTF-16 surrogate, which NTFS
  does not reject.
