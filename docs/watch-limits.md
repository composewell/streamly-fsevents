# Limits on File System Watches

Each limit below is a limit of the operating system. These limits are important
to understand for using the library.
See [robustness.md](robustness.md) for how the library handles error scenarios.

## Limits on Linux

On Linux each directory in a recursive watch uses one inotify watch, and
the number of inotify watches is limited. On macOS and Windows the size of
the tree is not limited.

The limits are in `/proc/sys/fs/inotify/`:

| Limit | Counts | Default | When exceeded |
|-------|--------|---------|---------------|
| `max_user_watches` | inotify watches of the user | Depends on the kernel version and the memory | An exception is raised |
| `max_user_instances` | inotify fds of the user | 128 | An exception is raised |
| `max_queued_events` | Unread events of one inotify fd | 16384 | Events are lost, `isEventsLost` is reported |

* `max_user_watches`: a non-recursive watch uses one inotify watch for each
  watch root, a recursive watch uses one watch for each directory in the
  tree. The limit is shared by all the processes of the user. Adding a
  watch raises an exception if the limit is exceeded. The event stream
  raises an exception if the limit is exceeded when a new directory is
  added to the tree.
* `max_user_instances`: one watch uses one inotify fd, for any number of
  watch roots and directories. The limit is shared by all the processes of
  the user.
* `max_queued_events`: each watch has its own queue, in its inotify fd. A
  burst of events overflows the queue if the events are not read fast
  enough. Moving a directory out of a recursive watch removes the watch of
  every directory under it, and the kernel queues one `IN_IGNORED` event
  for each removed watch. Moving out a tree of more than
  `max_queued_events` directories therefore overflows the queue. Set
  `max_queued_events` above the number of directories in the largest tree
  that is moved out.

To see the current value of a limit and to change it until the next reboot:

```
sysctl fs.inotify.max_user_watches
sudo sysctl fs.inotify.max_user_watches=1048576
```

Each inotify watch uses kernel memory only while it is in use, so a large
limit costs nothing until that many watches are added.

## Limits on macOS

| Limit | Value | When exceeded |
|-------|-------|---------------|
| Paths in one watch | 4096 | An exception is raised |
| Event streams of a process | 512, not documented by Apple | An exception is raised |
| Unread events of one watch | Not documented by Apple, about 1100 in a test | Events are lost, `isEventsLost` is reported |

* Paths in one watch: `MAX_WATCH_PATHS` in this library. It counts the watch
  roots, not the directories in the tree.
* Event streams: each watch uses one event stream, and more than one for a
  short time while its roots are changed with the handle API. The limit of
  512 for a process is tested with this library. Third party reports also
  give a limit of 1024 for the system, which is not tested.
* Unread events: in a test on macOS 26, 2000 files were created in a
  watched directory before the program read any event. The watch reported
  `isCreated` for 1057 to 1190 of them in three runs, and one `isEventsLost`
  event.
* `isKernelDropped` and `isUserDropped`, in the macOS module, give the reason
  for an `isEventsLost` event if events were dropped.

## Limits on Windows

| Limit | Value | When exceeded |
|-------|-------|---------------|
| Event buffer | 63 KB for each watch root | Events are lost, `isEventsLost` is reported |

The system holds the changes under each root in a buffer of 63 KB until the
watch reads them. If the program does not read events fast enough and the
buffer overflows, all the changes in it are lost. The watch then reports one
`isEventsLost` event for that root and continues. The program scans the tree
under the root to learn its current state; [robustness.md](robustness.md)
describes the recovery.

Each watch root uses one directory handle, Windows has no specific limit on
the number of these handles. Each watch also uses one thread and one I/O
completion port.
