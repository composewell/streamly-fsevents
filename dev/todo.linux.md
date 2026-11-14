# Linux TODO

Problems found by reading `src/Streamly/Internal/FS/Event/Linux.hs`. None of
them has been reproduced. Reproduce a problem with a test before changing the
code, and delete its entry when it is fixed.

## removeRoot during a scan

`removeRoot` marks the paths under the root as removed. A scan of a
directory under that root that is in progress keeps adding the subdirectories
it finds: `insertPath` adds a path without checking that its root is still
watched. `handleEvent` selects the paths that are not removed from the map it
read at the start of the event, it does not see a root removed after that
either.

The paths added after the removal are not marked removed. They report events
under the removed root, and no `isRootUnwatched` event is reported for them.
`restartTree` does not remove them, their root is neither in the roots it
restarted nor in the roots it skipped. They stay until `closeWatch`.

"Remaining limits" under the soft restart in `dev/design.linux.md` lists the
same problem for a root removed during the walk of a restart.

To reproduce: move a large tree into a recursive root, read a few of its scan
events, call `removeRoot` on the root, read the rest of the events, and
create a file in a subdirectory that the scan reported after the removal.

Proposed change: `insertPath` does not add a subdirectory path if its root
has no path that is not removed. The path index of `WdMap` finds the paths of
the root.

## Async exception in removeRoot and addRoot

`unwatchPaths` runs with async exceptions unmasked when `removeRoot`
calls it from a user thread. It marks the paths as removed in the map, and
then calls `inotify_rm_watch` for each watch descriptor whose paths are all
removed. An async exception between the two, for example, a timeout, leaves
paths that are marked removed and whose kernel watches still exist:

* The kernel does not generate `IN_IGNORED` for them. They do not report
  `isRootUnwatched`, and they keep reporting events.
* Calling `removeRoot` again does nothing, it skips the paths already
  marked removed.
* After an overflow `syncRemovals` removes them from the map, the kernel
  watches stay until `closeWatch`.

`queueIgnored` creates a pipe and then installs the handler that closes it.
An async exception between the two leaks the two fds of the pipe.

`addPathUsing` adds the kernel watch and then inserts the path in the map. An
async exception between the two leaves a kernel watch that is not in the map.
Its events are dropped, and a later add of the same directory finds it.

The event stream makes these calls with async exceptions masked, only the
calls from `removeRoot` and `addRoot` are affected.

Proposed change: run the body of `unwatchPaths` and of `addPathUsing` with
async exceptions masked. They do not block, the lock is already held and the
system calls do not wait.

## Wait registration left in the IO manager

Threaded RTS only. `readEvents` registers the wait for the fd with
`threadWaitReadSTM`. The wait ends when the fd is readable or when a
synthetic event is recorded. The registration is removed only if the wait
raises an exception. When the wait ends because of a synthetic event, for
example, after a `removeRoot` without `/proc`, or after an `addRoot`
that reports a denied subdirectory, the registration stays in the IO manager
until the fd becomes readable or is closed. Each such wake up leaves one more
registration.

Proposed change: call the unregister action whenever the wait ends. It does
nothing for a registration that has fired.

## closeWatch does not end the stream at once

After `closeWatch` the stream ends when it reaches the next read of the fd.
The events already read from the fd, and the entries of a scan in progress,
are still reported. The haddock of `closeWatch` says that the event streams
of the watch end.

If no stream reads the watch after `closeWatch`, the directories of a scan in
progress stay open until the `Watch` is garbage collected.

In the non-threaded RTS without `/proc`, `closeWatch` cannot queue the event
that wakes up a waiting thread, and the fd stays open until the next event
arrives. `inotify_rm_watch` on a watch descriptor of the map queues an
`IN_IGNORED` event without using `/proc`.

Proposed change: `eventStream` ends the stream if the watch is closed when
a step starts, and `closeWatch` removes one watch descriptor of the map when
it cannot queue a marker.
