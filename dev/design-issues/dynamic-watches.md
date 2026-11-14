# Adding and removing watch roots dynamically

Not implemented. This note describes a portable API to add and remove watch
roots while the events are being read, and how it can be implemented on each
platform.

## API

A handle based API. The roots are added and removed using the handle, from
any thread, while other threads read the events from the handle as a stream.
On Linux these functions exist in `Streamly.Internal.FS.Event.Linux`:

```haskell
openWatch :: Config -> NonEmpty Path -> IO Watch
addRoot :: Watch -> Path -> IO ()
removeRoot :: Watch -> Path -> IO ()
readEvent :: Watch -> IO (Maybe Event)
eventStream :: Watch -> Stream IO Event
closeWatch :: Watch -> IO ()
```

A stream on the handle can stop reading before the watch is closed, the next
stream starts with the first event not yet read. A stream of commands can be
built on top of the handle:

```haskell
data Command = AddRoot Path | RemoveRoot Path

watchDynamic :: (Config -> Config) -> Stream IO Command -> Stream IO Event
```

## Requirements

* No command waits for an event. Removing a root, or ending the stream, must
  wake up a thread blocked waiting for the events of that root.
* A removed root reports the events already queued for it, followed by an end
  of watch event, like `isRootUnwatched` on Linux, on all platforms.

## Linux

One inotify fd for all the roots. `AddRoot` calls `addRoot` and
`RemoveRoot` calls `removeRoot`, both can be called while another thread
reads the events of the fd.

An inotify fd per root, with the streams of the roots merged, is not used:

* `max_user_instances`, 128 by default, is per user, shared by all the
  processes of the user.
* `IN_MOVED_FROM` and `IN_MOVED_TO` of a move between two roots arrive on
  different fds, they cannot be paired by the cookie.
* The events of different roots lose their relative order.

## Windows

One directory handle per root, each read by `ReadDirectoryChangesW` in its own
thread. The streams of the roots are merged using `parConcatMap` over the
stream of added roots.

`killThread` cannot interrupt a synchronous `ReadDirectoryChangesW`. Removing a
root needs `CancelIoEx` on the handle before `CloseHandle`, or overlapped IO.

## macOS

An `FSEventStream` watches a fixed list of paths. Two options:

* One `FSEventStream` per root, all on the existing dispatch queue and writing
  to the same pipe. `RemoveRoot` stops and invalidates the stream of the root.
* One `FSEventStream` for all the roots, recreated with the new list of paths.
  The new stream starts from the last event id received (`sinceWhen`), so that
  no event is lost. The duplicate events are dropped by event id.

## Other libraries

All of these support adding and removing watches while the events are read.
The details below are from memory, verify them against the sources before
relying on them.

| Library | Windows | macOS |
|---|---|---|
| Rust `notify` | a thread per path | one `FSEventStream`, stopped and recreated on watch and unwatch |
| libuv (Node `fs.watch`) | a handle per watch | one shared `FSEventStream`, recreated when the watches change |
| Python `watchdog` | a thread per watch | an `FSEventStream` per watch |
| Go `fsnotify` | `ReadDirectoryChangesW` with an IO completion port | kqueue instead of FSEvents, an fd per watched file |
| Java `WatchService` | a background thread | polling |
