# Linux TODO

## API design

- **`watchToStream` takes a `Config`** that must match the one given to
  `openWatch`; a mismatch silently changes recursion and filtering. Store the
  `Config` in `Watch`: `watchToStream :: Watch -> Stream IO Event`.
- **`addToWatch` takes its own `Config`.** The kernel keeps one mask per
  inode, so with `ReplaceIfExists` a later add on a shared directory replaces
  the mask for every root on it.
- **`addToWatch` takes `PathRep`**, an unexported internal tuple
  `(Path, Maybe Path)`. Use `addToWatch :: Watch -> Path -> IO ()`.
- **`removeFromWatch` on an unwatched path** silently does nothing. Return a
  `Bool` or raise an exception.
- **`getCookie` returns a `Cookie` for every event**, not only moves (see the
  existing "XXX Maybe" note).
- **`isRootUnwatched` gives no reason**: user removal, deletion or unmount.
- **Recursive mode raises an exception and ends the stream** on
  `EACCES`/`ENOSPC` from one subdirectory. Add an option to skip such
  directories, like `ignoreInaccessible` in streamly-core.
- **`setFollowSymLinks False`** with a symlinked root in recursive mode still
  scans the symlink's target directory.

## Documentation / user pitfalls

- **Abandoned streams.** `watch`/`watchWith` use `bracketIO`, so a stream cut
  short (by `take`, or an exception in the consumer) closes its fd only at
  GC. A loop can exhaust `max_user_instances`.
- **Lost events on re-read.** Events read from the kernel but not consumed
  (up to 32 KiB per read) are lost when a stream is abandoned; a second
  `watchToStream` starts after them. The tests' `readUntil` relies on this.
- **Two readers on one `Watch`** split the events and both modify the shared
  state.
- **`ENOSPC`** means `max_user_watches` was reached, but the message says
  "No space left on device".
- **Moved recursive root** keeps reporting its old path. New subdirectories
  are then looked up by the old path, which may now name an unrelated
  directory. Found by reading the code, not tested.
- **Deep or large trees:**
  - paths longer than `PATH_MAX` fail with `ENAMETOOLONG`;
  - the queue can overflow during the initial scan;
  - every directory move scans all watched paths (`isInSubtree`).
- **Non-threaded RTS:** `closeWatch` takes effect only at the next event.
- **Hung network/FUSE mounts.** `inotify_add_watch` is an `unsafe` FFI call,
  so on such a mount it blocks a capability while holding the watch lock.
- **Removing a root that shares a directory** with another root needs
  `/proc` mounted and a free fd.

## Suspected bugs (not verified)

- **Overflow drops `IN_IGNORED`.** If the kernel removes a watch (directory
  deleted, filesystem unmounted) and its `IN_IGNORED` is lost in an
  overflow, the entry stays in the map. Memory leak only, no wrong events.
- **Overflow drops a move.** A subdirectory moved out of the tree whose move
  event was lost stays watched under its old path. Documented in
  `isEventsLost`, not fixed.
- **`addToWatch` from another thread.** Events for a new watch descriptor
  read before its map insert used to be dropped. The lock from `edf96bb`
  should prevent this, since reads happen under the same lock. Not tested.
- **Non-threaded RTS fd limit.** That RTS waits on fds with `select`, which
  may fail for fds >= 1024. Pre-existing, not tested.
