# FreeBSD backend: design

## Decision

The FreeBSD backend uses the native inotify(2) system calls. These calls
are in FreeBSD 14.5, 15.0 and later releases. The Linux backend
(`Streamly.Internal.FS.Event.Linux`) is compiled on FreeBSD as well, with
`CABAL_OS_FREEBSD` conditionals for the few kernel differences. The new
modules `Streamly.FileSystem.Event.FreeBSD` and
`Streamly.Internal.FS.Event.FreeBSD` re-export it. The public API, the
defaults and the event predicates are the same as on Linux.

FreeBSD 14.0 to 14.4 and 13.x have no inotify. The package does not build
on these releases, and the portable module raises a compile time error
that names the required release.

## The existing backends

### Backend selection

* `streamly-fsevents.cabal` defines `CABAL_OS_LINUX`, `CABAL_OS_DARWIN`
  and `CABAL_OS_WINDOWS` with `if os(...)` in the `compile-options` stanza,
  and lists the platform modules under `if os(...)`.
* `Streamly.Internal.FS.Event` imports `Config`, `Event` and the portable
  functions from the platform module selected by these macros, and has an
  `#error` for other platforms.
* `configure` (autoconf) writes `src/config.h`. On Linux it defines
  `HAVE_DECL_IN_EXCL_UNLINK` and `HAVE_DECL_IN_MASK_CREATE`. When
  `HAVE_DECL_IN_EXCL_UNLINK` is 0 the Linux modules compile to empty
  modules with a `#warning`.
* Each platform has a test-suite that is buildable only on that platform.
  The portable test-suite `FileSystem.Event` is buildable on Linux with
  GHC 9.4 or later, macOS and Windows.

### The contract of a backend

The portable module requires from each backend:

* `Config`, `setRecursiveMode`, `watch`, `watchRecursive` and
  `watchWith :: (Config -> Config) -> NonEmpty Path -> Stream IO Event`.
* `Event`, `getRoot`, `getRelPath :: Event -> Maybe Path` (`Nothing` for
  the watch root itself), `getAbsPath`, `isCreated`, `isDeleted`,
  `isMoved`, `isModified`, `isEventsLost` and `showEvent`.
* The default configuration reports create, delete, move and modify
  events, and is not recursive.
* Paths are streamly `Path` values. `getAbsPath` joins the root, as given
  by the caller, with the relative path. For a root that is a symbolic
  link, the path goes through the link.
* Watching a path that does not exist raises an exception.
* The watch is opened and closed with `bracketIO`: the resources are
  released when the stream ends, when the consumer raises an exception,
  or at garbage collection when the stream is abandoned.
* Events are reported in the order the kernel queues them. Identical
  successive events may be coalesced into one.
* When the kernel drops events, `isEventsLost` is reported once per watch
  root.

The Linux module also exports the low level API `openWatch`,
`closeWatch`, `watchToStream`, `addToWatch` and `removeFromWatch`. Its
behaviour is specified by the Linux test-suite: removing a root reports
the queued events and then `isRootUnwatched`; roots that share a
directory share one watch descriptor; recursive mode adds a directory on
`IN_CREATE` and `IN_MOVED_TO`, removes it on `IN_MOVED_FROM`, and scans
the tree again after `IN_Q_OVERFLOW`; `closeWatch` from another thread
ends the stream.

The Linux module queues a private marker event to report
`isRootUnwatched` in order for a root that shares a watch descriptor with
another root. The marker is an inotify watch on a private pipe, added
through `/proc/self/fd/N` and removed at once; its `IN_IGNORED` event
follows the events already queued.

## FreeBSD mechanisms compared

* inotify(2): in FreeBSD 14.5, 15.0 and later. Events carry the name
  of the changed entry and a cookie that pairs the two halves of a
  rename. One fd per watch, no fd per watched object. Recursion in user
  space, as on Linux. Almost all the code is shared with the Linux
  backend. No dependency.
* kqueue `EVFILT_VNODE`: in all releases. Events do not carry entry
  names; a directory must be read again and compared with its previous
  listing. One open fd per watched file and directory. Recursion in user
  space. No rename pairing. A new backend, no code shared with Linux. No
  dependency.
* libinotify-kqueue (port `devel/libinotify`): in all releases. Emulates
  the inotify API with kqueue, with one fd per watched object inside the
  library and names found by reading directories again. Almost all the
  code is shared with Linux. A third party C library.

kqueue reports `NOTE_WRITE`, `NOTE_DELETE`, `NOTE_RENAME`, `NOTE_ATTRIB`,
`NOTE_EXTEND`, `NOTE_LINK` and `NOTE_REVOKE` for an open fd. A directory
reports `NOTE_WRITE` when an entry is added or removed. To learn which
entry, the backend must keep a listing of each watched directory and
compare it with a new listing. A rename reports `NOTE_RENAME` on the
renamed object, but the new name is not reported. To report `isModified`
for a file, the backend must open every file in every watched directory.
A tree of 10^5 files then needs 10^5 open fds, counted against
`kern.maxfilesperproc` and `kern.maxfiles`. Between a change and the
comparison of the listings, further changes are merged or lost.

libinotify-kqueue implements the inotify API with kqueue, with the same
fd usage and the same races. It is a third party C library, a hard
dependency of the package. It is rejected.

The FreeBSD inotify man page states that the calls "aim to be compatible
with the Linux inotify interface", and that, unlike `EVFILT_VNODE`, they
watch a directory "without needing to open each object in that
directory".

## inotify on FreeBSD

### Availability

Checked in the FreeBSD source repository (`sys/sys/inotify.h` and
`sys/kern/vfs_inotify.c`):

| Branch | inotify | `IN_IGNORED` without `IN_DELETE_SELF` |
|-|-|-|
| releng/14.4 and earlier, 13.x | no | |
| releng/14.5, stable/14 | yes | fixed |
| releng/15.0, releng/15.1 | yes | missing |
| stable/15, main | yes | fixed |

### The same as on Linux

* Header `sys/inotify.h`, the functions `inotify_init1`,
  `inotify_add_watch` and `inotify_rm_watch`.
* `struct inotify_event` has the Linux layout: `int wd`, `uint32_t mask`,
  `uint32_t cookie`, `uint32_t len`, then the name padded with NUL bytes
  to the alignment of the struct, 4 bytes.
* All the event and flag bits used by the Linux module, with the Linux
  values: `IN_ACCESS` to `IN_MOVE_SELF`, `IN_UNMOUNT`, `IN_Q_OVERFLOW`,
  `IN_IGNORED`, `IN_ISDIR`, `IN_ONLYDIR`, `IN_DONT_FOLLOW`,
  `IN_EXCL_UNLINK`, `IN_MASK_CREATE`, `IN_MASK_ADD`, `IN_ONESHOT`,
  `IN_CLOEXEC`, `IN_NONBLOCK`.
* A read returns as many whole events as fit in the buffer; a buffer
  smaller than one event fails with `EINVAL`.
* One watch descriptor per vnode per inotify fd. Adding a watched vnode
  again updates the existing watch, or fails with `EEXIST` with
  `IN_MASK_CREATE`. Watch descriptors are not reused while possible.
* `inotify_rm_watch` queues `IN_IGNORED` after the pending events of the
  watch and keeps them; an unknown wd fails with `EINVAL`.
* The queue limit is `vfs.inotify.max_queued_events`, 16384 events; the
  excess is replaced by one `IN_Q_OVERFLOW` event with wd -1.
* Identical successive events are coalesced.
* The inotify fd supports `poll`, `select` and `kevent` `EVFILT_READ`. The
  GHC IO manager on FreeBSD uses kqueue, `threadWaitRead` and
  `closeFdWith` work as on Linux.

### Differences, and what the backend does

1. **`IN_IGNORED` on deletion, FreeBSD 15.0 and 15.1.** When a watched
   object is deleted, these releases queue `IN_IGNORED` and remove the
   watch only if the mask of the watch has `IN_DELETE_SELF`. The default
   configuration does not set it (`setRootDeleted False`). Without
   `IN_IGNORED`, a deleted root never reports `isRootUnwatched`, and in
   recursive mode a deleted subdirectory stays in the watch descriptor
   map and keeps its vnode referenced until `closeWatch`.

   The backend always adds `IN_DELETE_SELF` to the kernel mask on
   FreeBSD and drops the `IN_DELETE_SELF` events the configuration did not
   request. The recursive mode already does the same for `IN_CREATE`,
   `IN_MOVED_FROM` and `IN_MOVED_TO`. No API change.

2. **No `/proc/self/fd`.** FreeBSD has no `/proc/self/fd`, and a pipe is
   not a vnode, so it cannot be watched. The marker uses a directory
   instead: the backend adds a watch on `/` with `IN_MASK_CREATE` and
   removes it at once. `IN_MASK_CREATE` makes the add fail with `EEXIST`
   if the same inotify fd already watches `/`; the marker would otherwise
   get, and remove, the watch descriptor of the user's root. On `EEXIST`
   the backend tries `/dev`. The add and the remove are made with the lock
   of the watch held, no user add can come in between. The events of `/`
   queued between the add and the remove have the marker's wd and are
   dropped, as on Linux.

   The marker is needed when `removeFromWatch` removes a root that shares
   a directory with another root, and after `IN_Q_OVERFLOW`. If both `/`
   and `/dev` are watched by the same `Watch`, for example by a recursive
   watch of `/`, the marker fails: `removeFromWatch` on a shared root
   raises an exception and can be retried, as on Linux when `/proc` is not
   mounted.

   Side effects: adding the watch reads `/` and looks up each entry. The
   read generates `IN_ACCESS` on `/`, and on main the kernel also opens and
   closes `/` to read it. A watch of `/` in another process with
   `setAccessed`, `setOpened` or `setNonWriteClosed` enabled may see these
   events.

3. **`IN_EXCL_UNLINK` has no effect.** FreeBSD never reports the events
   of an unlinked file, as if `IN_EXCL_UNLINK` were always set. The default
   `setExcludeUnlinked True` behaves as on Linux; `setExcludeUnlinked False`
   has no effect on FreeBSD. Documented.

4. **Link count changes.** On FreeBSD, `unlink`, `link` and a rename over
   an existing file report `IN_ATTRIB` only to a watch on the file itself,
   not to a watch on its directory, and `rmdir` reports no `IN_ATTRIB`. On
   Linux the directory watch reports `isAttrsModified` for the entry.
   Documented.

5. **Hard links.** An access through any hard link of a file generates
   an event in a watched directory that has a link to the file, even if
   the access used a link in an unwatched directory. A watched directory
   with two links to one file reports each event twice, once per name.
   From the BUGS section of the man page. Documented.

6. **Adding a directory watch reads the directory.** `inotify_add_watch`
   on a directory reads all its entries and looks up each one, to fill
   the name cache. The cost is proportional to the number of entries. The
   read generates `IN_ACCESS` for the directory, reported by a watch on its
   parent. Two failures follow:

   * An entry deleted between the read and the lookup fails the add with
     `ENOENT`, although the directory exists. The backend retries the add
     up to 10 times on `ENOENT`. A path that does not exist fails all the
     retries; a retry costs one failed lookup. In recursive mode, a
     subdirectory that failed with `ENOENT` would otherwise be skipped
     silently as deleted.
   * A directory that is readable but not searchable (mode `r--`) fails
     with `EACCES`. Linux watches it. Documented.

   On FreeBSD `inotify_add_watch` is imported as a `safe` call, so that a
   large directory does not block the other Haskell threads of the
   capability. Linux keeps the `unsafe` call.

7. **File systems without a name cache.** devfs delivers no events
   (main). tmpfs mounted with `nonc` and other file systems that
   bypass the name cache may miss events. Documented.

8. **Limits.** `vfs.inotify.max_user_instances` (256) limits the inotify
   fds per user, Linux has 128; `vfs.inotify.max_user_watches` (one third
   of `kern.maxvnodes` at boot) limits the watched objects per user.
   Exceeding the second fails with `ENOSPC`, as on Linux. Each watch keeps
   a reference to its vnode.

## Event mapping

The `Event` type and all the predicates are those of the Linux module;
each predicate tests the same `IN_*` bit. The portable events:

| Portable | inotify bit | FreeBSD source of the event |
|-|-|-|
| `isCreated` | `IN_CREATE` | open, mkdir, link, symlink, mknod, bind |
| `isDeleted` | `IN_DELETE` | unlink, rmdir |
| `isMoved` | `IN_MOVED_FROM`, `IN_MOVED_TO` | rename, with a cookie |
| `isModified` | `IN_MODIFY` | write, truncate, fallocate, copy |
| `isEventsLost` | `IN_Q_OVERFLOW` | queue full or allocation failure |

The Linux specific predicates (`isRootDeleted`, `isRootMoved`,
`isRootUnwatched`, `isRootUnmounted`, `isAttrsModified`, `isAccessed`,
`isOpened`, `isWriteClosed`, `isNonWriteClosed`, `isDir`, `getCookie`) are
exported from the FreeBSD module with the differences 3 to 5 above.
`isRootUnmounted` is generated when the file system of the watched object
is unmounted or the vnode is revoked.

## Code organisation

* `src/Streamly/Internal/FS/Event/Linux.hs`: `CABAL_OS_FREEBSD` blocks
  for differences 1, 2 and 6.
* `src/Streamly/Internal/FS/Event/FreeBSD.hs`: new, re-exports the Linux
  module, FreeBSD documentation.
* `src/Streamly/FileSystem/Event/FreeBSD.hs`: new, the released API, the
  export list of `Streamly.FileSystem.Event.Linux`.
* `src/Streamly/Internal/FS/Event.hs`: import the FreeBSD module; a
  compile time error on FreeBSD without inotify.
* `src/Streamly/FileSystem/Event.hs`: documentation.
* `streamly-fsevents.cabal`: `os(freebsd)` conditionals; the Linux
  internal module is an other-module on FreeBSD.
* `configure.ac`: no change, the inotify checks already run on all
  platforms.
* `test/Streamly/Test/FileSystem/Event/Linux.hs` and `Common.hs`: build
  on FreeBSD, with FreeBSD replacements for the `/proc` helpers and for
  the expected event lists.
* README and Changelog: platform list, FreeBSD entry.

The FreeBSD module re-exports the Linux module instead of copying it. A
copy would duplicate 1700 lines and every later fix. The alternative,
renaming the Linux module to a shared `Inotify` module with two re-export
modules, moves all the Linux code in the diff; it is not chosen. The
Linux module stays hidden on FreeBSD (`other-modules`), so a FreeBSD
user sees only the FreeBSD modules.

## Recursion

The same as on Linux: a watch per directory, added by a scan when a root
is added and on `IN_CREATE` or `IN_MOVED_TO` with `IN_ISDIR`, removed on
`IN_MOVED_FROM` with `IN_ISDIR` and on `IN_IGNORED`, and a scan of all the
watched directories after `IN_Q_OVERFLOW`. A directory is added before its
entries are scanned, so a directory created during the scan is added by
the scan or by its create event. Events in a new directory before its
watch is added are lost, as on Linux.

## Resources, cancellation, cleanup

The same as on Linux:

* One inotify fd per `Watch`, created with `IN_CLOEXEC | IN_NONBLOCK`.
  `openWatch` closes it if adding a root fails.
* The marker needs no fd on FreeBSD; on Linux it needs a pipe.
* `watchWith` uses `bracketIO`: the fd is closed when the stream ends, on
  an exception, or at garbage collection of an abandoned stream.
* The reading thread waits for the fd with the GHC IO manager.
  `closeWatch` from another thread closes the fd with `closeFdWith`, which
  wakes the waiting thread; the stream ends. With the non-threaded RTS the
  fd is closed by the last waiting thread.
* Closing the inotify fd removes all its watches in the kernel and frees
  the queued events.

## FFI

* `foreign import capi "sys/inotify.h value IN_..."` for the constants,
  as on Linux; the values are checked by the C compiler.
* `foreign import ccall unsafe` for `inotify_init1`, `inotify_rm_watch`,
  `read` (on a non-blocking fd) and `close`. `inotify_add_watch` is
  `safe` on FreeBSD.
* `struct inotify_event` is read with `peekByteOff` at offsets 0, 4, 8 and
  12, the same on both systems.
* On FreeBSD 15 the system calls are in `libsys`, which `libc` links;
  no `extra-libraries` entry.

## Decisions

Decided in review:

1. FreeBSD 14.4 and earlier are not supported. A kqueue backend for them
   would be a separate backend with weaker semantics: no entry names, one
   fd per watched file.
2. `setExcludeUnlinked False` has no effect on FreeBSD, documented.
3. The marker watches `/`, then `/dev`. The rejected alternative is a
   private directory created with `mkdtemp`, removed, and watched through
   an open fd with `inotify_add_watch_at(fd, dirfd, ".")`. It cannot
   collide with a user root, but it creates and deletes a directory in
   `$TMPDIR`, and a lookup of `.` in a removed directory is not verified on
   ZFS and tmpfs.
4. The FreeBSD modules re-export the Linux module.

## Sources

* FreeBSD inotify(2) man page, `lib/libsys/inotify.2` in
  <https://github.com/freebsd/freebsd-src>, branches releng/15.0 and main.
* `sys/sys/inotify.h`, `sys/kern/vfs_inotify.c`, `sys/kern/vfs_subr.c`
  and `sys/kern/vfs_cache.c` in the same repository.
* FreeBSD 15.0 announcement, and "FreeBSD 15.0: Fixes and Features",
  FreeBSD Journal.
