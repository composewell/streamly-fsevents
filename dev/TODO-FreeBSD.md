# FreeBSD TODO

The FreeBSD backend has not been built or run on FreeBSD. It was built on
Linux in two configurations: the normal Linux build, and a build where
cabal treats Linux as FreeBSD (`CABAL_OS_FREEBSD` defined, the FreeBSD
modules and the FreeBSD test-suite built, the FreeBSD code paths run
against Linux inotify). See `dev/FreeBSD-design.md` for the design.

## Testing on FreeBSD

FreeBSD 14.5, 15.0 or later is required. On 14.4 and earlier the build
must stop with the error "streamly-fsevents requires inotify".

```
pkg install -y ghc hs-cabal-install git
git clone -b freebsd-impl https://github.com/composewell/streamly-fsevents
cd streamly-fsevents
cabal update
cabal build --flags=limit-build-mem --enable-tests all
cabal test --flags=limit-build-mem --test-show-details=direct \
    --test-options='+RTS -M1g -RTS' \
    FileSystem.Event FileSystem.Event.FreeBSD
```

Check after the build:

1. `src/config.h` in the build directory defines
   `HAVE_DECL_IN_EXCL_UNLINK` as 1.
2. `sysctl vfs.inotify` lists `max_queued_events`, `max_user_instances`
   and `max_user_watches`.
3. Run the tests on UFS, ZFS and tmpfs (`TMPDIR` on each), on 15.0 or 15.1
   and on 14.5 or stable/15. 15.0 and 15.1 have the `IN_IGNORED` bug that
   `alwaysEvents` works around; the test "reports isRootUnwatched for a
   deleted root" must fail on them with `alwaysEvents = 0`.
4. Run the FreeBSD test-suite about 20 times to find flaky tests.
5. Build with the non-threaded RTS and run the concurrency tests.

## CI

A GitHub Actions job with vmactions/freebsd-vm. Check the release names the
action supports.

```yaml
  freebsd:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: vmactions/freebsd-vm@v1
        with:
          release: "15.0"
          usesh: true
          mem: 8192
          prepare: |
            pkg install -y ghc hs-cabal-install git
          run: |
            cabal update
            cabal build --flags=limit-build-mem --enable-tests all
            cabal test --flags=limit-build-mem --test-show-details=direct \
              --test-options='+RTS -M1g -RTS' \
              FileSystem.Event FileSystem.Event.FreeBSD
```

## Not verified

- **Linking.** The inotify functions are in `libsys` on FreeBSD 15 and in
  `libc` on 14.5. GHC links `libc`, which is expected to pull in `libsys`.
- **Expected events.** The FreeBSD test-suite expects the events listed in
  the Linux test-suite, except `isAttrsModified` on rmdir. The kernel
  source shows `IN_OPEN`, `IN_ACCESS` and `IN_CLOSE_NOWRITE` when a
  directory is read, reported by a watch on its parent through the name
  cache; this has not been observed.
- **Spurious `ENOENT`.** The retry of `inotify_add_watch` on `ENOENT` is
  from reading `vn_inotify_add_watch`. A test would need a directory whose
  entries are deleted while the watch is added.
- **The `/dev` marker.** In a jail without devfs, `/dev` may be missing;
  the marker then fails when `/` is watched by the same watch.
- **GHC IO manager.** The inotify fd supports kqueue `EVFILT_READ`, which
  the threaded RTS uses. `closeFdWith` waking a waiting thread is not
  tested on FreeBSD.
- **streamly-core.** Its directory reading uses `dirent` and `d_type`
  through hsc2hs, which FreeBSD has; not built on FreeBSD here.
- **Queue overflow tests.** They create `max_queued_events + 1000` files,
  17384 with the FreeBSD default.
- **The marker test** ("removeFromWatch can be retried after it fails")
  watches `/` and `/dev` in the test process. The events of `/` are read
  and dropped by the test.

## Possible follow-ups

- A kqueue backend for FreeBSD 14.4 and earlier, with weaker semantics.
- Add the CI job above to `.github/workflows/haskell.yml`.
- `inotify_add_watch_at` could watch a directory through an open fd, for
  a marker that cannot collide with a user root.
