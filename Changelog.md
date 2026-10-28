# Changelog

## Unreleased

- Add the platform specific modules `Streamly.FileSystem.Event.Darwin`,
  `Streamly.FileSystem.Event.Linux` and `Streamly.FileSystem.Event.Windows`.
  Earlier the platform specific APIs were available only from the internal
  modules.
- Add `Config`, `setRecursiveMode`, `watchWith`, `watchRecursive`, `getRoot`
  and `getRelPath` to the portable module `Streamly.FileSystem.Event`.
- Implement the non-recursive `watch` on macOS, it raised an exception
  earlier.
- Add `setRecursiveMode` to the macOS `Config`. **Breaking:** `watchWith` on
  macOS is now non-recursive by default, consistent with Linux and Windows,
  use `setRecursiveMode True` for the earlier behavior.
- **Breaking:** `Streamly.FileSystem.Event.watch` on macOS is now
  non-recursive, consistent with Linux and Windows, it watched recursively
  earlier.
- macOS: `setFullHistory`, `isCloned`, `isHardLink` and `isLastHardLink` are
  always exported. When built with an SDK that does not have the
  corresponding flag, the setter is a no-op and the predicates return
  `False`. `setFullHistory` failed to link earlier.
- Linux: the `FailIfExists` constructor of `WhenExists` is always available.
- macOS: add `getRoot` and `getRelPath`.
- **Breaking:** macOS: when a watch root is a symbolic link, `getAbsPath`
  returns the path via the original symbolic link, consistent with Linux and
  Windows. It returned the path via the resolved target earlier.
- Linux: export the `Cookie` type returned by `getCookie`.
- **Breaking:** Windows: `getRelPath` returns `Maybe Path`, consistent with
  Linux.
- Linux: fix `setWhenExists`, it set both `IN_MASK_ADD` and `IN_MASK_CREATE`
  for `FailIfExists` after `defaultConfig`, adding a watch then failed.
- Linux: report the queue overflow (`isEventsLost`) event once for each
  watch root. `getRoot` and `getAbsPath` raised an exception on this event
  because the kernel reports it without a watch descriptor.

## 0.1.1

- Fix a build issue due to configure script on macOS + nix
- Allow GHC-9.14

## 0.1.0

- Initial release
