# Changelog

## Unreleased

- Implement the non-recursive `watch` on macOS, it raised an exception
  earlier.
- Add `setRecursiveMode` to the macOS `Config`. **Breaking:** `watchWith` on
  macOS is now non-recursive by default, consistent with Linux and Windows,
  use `setRecursiveMode True` for the earlier behavior.

## 0.1.1

- Fix a build issue due to configure script on macOS + nix
- Allow GHC-9.14

## 0.1.0

- Initial release
