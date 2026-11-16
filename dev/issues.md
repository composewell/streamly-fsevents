# Issues

## Darwin: use unsafe FFI calls for the constant getters

`src/Streamly/Internal/FS/Event/Darwin.hs` imports the FSEvents flag
constants, e.g. `kFSEventStreamCreateFlagNoDefer` and the
`FSEventStreamEventFlag*` functions, with `foreign import ccall safe`.
These C functions only return a constant, they do not block and do not
call back into Haskell. A `safe` call costs more than an `unsafe` call
in both the threaded and the non-threaded RTS. Import them with
`unsafe`.

## CI: run the non-threaded RTS tests on macOS

The `non-threaded` job in `.github/workflows/haskell.yml` builds with
`--flag -threaded` on `ubuntu-latest` only. The Darwin backend also has
code specific to the non-threaded RTS: `closeWatch` does not close the
read fd while a thread waits for it, because `select` in the
non-threaded RTS fails with `EBADF` on a closed fd and the RTS
terminates the program. No CI job tests this code. Add a `non-threaded`
job on `macos-latest`.
