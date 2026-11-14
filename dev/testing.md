# Tests

## Portable tests

`test/Streamly/Test/FileSystem/Event.hs`, the test suite `FileSystem.Event`,
uses only `Streamly.Internal.FS.Event` and expects the same events on Linux,
macOS and Windows. A thread reads the stream of a watch into a channel. A watch
is known to have started only after it reports an event, so the test creates a
file `startN` in each root until the watch reports one in every root.

The recursive mode tests make a sequence of changes to the tree without reading
the events in between, for example, a new directory renamed twice. They then
create files in the tree and check that each file reports `isCreated` once,
and that no change is reported under a path that no longer exists. On macOS a
later event for a path can repeat `isCreated`, there the check is that each
file reports at least one `isCreated`. Symbolic link cycles, and a directory
renamed over an empty directory, are not tested on Windows.

`test/Streamly/Test/FileSystem/Event/Linux.hs` keeps the tests of the Linux
API that the other platforms do not have: the scan events, `isRootUnwatched`,
the handle API of a watch, file descriptor limits and the inotify queue
overflow. A root listed twice and nested roots are also tested on Linux only,
macOS reports an event once, for the innermost root that contains the path,
Linux and Windows report it once for each root.

The large tree and stress tests run on all platforms. The exhaustive test of
the recursive watch on Linux is described in `dev/design.linux.md`.

## Large tree test

`test/Streamly/Test/FileSystem/Event/LargeTree.hs` watches the largest tree
that the free inotify watches, inodes and space allow, up to 100000
directories, using at most half of the free inodes and space. It moves half
of the tree out of the watched tree and back in, and renames it within the
tree and back, three times. It checks that each move reports every
directory moved in, that a file created in every directory at the end is
reported once, and that no events are lost. It prints the start time of the
watch, the peak RSS of each step and, with `+RTS -T`, the live heap.

The test runs on all platforms. The inotify watch limit, the check of the
directories reported by the scan of a move in, and the peak RSS apply to
Linux only.

The times and memory in `README.md` and `docs/robustness.md` are from this
test. The tree is created in a temporary directory under `TMPDIR`, which is
`/tmp` by default. Set `TMPDIR` to a directory on the file system to be
measured, for example, on a machine where `/tmp` is tmpfs:

```
B=$(cabal list-bin test:FileSystem.Event.LargeTree)
D=$PWD/dist-newstyle/largetree-tmp; mkdir -p $D
$B +RTS -T -RTS                      # tmpfs
TMPDIR=$D $B +RTS -T -RTS            # the file system of the repo
TMPDIR=$D $B +RTS -T -G1 -A1m -RTS   # memory
```

The times are the average of three runs with the default GC settings. The
memory is from a run with `-G1 -A1m`, in which every GC is a major GC, so the
maximum residency includes short peaks. The live heap of the watch is the
`live` value printed after a step minus the `live` value printed for
`before the watch`, which is the heap of the test's own data.

## Stress tests

`test/Streamly/Test/FileSystem/Event/Stress.hs` runs a recursive watch under
a high rate of events, on all platforms:

* An overflow at the start of a watch and after a burst of file creations.
  On Linux the stream ends after an overflow, and the consumer of every
  stress test opens a new watch, see `startWatch` in
  `test/Streamly/Test/FileSystem/Event/Tree.hs`.
* A tree of fanout 4, with 2 files in each directory, created without delays
  while the consumer reads, and with the consumer blocked after a quarter of
  the tree followed by a burst that overflows the queue.
* Changes to a tree of 2000 directories during an overflow: a new subtree, a
  rename within the tree, a move to another parent, a move out, a move in, a
  deletion, and new directories in 20 places.
* A burst that overflows the queue, followed by 10 seconds of changes while
  the consumer reads, on a tree of 10000 directories: files created and
  deleted in the root, a subtree created and deleted, a rename of a subtree
  and back, and a move of a subtree out and back in. The changes while a new
  watch adds the tree cause another overflow, 13 in one run on Linux.

After the activity stops and the `isEventsLost` events have stopped for 2
seconds, every test creates a file in every directory of the tree and checks
that each is reported once. The number of overflows is asserted on Linux
only. The tests use at most a tenth of the free inodes and space, and on
Linux half of the free inotify watches. All the test suites run with a heap
limit of 1 GB, `-M1G`. Without profiling libraries, the
heap of a test can be examined with `+RTS -hT`, and with the build option
`-finfo-table-map -fdistinct-constructor-tables` and `+RTS -hi`.

