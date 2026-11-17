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
overflow. A root listed twice and nested roots are also tested on Linux only.
All platforms report a change in a root listed twice once, and a change in
nested roots once for each root, the test suite `FileSystem.Event.Behavior`
checks both.

The large tree and stress tests run on all platforms. The exhaustive test of
the recursive watch on Linux is described in `dev/design.linux.md`.

## Large tree test

`test/Streamly/Test/FileSystem/Event/LargeTree.hs` watches a tree of 20000
directories, 10 subdirectories in each directory, on all platforms, so that
the times measured on different platforms are for the same work. Half of the
tree, 10000 directories, is below the 14336 that the default
`max_queued_events` of 16384 allows on Linux. `--dirs N,M` or the environment
variable `LARGETREE_DIRS=N,M` runs the test on a tree of each size in turn. A
size is skipped if the free inotify watches, or half of the free inodes and
space, do not allow it.

The test moves half of the tree out of the watched tree and back in, and
renames it within the tree and back, three times. The half moved includes a
chain of 5 directories with paths longer than 260 characters. It then
renames an empty directory 2000 times, in batches of 500 that fit the 63 KB
event buffer of a watch root on Windows. It checks that each move out and in
is reported, that a file created in every directory at the end is reported
once, and that no events are lost. On Linux it also checks that the scan of
a move in reports every directory moved in, and prints the peak RSS of each
step.

After each size the test prints the OS, the number of CPUs, the GHC version,
the GC options and the tree, followed by two markdown tables:

* Times: the second listing of the tree, with warm caches, the start of the
  watch, the mean and range of the 3 moves in and of the 6 renames of 10000
  directories, and the 2000 renames of an empty directory with the rate.
* Memory, with `+RTS -T`: the live heap after the start of the watch and
  after each cycle of moves and renames, minus the live heap before the start,
  which is the data of the test, and the maximum live heap of the process
  before the watch and at the end of the renames. The maximum is of the
  process, when several sizes run, it includes the earlier sizes.

The tables in `README.md` and `docs/robustness.md` are from this test, a row
can be copied from its output. `--label TEXT` sets the first column of the
tables, the platform name by default. The tree is created in a temporary
directory under `TMPDIR`, which is `/tmp` by default. Set `TMPDIR` to a
directory on the file system to be measured, for example, on a machine where
`/tmp` is tmpfs:

```
B=$(cabal list-bin test:FileSystem.Event.LargeTree)
D=$PWD/dist-newstyle/largetree-tmp; mkdir -p $D
$B --label "Linux, tmpfs" +RTS -T -RTS
TMPDIR=$D $B --label "Linux, ext4" +RTS -T -RTS
TMPDIR=$D $B --label "Linux, ext4" +RTS -T -G1 -A1m -RTS   # memory
```

The times are the average of three runs with the default GC settings. The
memory is from a run with `-G1 -A1m`, in which every GC is a major GC, so the
maximum live heap includes short peaks. With the default settings it is
measured only at the major GCs.

CI runs the test on the 20000 directories on all platforms, and on macOS and
Windows, which have no limit on the number of watched directories, also on
100000 directories, with `LARGETREE_DIRS=20000,100000`. On GitHub Actions the
`CHECK_ENV` option of packcheck is empty so that the variable reaches the
test; packcheck clears the environment for any non-empty value, `"n"`
included.

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
that each is reported once. The number of overflows is asserted on Linux only.
The test of the start of a watch requires no overflow on all platforms: on
Linux the library lists a directory before it enables the open, access and
close events on it, and macOS and Windows do not list the tree at the start.
The tests use at most a tenth of the free inodes and space, and on Linux half
of the free inotify watches. All the test suites run with a heap limit of
1 GB, `-M1G`. Without profiling libraries, the heap of a test can be
examined with `+RTS -hT`, and with the build option
`-finfo-table-map -fdistinct-constructor-tables` and `+RTS -hi`.

