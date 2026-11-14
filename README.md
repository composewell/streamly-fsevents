# File System Event Notification (inotify/fsevents)

A **portable, robust, and stream-based file system event notification
API** for **Linux, macOS, and Windows**.  Provides a streaming interface
as well as a traditional handle style interface.

The portable module in this package offers a consistent, cross-platform,
common denominator API across all supported systems whereas
platform-specific modules expose more extensive APIs that allow you to
make full use of each operating system’s native capabilities.

## Features

The library is designed to be fast, light on resources, scalable and
reliable.

### Functionality

* Events are a streamly `Stream IO Event`, processed with streamly combinators.
* `Streamly.FileSystem.Event` has the same API on Linux, macOS and Windows.
* The `.Linux`, `.Darwin` and `.Windows` modules add OS-specific features.
* Streaming API: `watch` returns a stream; the watch is opened and closed for you.
* Handle API (Linux, internal): `openWatch` opens a watch, `addRoot` and
  `removeRoot` add or remove paths, `readEvent` reads one event,
  `eventStream` reads the events as a stream, `closeWatch` closes the watch.
  Several threads can read one watch.
* On Linux, moving a directory into the tree reports every file in it as created.
* On Linux, an unreadable subdirectory is reported and skipped; the rest of the
  tree stays watched.

### Scalability

On Linux:

* Large trees, deep trees, wide trees are all handled efficiently.
* Tree size is limited only by `max_user_watches`, one per directory.
* Starting a watch on a tree of any size cannot overflow the event queue.
* Moving a huge directory into the tree does not load the entire tree into memory.
* The watch keeps up with 5000 renames/s on a tree of 24200 dirs.

On a tree of 28259 directories on Linux, ext4, 2 CPUs, with warm caches:

| Measurement | Result |
|---|---|
| Start of the watch | ~0.82 s |
| Moving in or renaming 14129 directories | ~0.50 s |
| Memory after the start | ~8.7 MB |
| Memory after 3 move and rename cycles | ~8.7 MB |
| Peak memory, during the start | ~9.8 MB |

Notes for the table above:

* Traversing the tree alone takes 0.32 s during the startup
* Moving in or renaming a directory scans it and reports its contents
* Memory is about 320 bytes per directory

### Reliability

A watch is designed and tested to run indefinitely without leaking resources.
With the streaming API the watch is always closed, whether the stream ends,
fails, is interrupted or abandoned.
We have behavior tests to automatically verify and record [platform-specific
behavior differences](docs/platform-differences.md) for all platforms.

On Linux:

* No events are missed during renames within the tree.
* An overflow is reported by `isEventsLost`, and the watch is then closed.
* A symlink planted in the tree cannot make the watch follow it outside
  the tree.
* Security: a user having write access to the tree cannot crash the watch.
* Run manually: all 51436 combinations of up to 3 file operations, and up to 4 on fewer paths.
* The test suites run about 270 tests besides these combinations.

Tests on Linux:

| Property | Test |
|---|---|
| No event is missed when the tree changes during a scan | CI: 2157 combinations of up to 2 file operations |
| Timeouts and unwatchable dirs do not break the watch | Handle API: a timeout, fds running out |
| Deep trees do not run out of file descriptors | 300-level deep tree with 256 open fd limit |
| Killing the reading thread does not leak file descriptors | 2000 kills of the reading thread at varying points |
| Memory does not grow over a long-running watch | 3 cycles of moving 14129 dirs out and back in |

See [docs/robustness.md](docs/robustness.md),
[docs/platform-differences.md](docs/platform-differences.md) and
[docs/watch-limits.md](docs/watch-limits.md) for the details.

## Example

Watch the directory trees given on the command line and print the events:

```haskell
import Control.Monad (when)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import System.Environment (getArgs)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Event as Event
import qualified Streamly.FileSystem.Path as Path

handler :: Event.Event -> IO ()
handler ev = do
    let path = Path.toString (Event.getAbsPath ev)

    -- test the predicates
    when (Event.isCreated ev) $ putStrLn ("created: " ++ path)
    when (Event.isDeleted ev) $ putStrLn ("deleted: " ++ path)
    when (Event.isMoved ev) $ putStrLn ("moved: " ++ path)
    when (Event.isModified ev) $ putStrLn ("modified: " ++ path)
    when (Event.isEventsLost ev) $ putStrLn ("events lost, rescan: " ++ path)

main :: IO ()
main = do
    args <- getArgs
    paths <- mapM Path.fromString args
    case paths of
        [] -> putStrLn "Usage: watch DIR..."
        (p:ps) ->
            Event.watchRecursive (p :| ps)
                & Stream.fold (Fold.drainMapM handler)
```

Start the program to watch the directory `/home/user/notes`:

```
watch /home/user/notes
```

Then run these commands in `/home/user/notes` on Linux:

```
mkdir 2026
echo "buy milk" > 2026/todo.txt
mv 2026/todo.txt done.txt
rm done.txt
```

The program prints:

```
created: /home/user/notes/2026
created: /home/user/notes/2026/todo.txt
modified: /home/user/notes/2026/todo.txt
moved: /home/user/notes/2026/todo.txt
moved: /home/user/notes/done.txt
deleted: /home/user/notes/done.txt
```

The events reported, and how they are coalesced, differ between platforms.
See [docs/platform-differences.md](docs/platform-differences.md).

## Combining Predicates

An event predicate is a pure function `Event -> Bool`. The boolean operators
`&&` and `||` can be lifted to predicates:

```haskell
and_ :: (a -> Bool) -> (a -> Bool) -> a -> Bool
and_ = liftA2 (&&)
infixr 3 `and_`

or_ :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or_ = liftA2 (||)
infixr 2 `or_`
```

To keep only the create and move events:

```hs
Stream.filter (Event.isCreated `or_` Event.isMoved)
```

To keep only the modify events of non-directories, using the Linux specific
`isDir` predicate from `Streamly.FileSystem.Event.Linux`:

```hs
Stream.filter (Event.isModified `and_` not . Linux.isDir)
```

## Contributing

We welcome contributions of all kinds, bug reports, feature requests, and pull
requests. If you’d like to get involved, please open an issue or start a
discussion.

## Getting Support

* 💬 Join our [community chat](https://gitter.im/composewell/streamly).
* 📧 For dedicated support and feature requests, contact us at [support@composewell.com](mailto:support@composewell.com).
