-- |
-- Module      : Streamly.Test.FileSystem.Event.Darwin
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Darwin (main) where

import Control.Concurrent
    ( forkFinally, forkIO, killThread, newEmptyMVar, putMVar, readMVar
    , takeMVar, threadDelay, tryPutMVar)
import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM, forever, void)
import Data.Either (lefts)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Foreign.C.Types (CUInt(..))
import System.Directory (createDirectory, listDirectory)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performMajorGC)
import System.Posix.Process
    (executeFile, exitImmediately, forkProcess, getProcessStatus)
import System.Posix.Resource
    ( Resource(ResourceOpenFiles), ResourceLimit(..), ResourceLimits(..)
    , getResourceLimit, setResourceLimit)
import System.Posix.Signals
    ( Handler(Default, Ignore), installHandler, sigKILL, sigPIPE
    , signalProcess)
import System.Timeout (timeout)
import Test.Hspec

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FS.Event.Darwin as Event

import Streamly.Test.FileSystem.Event.Common

moduleName :: String
moduleName = "FS.Event.Darwin"

-------------------------------------------------------------------------------
-- Robustness tests
-------------------------------------------------------------------------------

-- | Raise the soft limit on open files to @n@, or to the hard limit if that
-- is lower. Each watch uses two fds.
raiseOpenFilesLimit :: Integer -> IO ()
raiseOpenFilesLimit n = do
    lim <- getResourceLimit ResourceOpenFiles
    let cap = case hardLimit lim of
            ResourceLimit h -> min h n
            _ -> n
    case softLimit lim of
        ResourceLimit s | s < cap ->
            setResourceLimit
                ResourceOpenFiles lim {softLimit = ResourceLimit cap}
        _ -> return ()

-- | Run @n@ watches on @dir@ in separate threads while a file in @dir@ is
-- written every 100 ms. Return the result of each watch: the exception
-- raised by the watch, or @Right ()@ when it reported an event. Return
-- 'Nothing' if a watch neither reported an event nor raised an exception
-- within 30 seconds.
--
-- A watch thread keeps draining its stream after the first event, so that
-- 'killThread' interrupts it inside the stream and the watch is closed
-- before the thread exits.
runWatches :: Int -> FilePath -> IO (Maybe [Either IOException ()])
runWatches n dir = do
    root <- Path.fromString dir
    threads <- forM [1 .. n] $ \_ -> do
        result <- newEmptyMVar
        exited <- newEmptyMVar
        let signal ev = tryPutMVar result (Right ()) >> return ev
            run = Stream.drain $ Stream.mapM signal
                    $ Event.watch (NonEmpty.fromList [root])
        tid <- forkFinally (try run) $ \r -> do
            case r of
                Right (Left e) -> void $ tryPutMVar result (Left e)
                _ -> return ()
            putMVar exited ()
        return (tid, result, exited)
    writer <- forkIO $ forever $ do
        writeFile (dir </> "file") ""
        threadDelay 100000
    results <-
        timeout 30000000 $ mapM (\(_, result, _) -> readMVar result) threads
    killThread writer
    mapM_ (\(tid, _, _) -> killThread tid) threads
    mapM_ (\(_, _, exited) -> takeMVar exited) threads
    return results

-- | macOS starts at most 512 FSEvents streams in a process. A watch beyond
-- that must raise an exception, not report nothing.
manyWatches :: Spec
manyWatches =
    it "a watch that cannot be started raises an exception" $ do
        raiseOpenFilesLimit 4096
        r <- withSystemTempDirectory "fsevent_dir" (runWatches 600)
        case r of
            Just xs ->
                putStrLn
                    $ "watches failed to start: " ++ show (length (lefts xs))
            Nothing -> return ()
        r `shouldSatisfy` isJust

-- | Close a watch while its callback is blocked writing to the full pipe,
-- with SIGPIPE set to its default action, which terminates the process.
-- The write fails when the read end is closed, it must not raise SIGPIPE.
--
-- The consumer stops reading after the first event, so that the pipe fills
-- up. The exception that stops the consumer arrives outside the bracket of
-- the stream, the watch is closed by the GC.
--
-- The RTS sends SIGPIPE to an OS thread to interrupt an interruptible
-- foreign call, e.g. the open(2) in 'writeFile', when an exception is
-- thrown to the Haskell thread making the call. Therefore, the writer
-- thread is stopped with a flag, after SIGPIPE is ignored again.
closeWithSigPipe :: Spec
closeWithSigPipe =
    it "closing a watch does not raise SIGPIPE" $
        withSystemTempDirectory "fsevent_dir" $ \dir -> do
            root <- Path.fromString dir
            started <- newEmptyMVar
            stop <- newIORef False
            writerDone <- newEmptyMVar
            watcherDone <- newEmptyMVar
            let block ev =
                    putMVar started () >> threadDelay 60000000 >> return ev
                write i = do
                    writeFile (dir </> show (i `mod` 1000 :: Int)) ""
                    done <- readIORef stop
                    if done then putMVar writerDone () else write (i + 1)
            _ <- installHandler sigPIPE Default Nothing
            _ <- forkIO $ write 0
            watcher <-
                flip forkFinally (const (putMVar watcherDone ()))
                    $ Stream.drain $ Stream.mapM block
                    $ Event.watch (NonEmpty.fromList [root])
            takeMVar started
            threadDelay 1000000
            killThread watcher
            takeMVar watcherDone
            performMajorGC
            threadDelay 1000000
            _ <- installHandler sigPIPE Ignore Nothing
            writeIORef stop True
            takeMVar writerDone

foreign import ccall unsafe "unistd.h sleep" c_sleep :: CUInt -> IO CUInt

-- | Number of open fds of this process.
countFds :: IO Int
countFds = length <$> listDirectory "/dev/fd"

-- | Close a watch while its callback is blocked writing to the full pipe
-- and a child process holds the read end of the pipe, so that the write
-- does not fail when the watch closes the read end. The child is created
-- by fork without exec, it inherits the fds even if they are close-on-exec.
-- Closing the watch must not hang; when it is done, the two fds of the
-- pipe are closed.
--
-- As in 'closeWithSigPipe', the consumer stops reading after the first
-- event and the watch is closed by the GC.
closeWithReader :: Spec
closeWithReader =
    it "closing a watch does not hang when a child holds the pipe" $
        withSystemTempDirectory "fsevent_dir" $ \dir -> do
            root <- Path.fromString dir
            started <- newEmptyMVar
            stop <- newIORef False
            writerDone <- newEmptyMVar
            watcherDone <- newEmptyMVar
            let block ev =
                    putMVar started () >> threadDelay 60000000 >> return ev
                write i = do
                    writeFile (dir </> show (i `mod` 1000 :: Int)) ""
                    done <- readIORef stop
                    if done then putMVar writerDone () else write (i + 1)
            fds <- countFds
            _ <- forkIO $ write 0
            watcher <-
                flip forkFinally (const (putMVar watcherDone ()))
                    $ Stream.drain $ Stream.mapM block
                    $ Event.watch (NonEmpty.fromList [root])
            takeMVar started
            child <- forkProcess $ c_sleep 30 >> exitImmediately ExitSuccess
            threadDelay 1000000
            killThread watcher
            takeMVar watcherDone
            writeIORef stop True
            takeMVar writerDone
            performMajorGC
            let waitClosed n = do
                    k <- countFds
                    if k <= fds || n <= (0 :: Int)
                    then return k
                    else threadDelay 100000 >> waitClosed (n - 1)
            k <- waitClosed 50
            signalProcess sigKILL child
            _ <- getProcessStatus True False child
            k `shouldBe` fds

-- | The number of fds open in a child process created with fork and exec.
childFds :: FilePath -> IO Int
childFds out = do
    pid <- forkProcess $
        executeFile "/bin/sh" False ["-c", "ls /dev/fd > " ++ out] Nothing
    _ <- getProcessStatus True False pid
    s <- readFile out
    evaluate (length (lines s))

-- | A child process must not inherit the fds of a watch.
childInherits :: Spec
childInherits =
    it "a child process does not inherit the fds of a watch" $
        withSystemTempDirectory "fsevent_dir" $ \dir -> do
            let out = dir </> "fds"
            root <- Path.fromString (dir </> "root")
            createDirectory (dir </> "root")
            n0 <- childFds out
            started <- newEmptyMVar
            let signal ev = tryPutMVar started () >> return ev
            watcher <- forkIO $ Stream.drain $ Stream.mapM signal
                $ Event.watch (NonEmpty.fromList [root])
            -- Repeat the write, the watch may not be started for the first
            writer <- forkIO $ forever $ do
                writeFile (dir </> "root" </> "file") ""
                threadDelay 100000
            takeMVar started
            killThread writer
            n1 <- childFds out
            killThread watcher
            n1 `shouldBe` n0

-- TODO:
-- Creation/deletion/move of hard links
-- Deletion of last hard link
-- If a directory is a symlink is it followed in recursive mode?

main :: IO ()
main = do
    let rootLevelTests =
              fileCreate "file-create-attrs"
                (\file ->
                    [ (file, fileEvent Event.isCreated)
                    , (file, fileEvent Event.isAttrsModified)
                    ]
                )
            : fileModify
                    "file-mod-attrs"
                    (\file -> [(file, fileEvent Event.isAttrsModified)])
            : dirDelete "dir-sec" (\dir ->
                [ (dir, dirEvent Event.isDeleted)
                , (dir, dirEvent Event.isSecurityModified)
                ]
                )
            : commonTests
    let rootTests =
              dirDelete "" (\dir ->
                [ (dir, dirEvent Event.isDeleted)
                , (dir, dirEvent Event.isSecurityModified)
                , (dir, Event.isRootPathEvent)
                ]
                )
            -- The watch root create event always seems to come even though the
            -- root is created before the watch is started. That may be because
            -- of batching?
            -- XXX Need to create watch root after adding the watch, otherwise
            -- it fails intermittently.
            -- : dirCreate "" (\dir -> [(dir, dirEvent Event.isCreated)])
            : rootDirMove "moved" (\src ->
                [ (src, Event.isRootPathEvent)
                , (src, Event.isMoved)
                ])
            : []

    let fileRootTests =
            [ fileDelete "" (\path ->
                [ (path, Event.isRootPathEvent)
                , (path, Event.isDeleted)
                ])
            , rootFileMove "moved" (\path ->
                [(path, Event.isRootPathEvent)
                , (path, Event.isMoved)
                ])
            , fileModify "" (const [])
            ]

    let symLinkNonRecTests = nestedOpsTest : rootLevelTests
        regularNonRecTests = rootTests ++ symLinkNonRecTests

    let w = failNested (Event.watchWith (Event.setAllEvents True))
        run = runTests moduleName "non-recursive" w

    run DirType regularNonRecTests
    run SymLinkOrigPath symLinkNonRecTests
    run FileType fileRootTests

    let symLinkRecTests =
              dirCreateWithParent "subdir/dir"
                (\dir -> [(dir, dirEvent Event.isCreated)])
            : fileCreateWithParent "subdir/file"
                (\file ->
                    [ (file, fileEvent Event.isCreated)
                    , (file, fileEvent Event.isAttrsModified)
                    ]
                )
            : rootLevelTests
            ++ commonRecTests
        regularRecTests = rootTests ++ symLinkRecTests

    let recw = Event.watchWith
                (Event.setRecursiveMode True . Event.setAllEvents True)
        runRec = runTests moduleName "recursive" recw

    runRec DirType regularRecTests
    runRec SymLinkOrigPath symLinkRecTests
    runRec FileType fileRootTests

    hspec $ describe moduleName $ describe "robustness" $ do
        closeWithSigPipe
        closeWithReader
        childInherits
        manyWatches
