-- |
-- Module      : Streamly.Test.FileSystem.Event
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Test.FileSystem.Event (main) where

import Control.Concurrent
    ( forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay )
import Control.Exception
    (IOException, SomeException, bracket, finally, throwIO, try)
import Control.Monad (forM_, unless, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (elemIndex, isPrefixOf, nub, sort, sortOn)
import Streamly.Internal.FS.Event (Config, Event)
import System.Directory (createDirectory)
import System.FilePath ((</>), pathSeparator, takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FS.Event as Event
import qualified Streamly.Internal.FileSystem.Path as Path
#if defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FS.Event.Linux as Linux
#endif

import Streamly.Test.FileSystem.Event.Common

moduleName :: String
moduleName = "FS.Event"

-------------------------------------------------------------------------------
-- Event helpers
-------------------------------------------------------------------------------

absPath :: Event -> FilePath
absPath = Path.toString . Event.getAbsPath

rootPath :: Event -> FilePath
rootPath = Path.toString . Event.getRoot

isCreatedAt :: FilePath -> Event -> Bool
isCreatedAt file ev = Event.isCreated ev && absPath ev == file

-- | The paths of the events that satisfy the predicate.
pathsOf :: (Event -> Bool) -> [Event] -> [FilePath]
pathsOf p = map absPath . filter p

-- | Whether the event reports a change of an entry.
isChange :: Event -> Bool
isChange ev =
       Event.isCreated ev
    || Event.isDeleted ev
    || Event.isMoved ev
    || Event.isModified ev

-- | @dir@ or a path under it.
isUnder :: FilePath -> FilePath -> Bool
isUnder dir path = path == dir || (dir ++ [pathSeparator]) `isPrefixOf` path

-- | The 'Event.isCreated' events must be for the given paths, one for each
-- path. On Linux they must be in the order of the paths, the order in which
-- the entries were created.
--
-- On macOS a later event for a path can repeat 'Event.isCreated', there must
-- be at least one for each path, and only the events of the given paths are
-- checked, the markers of 'waitStarted' and 'syncOn' can be reported again.
shouldCreate :: [Event] -> [FilePath] -> Expectation
shouldCreate evs paths =
#if defined(CABAL_OS_LINUX)
    created `shouldBe` paths
#elif defined(CABAL_OS_DARWIN)
    nub (sort (filter (`elem` paths) created)) `shouldBe` nub (sort paths)
#else
    sort created `shouldBe` sort paths
#endif

    where

    created = pathsOf Event.isCreated evs

createFile :: FilePath -> IO ()
createFile file = writeFile file ""

-------------------------------------------------------------------------------
-- Running a watch
-------------------------------------------------------------------------------

-- | On Linux the watch reports all the events, as the Linux test suite does.
allEvents :: Config -> Config
#if defined(CABAL_OS_LINUX)
allEvents = Linux.setAllEvents True
#else
allEvents = id
#endif

recursive :: Config -> Config
recursive = Event.setRecursiveMode True

-------------------------------------------------------------------------------
-- Handle API
-------------------------------------------------------------------------------

#if defined(CABAL_OS_WINDOWS)
handleTests :: Spec
handleTests =
    it "handle API" $ pendingWith "the handle API is not implemented"
#else
-- | Open a watch on the directory @root@ in a temporary directory, run the
-- action with the root and the watch, and close the watch.
withRootHandle ::
    (Config -> Config) -> (FilePath -> Event.Watch -> IO a) -> IO a
withRootHandle cfg action =
    withSystemTempDirectory "fsevent_dir" $ \fp -> do
        let root = fp </> "root"
        createDirectory root
        r <- Path.fromString root
        bracket
            (Event.openWatch (cfg . allEvents) (r NonEmpty.:| []))
            Event.closeWatch
            (action root)

-- | Read the events from the watch using 'Event.readEvent' until the
-- predicate holds for the events read so far, or the watch is closed.
-- Throw an exception if neither happens within 10 seconds.
readEventsUntil :: ([Event] -> Bool) -> Event.Watch -> IO [Event]
readEventsUntil done w = do
    r <- timeout 10000000 (go [])
    maybe (error "readEventsUntil: timed out") return r

    where

    go acc
        | done (reverse acc) = return (reverse acc)
        | otherwise = do
            r <- Event.readEvent w
            case r of
                Nothing -> return (reverse acc)
                Just ev -> go (ev : acc)

-- | Read the events of the watch up to and including the first event that
-- satisfies the predicate. Throw an exception if it does not occur within 10
-- seconds or the watch is closed.
readUntilH :: (Event -> Bool) -> Event.Watch -> IO [Event]
readUntilH p w = do
    r <- timeout 10000000 (go [])
    maybe (error "readUntilH: timed out") return r

    where

    go acc = do
        r <- Event.readEvent w
        case r of
            Nothing -> error "readUntilH: the watch is closed"
            Just ev
                | p ev -> return (reverse (ev : acc))
                | otherwise -> go (ev : acc)

-- | Like 'readUntilH' but read the events using 'Event.eventStream'.
readUntilS :: (Event -> Bool) -> Event.Watch -> IO [Event]
readUntilS p w = do
    r <- timeout 10000000
        $ Stream.fold (Fold.takeEndBy p Fold.toList)
        $ Event.eventStream w
    evs <- maybe (error "readUntilS: timed out") return r
    unless (any p evs) $ error "readUntilS: the watch is closed"
    return evs

-- | Wait until the action returns True, check it every 10 ms. Throw an
-- exception if it does not return True within 10 seconds.
waitUntil :: String -> IO Bool -> IO ()
waitUntil desc action = do
    r <- timeout 10000000 go
    maybe (error (desc ++ ": timed out")) return r

    where

    go = do
        ok <- action
        unless ok (threadDelay 10000 >> go)

-- | Run the action while another thread reads the events of the watch up to
-- and including the first event that satisfies the predicate. Return the
-- result of the action and the events read. The reader uses one
-- 'Event.eventStream' from before the action to after it, the stream must
-- report the events of the roots that the action adds.
whileReading :: (Event -> Bool) -> Event.Watch -> IO a -> IO (a, [Event])
whileReading p w action = do
    mv <- newEmptyMVar
    bracket
        (forkIO (try (readUntilS p w) >>= putMVar mv))
        killThread
        $ \_ -> do
            -- Let the reader block in the read
            threadDelay 200000
            r <- action
            evs <- takeMVar mv
            either (\(e :: SomeException) -> throwIO e) (return . (r,)) evs

-- | The roots of the create events of the file.
createdRoots :: FilePath -> [Event] -> [FilePath]
createdRoots file = nub . map rootPath . filter (isCreatedAt file)

-- The watch reports a change made right after 'Event.openWatch' returns,
-- without waiting for a marker event. Repeated to catch a watch that starts
-- asynchronously.
openActive :: Spec
openActive =
    it "a change right after openWatch returns is reported"
        $ withSystemTempDirectory "fsevent_dir" $ \fp -> do
            r <- Path.fromString fp
            forM_ [1 .. 50 :: Int] $ \i ->
                bracket
                    (Event.openWatch allEvents (r NonEmpty.:| []))
                    Event.closeWatch
                    $ \w -> do
                        let file = fp </> ("file" ++ show i)
                        createFile file
                        evs <- readUntilH (isCreatedAt file) w
                        createdRoots file evs `shouldBe` [fp]

-- The reader thread is blocked reading the watch.
addWhileReading :: Spec
addWhileReading =
    it "addRoot does not block while another thread reads the events"
        $ withRootHandle id $ \root w -> do
            let root2 = takeDirectory root </> "root2"
                file = root2 </> "file"
            createDirectory root2
            p <- Path.fromString root2
            (r, evs) <- whileReading (isCreatedAt file) w $ do
                r <- timeout 2000000 $ Event.addRoot w p
                -- Unblock the reader
                createFile file
                return r
            r `shouldBe` Just ()
            createdRoots file evs `shouldBe` [root2]

-- The tree under the added root is watched, as for a root of
-- 'Event.openWatch'.
addRecursive :: Spec
addRecursive =
    it "addRoot in recursive mode watches the tree under the root"
        $ withRootHandle recursive $ \root w -> do
            let root2 = takeDirectory root </> "root2"
                sub = root2 </> "sub"
                file = sub </> "file"
            createDirectory root2
            createDirectory sub
            p <- Path.fromString root2
            (_, evs) <- whileReading (isCreatedAt file) w $ do
                Event.addRoot w p
                createFile file
            createdRoots file evs `shouldBe` [root2]

-- On macOS each addRoot and removeRoot replaces the FSEvents stream of the
-- watch, the events of the root that stays are not lost or duplicated.
--
-- TODO: Test the root changes with a root on a read-only volume on macOS.
-- A read-only volume has no FSEvents history, replacing the stream cannot
-- replay the events since the last event id and may report isEventsLost
-- instead, as documented for addRoot and removeRoot. No test covers this
-- case now: this test is marked pending when isEventsLost is reported, but
-- its roots are on a writable volume. A read-only volume can be made with
-- "hdiutil create -size 10m -fs APFS" and "hdiutil attach -readonly". Add a
-- directory in the image as a second root, add and remove other roots while
-- files are created in the writable root, and check that every event of
-- the writable root, or an isEventsLost event, is reported. Needs a CI
-- runner that allows attaching a disk image.
keepEventsWhileAdding :: Spec
keepEventsWhileAdding =
    it "a root reports all its events while other roots are added, removed"
        $ withRootHandle id $ \root w -> do
            let base = takeDirectory root
                files = [root </> ("file" ++ show i) | i <- [1 .. n]]
                end = root </> "end"
                dirs = [base </> ("dir" ++ show i) | i <- [1 .. adds]]
                writer = mapM_ createFile files >> createFile end
                changeRoots (i, dir) = do
                    p <- Path.fromString dir
                    Event.addRoot w p
                    when (even i) $ Event.removeRoot w p
            mapM_ createDirectory dirs
            done <- newEmptyMVar
            (_, evs) <- whileReading (isCreatedAt end) w $
                bracket
                    (forkIO (writer `finally` putMVar done ()))
                    killThread
                    $ \_ -> do
                        mapM_ changeRoots (zip [1 :: Int ..] dirs)
                        takeMVar done
            let ofRoot = filter ((== root) . rootPath) evs
            if any Event.isEventsLost evs
            then pendingWith "events were lost, reported by isEventsLost"
            else ofRoot `shouldCreate` (files ++ [end])

    where

    n = 2000 :: Int
    adds = 20 :: Int

-- On macOS the events of the removed root can be reported until the
-- FSEvents stream of the watch is replaced. A file is created in the removed
-- root and a marker in the other root until the marker is reported without
-- the file.
removeStops :: Spec
removeStops =
    it "a removed root stops reporting its events"
        $ withRootHandle id $ \root w -> do
            let root2 = takeDirectory root </> "root2"
            createDirectory root2
            p <- Path.fromString root2
            Event.addRoot w p
            Event.removeRoot w p
            let try1 i = do
                    let file = root2 </> ("file" ++ show i)
                        marker = root </> ("marker" ++ show i)
                    createFile file
                    createFile marker
                    evs <- readUntilH (isCreatedAt marker) w
                    return (file `notElem` pathsOf Event.isCreated evs)
                go i
                    | i > (100 :: Int) =
                        expectationFailure "the removed root still reports"
                    | otherwise = do
                        stopped <- try1 i
                        if stopped then check (i + 1) else go (i + 1)
                check i = do
                    let file = root2 </> ("file" ++ show i)
                        marker = root </> ("marker" ++ show i)
                    createFile file
                    createFile marker
                    evs <- readUntilH (isCreatedAt marker) w
                    filter (isUnder root2) (pathsOf isChange evs)
                        `shouldBe` []
            go 1

-- The events of the root after it is added again are reported.
removeAddAgain :: Spec
removeAddAgain =
    it "a root removed and added again reports the events after the add"
        $ withRootHandle id $ \root w -> do
            let file = root </> "file"
            p <- Path.fromString root
            Event.removeRoot w p
            Event.addRoot w p
            createFile file
            evs <- readUntilH (isCreatedAt file) w
            createdRoots file evs `shouldBe` [root]

closedWatchOps :: Spec
closedWatchOps =
    it "addRoot raises an exception, removeRoot does nothing on a closed watch"
        $ withRootHandle id $ \root w -> do
            p <- Path.fromString root
            Event.closeWatch w
            r <- try (Event.addRoot w p)
            either (\(_ :: IOException) -> True) (const False) r
                `shouldBe` True
            Event.removeRoot w p
            Event.closeWatch w

-- The reader thread is blocked reading the watch. The stream ends when the
-- watch is closed, without any file system event.
closeWhileReading :: Spec
closeWhileReading =
    it "closeWatch does not block while another thread reads the events"
        $ withRootHandle id $ \_ w -> do
            mv <- newEmptyMVar
            let reader = Stream.fold Fold.drain (Event.eventStream w)
            bracket
                (forkIO (try reader >>= putMVar mv))
                killThread
                $ \_ -> do
                    threadDelay 200000
                    timeout 2000000 (Event.closeWatch w)
                        `shouldReturn` Just ()
                    -- The stream ends without an exception
                    r <- timeout 2000000 (takeMVar mv)
                    fmap (either (\(_ :: SomeException) -> False) ok) r
                        `shouldBe` Just True

    where

    ok () = True

-- The events of the file created before 'Event.closeWatch' can still be in
-- the watch after it, the reads return them before 'Nothing'.
readEventAfterClose :: Spec
readEventAfterClose =
    it "readEvent returns Nothing after closeWatch, and on every later call"
        $ withRootHandle id $ \root w -> do
            let file = root </> "file"
            createFile file
            evs <- readUntilH (isCreatedAt file) w
            createdRoots file evs `shouldBe` [root]
            Event.closeWatch w
            _ <- readEventsUntil (const False) w
            isNothing <$> Event.readEvent w `shouldReturn` True
            isNothing <$> Event.readEvent w `shouldReturn` True

    where

    isNothing = maybe True (const False)

-- A read interrupted by a timeout while it waits does not lose an event.
--
-- On Linux, with all the events enabled, 'Event.openWatch' opens and closes
-- the root after adding its watch, the watch reports the open and the close.
-- The reads before the first timeout return these events.
readTimeout :: Spec
readTimeout =
    it "a readEvent interrupted while waiting does not lose an event"
        $ withRootHandle id $ \root w -> do
            forM_ [1 .. 20 :: Int] $ \_ -> readUntilTimeout w
            let file = root </> "file"
            createFile file
            evs <- readUntilH (isCreatedAt file) w
            createdRoots file evs `shouldBe` [root]

    where

    readUntilTimeout w = do
        r <- timeout 1000 (Event.readEvent w)
        case r of
            Nothing -> return ()
            Just Nothing -> expectationFailure "the watch is closed"
            Just (Just _) -> readUntilTimeout w

-- Each event is returned to one of the readers, on Linux an event returned
-- to both would fail 'shouldCreate'. The readers end when the watch is
-- closed. The events of the two readers interleave in any order, they are
-- sorted in the order of creation before the check.
twoReaders :: Spec
twoReaders =
    it "two threads reading a watch with readEvent get each event once"
        $ withRootHandle id $ \root w -> do
            let files = [root </> ("file" ++ show i) | i <- [1 .. n]]
            seen <- newIORef []
            let record ev =
                    when (Event.isCreated ev)
                        $ atomicModifyIORef' seen (\s -> (absPath ev : s, ()))
                loop acc = do
                    r <- Event.readEvent w
                    case r of
                        Nothing -> return (reverse acc)
                        Just ev -> record ev >> loop (ev : acc)
                allSeen = do
                    s <- readIORef seen
                    return (all (`elem` s) files)
            mv1 <- newEmptyMVar
            mv2 <- newEmptyMVar
            bracket
                (mapM (\mv -> forkIO (try (loop []) >>= putMVar mv))
                    [mv1, mv2])
                (mapM_ killThread)
                $ \_ -> do
                    mapM_ createFile files
                    waitUntil "twoReaders" allSeen
                    Event.closeWatch w
                    evs1 <- takeMVar mv1 >>= either rethrow return
                    evs2 <- takeMVar mv2 >>= either rethrow return
                    let byFile ev = elemIndex (absPath ev) files
                    sortOn byFile (evs1 ++ evs2) `shouldCreate` files

    where

    n = 200 :: Int
    rethrow (e :: SomeException) = throwIO e

handleTests :: Spec
handleTests = do
    openActive
    addWhileReading
    addRecursive
    keepEventsWhileAdding
    removeStops
    removeAddAgain
    closedWatchOps
    closeWhileReading
    readEventAfterClose
    readTimeout
    twoReaders
#endif

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

watchTests :: IO ()
watchTests =
    hspec
        $ describe moduleName
        $ describe "handle API" handleTests

main :: IO ()
main = do
    let run = runTests moduleName "non-recursive" (failNested Event.watch)
    run DirType (nestedOpsTest : commonTests)
    run SymLinkOrigPath (nestedOpsTest : commonTests)

    let runRec = runTests moduleName "recursive" Event.watchRecursive
    runRec DirType (commonTests ++ commonRecTests)
    runRec SymLinkOrigPath (commonTests ++ commonRecTests)

    watchTests
