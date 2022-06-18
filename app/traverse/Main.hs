{-# LANGUAGE TypeApplications #-}
-- |
-- Traverse directory hierarchy.
-- Effective Haskell ch. 10
--
module Main where

import Control.Exception (IOException, handle)
import Control.Monad     (join, void, when)
import Data.Foldable     (for_)
import Data.IORef        (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List         (isSuffixOf)
import qualified Data.Map as Map
import System.Directory  (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import qualified Data.Set as Set (empty, insert, member)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Text.Printf (printf)


import Metrics


dropSuffix :: String -> String -> String
dropSuffix suffix s
    | suffix `isSuffixOf` s = take (length s - length suffix) s
    | otherwise = s


data FileType = FileTypeDirectory | FileTypeRegularFile | FileTypeOther

classifyFile :: FilePath -> IO FileType
classifyFile fname = do
    isDirectory <- doesDirectoryExist fname
    isFile <- doesFileExist fname
    pure $ case (isDirectory, isFile) of
        (True, False) -> FileTypeDirectory
        (False, True) -> FileTypeRegularFile
        otherwise     -> FileTypeOther


-- |
-- Naive implementation of directory traversal without IORef.
naiveTraversal :: FilePath -> (FilePath -> a) -> IO [a]
naiveTraversal rootPath action = do
    classification <- classifyFile rootPath
    case classification of
      FileTypeOther -> pure []
      FileTypeRegularFile -> pure $ [action rootPath]
      FileTypeDirectory -> do
        contents <- map (fixPath rootPath) <$> listDirectory rootPath
        results <- concat <$> mapM (\path -> naiveTraversal path action) contents
        pure results
    where
      -- listDirectory returns relative paths so, we have to prepend the rootpath
      fixPath parent fname = parent <> "/" <> fname


-- |
-- More efficient directory traversal handling also circular references.
traverseDirectory :: Metrics -> FilePath -> (FilePath -> IO ()) -> IO ()
traverseDirectory metrics rootPath action = do
    -- Keep reference to all paths already seen in a set.
    seenRef <- newIORef Set.empty
    let
        -- Some helper functions that need access to seenRef
        haveSeenDirectory canonicalPath =
            Set.member canonicalPath <$> readIORef seenRef

        addDirectoryToSeen canonicalPath = do
            modifyIORef seenRef $ Set.insert canonicalPath

        traverseSubdirectory subdirPath =
          timeFunction metrics "traverseSubdirectory" $ do
            contents <- listDirectory subdirPath
            for_ contents $ \file' ->
                handle @IOException (\ex -> putStrLn (show ex) >> tickFailure metrics) $ do
                    let file = subdirPath <> "/" <> file'
                    canonicalPath <- canonicalizePath file
                    classification <- classifyFile canonicalPath
                    result <- case classification of
                      FileTypeOther -> pure ()
                      FileTypeRegularFile -> action file
                      FileTypeDirectory -> do
                        alreadyProcessed <- haveSeenDirectory file
                        when (not alreadyProcessed) $ do
                          addDirectoryToSeen file
                          traverseSubdirectory file
                    tickSuccess metrics
                    pure result
    traverseSubdirectory (dropSuffix "/" rootPath)


traverseDirectory' :: Metrics -> FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory' metrics rootPath action = do
    -- Keep reference to list of result items.
    resultsRef <- newIORef []
    traverseDirectory metrics rootPath $ \file -> do
        modifyIORef resultsRef (action file :)
    readIORef resultsRef


directorySummaryWithMetrics :: FilePath -> IO ()
directorySummaryWithMetrics root = do
    metrics <- newMetrics
    histogramRef <- newIORef Map.empty
    traverseDirectory metrics root $ \file -> do
        putStrLn $ file <> ":"
        contents <- timeFunction metrics "TextIO.readFile" $ TextIO.readFile file
        timeFunction metrics "wordCount" $
            let wordCount = length $ Text.words contents
            in putStrLn $ "    word count: " <> show wordCount
        timeFunction metrics "histogram" $ do
            oldHistogram <- readIORef histogramRef
            let
                addCharToHistogram :: Map.Map Char Int -> Char -> Map.Map Char Int
                addCharToHistogram histogram letter =
                    Map.insertWith (+) letter 1 histogram
                newHistogram = Text.foldl' addCharToHistogram oldHistogram contents
            writeIORef histogramRef newHistogram
    histogram <- readIORef histogramRef
    putStrLn "Histogram Data:"
    for_ (Map.toList histogram) $ \(letter, count) ->
        putStrLn $ printf "    %c: %d" letter count
    displayMetrics metrics


main = do
    directorySummaryWithMetrics "./"

