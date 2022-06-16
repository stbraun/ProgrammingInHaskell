{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Exception (IOException, handle)
import Control.Monad     (join, void, when)
import Data.Foldable     (for_)
import Data.IORef        (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List         (isSuffixOf)
import System.Directory  (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import qualified Data.Set as Set (empty, insert, member)

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
traverseDirectory :: FilePath -> (FilePath -> a) -> IO [a]
traverseDirectory rootPath action = do
    -- Keep all paths already seen in a set.
    seenRef <- newIORef Set.empty
    -- Keep a list of results.
    resultRef <- newIORef []
    let
        -- Some helper functions that need access to seenRef
        haveSeenDirectory canonicalPath =
            Set.member canonicalPath <$> readIORef seenRef
        addDirectoryToSeen canonicalPath = do
            modifyIORef seenRef $ Set.insert canonicalPath
        traverseSubdirectory subdirPath = do
            contents <- listDirectory subdirPath
            for_ contents $ \file' ->
                handle @IOException (\_ -> pure ()) $ do
                    let file = subdirPath <> "/" <> file'
                    canonicalPath <- canonicalizePath file
                    classification <- classifyFile canonicalPath
                    case classification of
                      FileTypeOther -> pure ()
                      FileTypeRegularFile -> modifyIORef resultRef (\results -> action file : results)
                      FileTypeDirectory -> do
                        alreadyProcessed <- haveSeenDirectory file
                        when (not alreadyProcessed) $ do
                          addDirectoryToSeen file
                          traverseSubdirectory file
    traverseSubdirectory (dropSuffix "/" rootPath)
    readIORef resultRef





main = do
    files <- traverseDirectory "./" (\p -> p)
    mapM putStrLn files
    putStrLn $ "Number of items: " <> show (length files)
    return ()


