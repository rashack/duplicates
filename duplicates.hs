#!/usr/bin/env runhaskell

module Main where

import Control.Monad (forM)
import qualified Data.HashMap.Lazy as M
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>), takeFileName)
import System.IO (openFile, hFileSize, hClose, IOMode(ReadMode))

data Size = Size Integer deriving Show
data Entry = Entry FilePath Size

main :: IO ()
main = do
  [pa, pb] <- getArgs
  filesa <- find pa
  filesb <- find pb
  let hm = files2map filesa
  mapM_ (\(Entry f _s) -> exists f hm) filesb
  return ()

find :: FilePath -> IO [Entry]
find path = do
  allFiles <- getDirectoryContents path
  let files = filter (`notElem` [".", ".."]) allFiles
  paths <- forM files $ \file -> do
    let fullPath = path </> file
    isDir <- doesDirectoryExist fullPath
    if isDir
      then find fullPath
      else return $ [Entry fullPath (Size (-1))]
  return (concat paths)

fileSize :: FilePath -> IO Integer
fileSize file = do
  h <- openFile file ReadMode
  size <- hFileSize h
  hClose h
  return size

files2map :: [Entry] -> M.HashMap FilePath Entry
files2map files =
  M.fromList $ map (\(Entry f s) -> (takeFileName f, Entry f s)) files

printMap hm =
  mapM_ (\k -> printEntry k hm) $ M.keys hm

printEntry k hm =
  case M.lookup k hm of
    Just (Entry f s) -> putStrLn $ k ++ " -> " ++ f ++ " (" ++ (show s) ++ ")"
    Nothing -> return ()

exists f hm =
  case M.lookup (takeFileName f) hm of
    Just (Entry f s) -> putStrLn f
    Nothing -> return ()
