{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Directory
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.Search as B -- package stringsearch
import Codec.Archive.Zip

main = do
    [filename] <- getArgs
    z <- toArchive `fmap` B.readFile filename
    let css = [ file | file <- filesInArchive z, ".css" `isSuffixOf` file ]
        z' = foldl fixCSSFile z css
        tmp = filename ++ ".tmp"
    B.writeFile tmp $ fromArchive z'
    renameFile tmp filename

fixCSSFile z f = addEntryToArchive e' z
    where
        Just e = findEntryByPath f z
        e' = toEntry f (eLastModified e) $ fixCSSText $ fromEntry e

fixCSSText s = B.unlines l'
    where
        l = B.lines s
        l' = "@import url(res:///Data/userStyle.css);" : map fixLine
                (filter (not . ("@import" `B.isPrefixOf`)) l)

fixLine = B.replace "font-family" ("xxx-ignore" :: BS.ByteString)
