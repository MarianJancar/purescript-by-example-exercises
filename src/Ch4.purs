module Ch4 where

import Prelude  

import Data.Array
import Data.Path
import Data.Tuple

import FileOperations

dispatch :: Tuple (Array Path) (Array Path) → Path → Tuple (Array Path) (Array Path)
dispatch (Tuple f d) e@(Directory _ _) = Tuple f (snoc d e)
dispatch (Tuple f d) e = Tuple (snoc f e) d

files :: Path → Array Path
files =  ls >>> filter (not isDirectory)

dirs :: Path → Array Path
dirs = ls >>> filter isDirectory

lsX :: Path → Tuple (Array Path) (Array Path)
lsX path = foldl dispatch (Tuple [] []) (ls path)

allFiles :: Path → Array Path
allFiles path = foldl (<>) (files path) (allFiles <$> dirs path)