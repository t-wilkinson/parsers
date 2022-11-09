module Transpiler where

import System.IO
import Control.Monad (when, join)
import Data.List (intersperse)

import Syntax
import ParserCombinators

transpile :: String -> [String]
transpile expr = fmap eval $ run expr

save :: String -> IO ()
save input = do
    writeFile "output.py" $ join $ intersperse "\n" contents
    where
        contents =
            [ "import sys"
            , "import subprocess"
            ] ++ transpile input
