module Main where

import System.IO
import Control.Applicative
import Control.Monad

import Syntax
import ParserCombinators
import Transpiler

main :: IO ()
main = do
    handle <- openFile "examples/input.webshell" ReadMode

    contents <- hGetContents handle
    save contents
    hClose handle

    pure ()
