{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment

import           Text.Pandoc.JSON
import           Text.Pandoc.Filter.EmphasizeCode

import Paths_pandoc_emphasize_code
import qualified Data.Version          as Version

main :: IO ()
main =
  getArgs >>=
    \case
      (arg:_)
        | arg == "-V" -> showVersion
        | arg == "--version" -> showVersion
      _ -> toJSONFilter emphasizeCode
  where
    showVersion = putStrLn (Version.showVersion version)
