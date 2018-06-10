{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment

import           Text.Pandoc.Filter.EmphasizeCode
import           Text.Pandoc.JSON

import qualified Data.Version                     as Version
import           Paths_pandoc_emphasize_code

main :: IO ()
main =
  getArgs >>= \case
    (arg:_)
      | arg == "-V" -> showVersion
      | arg == "--version" -> showVersion
    _ -> toJSONFilter emphasizeCode
  where
    showVersion = putStrLn (Version.showVersion version)
