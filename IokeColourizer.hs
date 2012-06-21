{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

import Data.Data
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.List as List
import Text.Peggy 
import Data.Maybe

import Control.Applicative
import System.IO
import System.Environment

import IokeGrammar
import IokeColours

main :: IO ()
main = do
    the_args <- getArgs
    the_data <- readFile (from the_args)
    withFile (to the_args) WriteMode (\handle -> do
       hPutStr handle $ printColours the_data)
    where from [x, _] = x
          from [x]    = x
          from _      = "iokefile.ik.in"
          to   [_, y] = y
          to   _      = "iokefile.html.out"
