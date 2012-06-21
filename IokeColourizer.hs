{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

import Data.Data
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.List as List
import Text.Peggy 
import Data.Maybe

import Control.Applicative
import System.IO

import IokeGrammar
import IokeColours

main :: IO ()
main = do
    the_data <- readFile "iokefile.ik.in"
    withFile "iokefile.html.out" WriteMode (\handle -> do
      hPutStr handle $ printColours the_data)
