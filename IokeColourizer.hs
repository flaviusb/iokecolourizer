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
import IndexIokeMangling

main :: IO ()
main = do
    the_args <- getArgs
    the_data <- readFile (from the_args)
    let ioke_data = printColourized (twitterize (getIoke the_data)) in
      withFile (to the_args) WriteMode (\handle -> do
         hPutStr handle ioke_data)
    where from [x, _] = x
          from [x]    = x
          from _      = "iokefile.ik.in"
          to   [_, y] = y
          to   _      = "iokefile.html.out"

orEmpty :: String -> Either ParseError String -> Chunk
orEmpty _   (Right foo) = RawInsert foo
orEmpty str (Left  _)   = Lit str

twitterize :: [Ioke] -> [Ioke]
twitterize = map (\expr -> (case expr of
  LiteralString str -> LiteralString (atify str)
  x                 -> x))

atify :: LitS -> LitS
atify (SquareString chunks) = SquareString (atchunks chunks)
atify (QuotedString chunks) = QuotedString (atchunks chunks)

atchunks :: [Chunk] -> [Chunk]
atchunks [Lit text] = [orEmpty text (parseString tweet "tweet" text)]
atchunks chunks     = chunks
