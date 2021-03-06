{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module IokeColours
(printColours, getIoke, printColourized) where

import Data.Data
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.List as List
import Text.Peggy 
import Data.Maybe

import Control.Applicative
import System.IO

import IokeGrammar

printColours :: String -> String
printColours the_data = printColourized $ getIoke the_data

getIoke :: String -> [Ioke]
getIoke the_data = rainbowbraces 0 9 (case (parseString ioke "foo" the_data) of
        Right foo -> foo
        Left _    -> [])

rainbowbraces :: Int -> Int -> [Ioke] -> [Ioke]
rainbowbraces curr lim uncoloured = map (\ik -> case ik of
  Brackets lvl expr -> Brackets curr (rainbowbraces (if (curr + 1) <= lim then curr + 1 else 0) lim expr)
  x                 -> x) uncoloured

printColourized :: [Ioke] -> String
printColourized iks = concat (map printBlock (colourize iks))

data ColourBlock = ColourBlock String Colour

data Colour = Colour String | UnColoured | Rainbow Int

comment   = Colour "rem"
bangline  = Colour "hb"
quotes    = Colour "q"
litcolour = Colour "IokeString"
esccolour = Colour "esc"
splcolour = Colour "StringSpliceRegion"
symcolour = Colour "Symbol"


printBlock :: ColourBlock -> String
printBlock block = case block of
  ColourBlock text UnColoured    -> text
  ColourBlock text (Colour col)  -> "<div class=\"" ++ col ++ "\">" ++ text ++ "</div>"
  ColourBlock text (Rainbow lvl) -> "<b" ++ (show lvl) ++">" ++ text ++ "</b" ++ (show lvl) ++">"  -- For brackets. Use tags <b0> <b1> etc with styling in css, rather than divclaassdivclassdivclassdivclass

escHTML :: String -> String
escHTML str = str -- replace this with an actual escaper

colourize :: [Ioke] -> [ColourBlock]
colourize code = concat (map (\ik -> (case ik of
  Comment cont      -> [ColourBlock (escHTML cont)      comment]
  Ret               -> [ColourBlock "\r\n"              UnColoured]
  Fullstop          -> [ColourBlock "."                 UnColoured]
  BangLine bang     -> [ColourBlock (escHTML bang)      bangline]
  ISpace   num      -> [ColourBlock (replicate num ' ') UnColoured]
  LiteralString x   -> stringcolours x
  Symbol symb       -> symbolise symb
  Brackets lvl expr -> [ColourBlock "(" (Rainbow lvl)] ++ (colourize expr) ++ [ColourBlock ")" (Rainbow lvl)]
  _                 -> [ColourBlock "" UnColoured])) code)

stringcolours :: LitS -> [ColourBlock]
stringcolours (SquareString chunk) = [ColourBlock "#[" quotes] ++ (stringchunks chunk) ++ [ColourBlock "]"  quotes]
stringcolours (QuotedString chunk) = [ColourBlock "\"" quotes] ++ (stringchunks chunk) ++ [ColourBlock "\"" quotes]

symbolise :: LitSymb -> [ColourBlock]
symbolise (BareSymbol str)     = [ColourBlock (':':str) symcolour]
symbolise (QuotedSymbol chunk) = [ColourBlock ":\"" quotes] ++ (symbolchunks chunk) ++ [ColourBlock "\"" quotes]

chunkcolours :: Colour -> Colour -> [Chunk] -> [ColourBlock]
chunkcolours litcol esccol chunks = concat $ map (\chunk -> case chunk of
  Lit         str -> [ColourBlock (escHTML str) litcol]
  Escape      str -> [ColourBlock (escHTML str) esccol]
  RawInsert   str -> [ColourBlock str litcol]
  Interpolate x   -> [ColourBlock "#{" splcolour, ColourBlock "" UnColoured, ColourBlock "}" splcolour]) chunks

stringchunks = chunkcolours litcolour esccolour
symbolchunks = chunkcolours symcolour esccolour
