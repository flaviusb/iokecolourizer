{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module IokeColours
(printColours) where

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
printColours the_data = printColourized (case (parseString ioke "foo" the_data) of
        Right foo -> foo
        Left _    -> [])

printColourized :: [Ioke] -> String
printColourized iks = concat (map printBlock (colourize iks))

data ColourBlock = ColourBlock String Colour

data Colour = Colour String | UnColoured | Rainbow Int

comment  = Colour "rem"
bangline = Colour "hb"


printBlock :: ColourBlock -> String
printBlock block = case block of
  ColourBlock text UnColoured    -> text
  ColourBlock text (Colour col)  -> "<div class=\"" ++ col ++ "\">" ++ text ++ "</div>"
  ColourBlock text (Rainbow lvl) -> "<b" ++ (show lvl) ++">" ++ text ++ "</b" ++ (show lvl) ++">"  -- For brackets. Use tags <b0> <b1> etc with styling in css, rather than divdivdivdiv


colourize :: [Ioke] -> [ColourBlock]
colourize = map (\ik -> (case ik of
  Comment cont  -> ColourBlock cont                comment
  Ret           -> ColourBlock "\r\n"              UnColoured
  Fullstop      -> ColourBlock "."                 UnColoured
  BangLine bang -> ColourBlock bang                bangline
  ISpace   num  -> ColourBlock (replicate num ' ') UnColoured
  _             -> ColourBlock "" UnColoured))
