{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable #-}

module IndexIokeMangling
(tweet, urltext) where
import Data.Data
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.List as List
import Text.Peggy 
import Data.Maybe

[peggy|

tweet :: String
  = '@' [A-Za-z_0-9]+ !. { "<a href=\"http://twitter.com/" ++ $1 ++"\">@" ++ $1 ++ "</a>" }

urltext :: String
  = ( "http://" { "http://" } / "https://" { "https://" } ) [^ \n\r]+ !. { "<a href=\"" ++ $1 ++ $2 ++ "\">" ++ $1 ++ $2 ++ "</a>" }

|]
