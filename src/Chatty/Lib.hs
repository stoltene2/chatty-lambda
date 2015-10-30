module Chatty.Lib
    ( textToMessage
    , Message (..)
    , Name
    ) where


import ClassyPrelude

import qualified Data.Text as T
import qualified Data.Char as C

------------------------------------------------------------------------------
type Name = Text

data Message = UserText Text
             | UserDisconnect
             | UserJoin
             deriving (Show, Eq)


------------------------------------------------------------------------------

splitCommand :: Text -> (Text, Text)
splitCommand t = second T.strip (T.break C.isSpace t)


textToMessage :: Text -> Message
textToMessage (splitCommand -> ("/join", _)) = UserJoin
textToMessage (splitCommand -> ("/quit", _)) = UserDisconnect
textToMessage t = UserText t
