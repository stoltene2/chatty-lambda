module Chatty.Lib
    ( textToMessage
    , receive
    , Message (..)
    , UserMessage (..)
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


data UserMessage = UserMessage
                   { userMessage :: Message
                   , userMessageName :: Text
                   } deriving (Show, Eq)

------------------------------------------------------------------------------

splitCommand :: Text -> (Text, Text)
splitCommand t = second T.strip (T.break C.isSpace t)


textToMessage :: Text -> UserMessage
textToMessage (splitCommand -> ("/join", _)) = UserMessage UserJoin "unknown"
textToMessage (splitCommand -> ("/quit", _)) = UserMessage UserDisconnect "unknown"
textToMessage t = UserMessage (UserText t) "unknown"


------------------------------------------------------------------------------

receive :: Handle -> TChan UserMessage -> IO ()
receive h tmsg = do
  input <- hGetLine h
  atomically $ writeTChan tmsg (textToMessage input)
