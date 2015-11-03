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
             | UserInvalidCommand
             deriving (Show, Eq)


data UserMessage = UserMessage
                   { userMessage :: Message
                   , userMessageName :: Text
                   } deriving (Show, Eq)

------------------------------------------------------------------------------

splitCommand :: Text -> (Text, (Text, Text))
splitCommand t =
  let
    (cmd, nameAndRest) = second T.strip (T.break C.isSpace t)
    (name, rest) = second T.strip (T.break C.isSpace nameAndRest)
  in (cmd, (name, rest))



textToMessage :: Text -> UserMessage
textToMessage (splitCommand -> ("/join", (name, _))) = UserMessage UserJoin name
textToMessage (splitCommand -> ("/quit", (name, _))) = UserMessage UserDisconnect name
textToMessage (splitCommand -> ("/msg",  (name, msg))) = UserMessage (UserText msg) name
textToMessage t = UserMessage UserInvalidCommand "unknownUser"


------------------------------------------------------------------------------
receive :: Handle -> TChan UserMessage -> IO ()
receive h tmsg = do
  input <- hGetLine h
  atomically $ writeTChan tmsg (textToMessage input)


------------------------------------------------------------------------------
