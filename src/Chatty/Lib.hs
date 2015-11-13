module Chatty.Lib
    ( textToMessage
    , messageToText
    , receive
    , respond
    , runServer
    , Message (..)
    , UserMessage (..)
    , Name
    ) where


import ClassyPrelude

import qualified Data.Text as T
import qualified Data.Char as C

import Network
import System.IO (hSetBuffering, BufferMode(..))

import Control.Concurrent
import Control.Concurrent.Async (race_)

------------------------------------------------------------------------------
type Name = Text

data Message = UserText Text
             | UserDisconnect
             | UserJoin
             | UserInvalidCommand
             deriving (Show, Eq)


data UserMessage = UserMessage
                   { userMessage :: Message
                   , userMessageName :: Name
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


messageToText :: UserMessage -> Text
messageToText (UserMessage (UserText txt) name) = name ++ ": " ++ txt
messageToText (UserMessage UserDisconnect name) = "*** User " ++ name ++ " Disconnected ***"
messageToText (UserMessage UserJoin name) = "*** User " ++ name ++ " Connected ***"
messageToText _ = "*** Unknown command ***\n" ++
                  "/join <username>\n" ++
                  "/msg <username> <text\n" ++
                  "/quit <username>"


------------------------------------------------------------------------------
receive :: Handle -> TChan UserMessage -> IO ()
receive h msgq = loop
  where
    loop = do
      input <- try (hGetLine h) :: IO (Either SomeException Text)
      case input of
        Left e -> return ()
        Right t -> atomically (writeTChan msgq (textToMessage t)) >> loop

------------------------------------------------------------------------------
respond :: Handle -> TChan UserMessage -> IO ()
respond h msgq = forever $ do
  msg <- atomically (readTChan msgq)
  hPutStrLn h (messageToText msg)


------------------------------------------------------------------------------
runServer :: IO ()
runServer = withSocketsDo $ do
  bracket (listenOn (PortNumber 9000)) sClose $ \listenSock -> do

    broadcastChan <- newBroadcastTChanIO

    forever $ do
      (h, _, _) <- accept listenSock
      forkFinally (connectUser h broadcastChan) (const (hClose h))

  where
    connectUser h bc = do
      hSetBuffering h LineBuffering
      writeChan <- (atomically . dupTChan) bc
      race_ (receive h bc) (respond h writeChan)
