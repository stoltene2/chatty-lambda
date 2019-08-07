{-# LANGUAGE OverloadedStrings #-}
module Chatty.Lib
    ( textToMessage
    , messageToText
    , receive
    , respond
    , runServer
    , startServer
    , Message (..)
    , UserMessage (..)
    , Name
    ) where


import ClassyPrelude hiding (hSetBuffering, race_)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char as C

import Network.Socket
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

splitCommand :: Text -> (Text, Text)
splitCommand t = second T.strip (T.break C.isSpace t)


textToMessage :: Name -> Text -> UserMessage
textToMessage name (splitCommand -> ("/join",  _)) = UserMessage UserJoin name
textToMessage name (splitCommand -> ("/quit",  _)) = UserMessage UserDisconnect name
textToMessage name (splitCommand -> ("/msg", msg)) = UserMessage (UserText msg) name
textToMessage name t = UserMessage UserInvalidCommand name


messageToText :: UserMessage -> Text
messageToText (UserMessage (UserText txt) name) = name ++ ": " ++ txt
messageToText (UserMessage UserDisconnect name) = "*** User " ++ name ++ " Disconnected ***"
messageToText (UserMessage UserJoin name) = "*** User " ++ name ++ " Connected ***"
messageToText _ = "*** Unknown command ***\n" ++
                  "/join <username>\n" ++
                  "/msg <username> <text\n" ++
                  "/quit <username>"


------------------------------------------------------------------------------
receive :: Name -> Handle -> TChan UserMessage -> IO ()
receive name h msgq = loop
  where
    loop = do
      input <- try (TIO.hGetLine h) :: IO (Either SomeException Text)
      case input of
        Left e -> return ()
        Right t -> atomically (writeTChan msgq (textToMessage name t)) >> loop

------------------------------------------------------------------------------
respond :: Handle -> TChan UserMessage -> IO ()
respond h msgq = forever $ do
  msg <- atomically (readTChan msgq)
  TIO.hPutStrLn h (messageToText msg)


------------------------------------------------------------------------------
runServer :: IO ()
runServer = withSocketsDo $ do
  bracket mkListenSocket close $ \serverSocket -> do

    broadcastChan <- newBroadcastTChanIO
    forever $ do
      (userSocket, _) <- accept serverSocket
      hUser <- socketToHandle userSocket ReadWriteMode
      hSetBuffering hUser LineBuffering

      forkFinally (connectUser hUser broadcastChan) (\_ -> hClose hUser)

  where
    connectUser h bc = do
      name <- TIO.hGetLine h
      writeChan <- (atomically . dupTChan) bc
      race_ (receive name h bc) (respond h writeChan)

    mkListenSocket :: IO Socket
    mkListenSocket = do
      let addr = SockAddrInet
                 (9000 :: PortNumber)
                 (tupleToHostAddress (0, 0, 0, 0))
      s <- socket AF_INET Stream 6 {-tcp-}
      setSocketOption s ReuseAddr 1
      bind s addr
      listen s 128
      return s

startServer :: IO ThreadId
startServer = forkIO runServer
