module Chatty.LibSpec where

import ClassyPrelude

import Control.Concurrent.Async (race_)

import System.IO
import System.Posix.IO (createPipe, fdToHandle)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Chatty.Lib (receive, respond, textToMessage, messageToText, UserMessage(..), Message(..))


------------------------------------------------------------------------------
spec :: Spec
spec = describe "LibSpec" $ do

  describe "textToMessage" $ do
    it "shoul create UserText from '/msg username msg text'" $ do
      textToMessage "/msg myUserName foo bar bang" `shouldBe` UserMessage (UserText "foo bar bang") "myUserName"


    it "should create UserJoin message from '/join username'" $ do
      textToMessage "/join myUserName" `shouldBe` UserMessage UserJoin "myUserName"


    it "should create UserDisconnect message from '/quit'" $ do
      textToMessage "/quit myUserName" `shouldBe` UserMessage UserDisconnect "myUserName"


    it "should create UserInvalidCommand from anything else" $ do
      textToMessage "myUserName" `shouldBe` UserMessage UserInvalidCommand "unknownUser"


  describe "receive" $ do
    it "should put incoming UserJoin on the TChan" $ do
      tm <- newTChanIO
      runWithLinkedHandles $ \(read, write) -> do
        hPutStr write "/join myUserName\n"
        longRunning (receive read tm) $ do
          msg <- atomically $ readTChan tm
          msg `shouldBe` UserMessage UserJoin "myUserName"

    it "should put incoming UserDisconnect on the TChan" $ do
      tm <- newTChanIO
      runWithLinkedHandles $ \(read, write) -> do
        hPutStr write "/quit myUserName\n"
        longRunning (receive read tm) $ do

          msg <- atomically $ readTChan tm
          msg `shouldBe` UserMessage UserDisconnect "myUserName"


    it "should put UserText TChan" $ do
      tm <- newTChanIO

      runWithLinkedHandles $ \(read, write) -> do
        hPutStr write "/msg myUserName Why hello there\n"
        longRunning (receive read tm) $ do

          msg <- atomically $ readTChan tm
          msg `shouldBe` UserMessage (UserText "Why hello there") "myUserName"


  describe "messageToText" $ do
    it "should render UserMessage for text" $ do
      messageToText (UserMessage (UserText "text") "username") `shouldBe` "username: text"


    it "should render UserDisconnect" $ do
      messageToText (UserMessage UserDisconnect "username") `shouldBe` "*** User username Disconnected ***"


    it "should render UserJoin" $ do
      messageToText (UserMessage UserJoin "username") `shouldBe` "*** User username Connected ***"


    it "should render anything else as invalid command" $ do
      messageToText (UserMessage UserInvalidCommand "unknown") `shouldBe`
        "*** Unknown command ***\n" ++
        "/join <username>\n" ++
        "/msg <username> <text\n" ++
        "/quit <username>"


  describe "respond" $ do
    it "should dispatch messages from the UserMessage channel and respond over handle" $ do
      runWithLinkedHandles $ \(read, write) -> do
        tm <- newTChanIO
        atomically (writeTChan tm (UserMessage (UserText "hello") "username"))

        longRunning (respond write tm) $ do
          line <- ClassyPrelude.hGetLine read :: IO Text
          line `shouldBe` "username: hello"


------------------------------------------------------------------------------
-- Helpers

{-
longRunning is used for _long_ actions that may never return. This is
a simple wrapper around race_ so that when the test finishes the
_long_ action gets cancelled.
-}

longRunning :: IO a -> IO b -> IO ()
longRunning long test = race_ long test

{-
This takes an aciton to run where the arguments are the (read, write)
ends of a file pipe. The handles are automatically cleaned up when the
action is run. This is used for testing functions that read from file
handles.
-}
runWithLinkedHandles :: ((Handle, Handle) -> IO ()) -> IO ()
runWithLinkedHandles action = do
  bracket createLinkedHandles (\(read, write) -> hClose read >> hClose write) action


createLinkedHandles :: IO (Handle, Handle)
createLinkedHandles = do
   (fdRead, fdWrite) <- createPipe
   read <- fdToHandle fdRead
   write <- fdToHandle fdWrite
   hSetBuffering read LineBuffering
   hSetBuffering write LineBuffering
   return (read, write)
