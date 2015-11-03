module Chatty.LibSpec where

import ClassyPrelude

import Chatty.Lib (receive, textToMessage, UserMessage(..), Message(..))

import System.IO
import System.Posix.IO (createPipe, fdToHandle)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

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
        receive read tm

        msg <- atomically $ readTChan tm
        msg `shouldBe` UserMessage UserJoin "myUserName"

    it "should put incoming UserDisconnect on the TChan" $ do
      tm <- newTChanIO
      runWithLinkedHandles $ \(read, write) -> do
        hPutStr write "/quit myUserName\n"
        receive read tm

        msg <- atomically $ readTChan tm
        msg `shouldBe` UserMessage UserDisconnect "myUserName"


    it "should put UserText TChan" $ do
      tm <- newTChanIO

      runWithLinkedHandles $ \(read, write) -> do
        hPutStr write "/msg myUserName Why hello there\n"
        receive read tm

        msg <- atomically $ readTChan tm
        msg `shouldBe` UserMessage (UserText "Why hello there") "myUserName"


------------------------------------------------------------------------------
-- Helpers


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
