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
    it "parse any string as a message" $ do
      textToMessage "foo bar bang" `shouldBe` UserMessage (UserText "foo bar bang") "unknown"


    it "should create UserJoin message from '/join anything'" $ do
      textToMessage "/join me" `shouldBe` UserMessage UserJoin "unknown"

    it "should create UserDisconnect message from '/quit'" $ do
      textToMessage "/quit anything" `shouldBe` UserMessage UserDisconnect "unknown"


  describe "receive" $ do
    it "should put incoming UserJoin on the TChan" $ do
      tm <- newTChanIO
      (read, write) <- createLinkedHandles

      hPutStr write "/join\n"

      receive read tm

      msg <- atomically $ readTChan tm
      msg `shouldBe` UserMessage UserJoin "unknown"


    it "should put incoming UserDisconnect on the TChan" $ do
      tm <- newTChanIO
      (read, write) <- createLinkedHandles

      hPutStr write "/quit\n"

      receive read tm

      msg <- atomically $ readTChan tm
      msg `shouldBe` UserMessage UserDisconnect "unknown"


    it "should put UserText TChan" $ do
      tm <- newTChanIO
      (read, write) <- createLinkedHandles

      hPutStr write "Why hello there\n"

      receive read tm

      msg <- atomically $ readTChan tm
      msg `shouldBe` UserMessage (UserText "Why hello there") "unknown"


------------------------------------------------------------------------------
-- Helpers

createLinkedHandles :: IO (Handle, Handle)
createLinkedHandles = do
   (fdRead, fdWrite) <- createPipe
   read <- fdToHandle fdRead
   write <- fdToHandle fdWrite
   hSetBuffering read LineBuffering
   hSetBuffering write LineBuffering
   return (read, write)
