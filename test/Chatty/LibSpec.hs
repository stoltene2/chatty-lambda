module Chatty.LibSpec where

import ClassyPrelude

import Chatty.Lib (receive, textToMessage, Message(..))

import System.IO
import System.Posix.IO (createPipe, fdToHandle)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

------------------------------------------------------------------------------
spec :: Spec
spec = describe "LibSpec" $ do

  describe "textToMessage" $ do
    it "parse any string as a message" $ do
      textToMessage "foo bar bang" `shouldBe` (UserText "foo bar bang")


    it "should create UserJoin message from '/join anything'" $ do
      textToMessage "/join me" `shouldBe` UserJoin

    it "should create UserDisconnect message from '/quit'" $ do
      textToMessage "/quit anything" `shouldBe` UserDisconnect


  describe "receive" $ do
    it "should put incoming UserJoin on the TChan" $ do
      tm <- newTChanIO
      (read, write) <- createLinkedHandles

      hPutStr write "/join\n"

      receive read tm

      msg <- atomically $ readTChan tm
      msg `shouldBe` UserJoin

    it "should put incoming UserDisconnect on the TChan" $ do
      tm <- newTChanIO
      (read, write) <- createLinkedHandles

      hPutStr write "/quit\n"

      receive read tm

      msg <- atomically $ readTChan tm
      msg `shouldBe` UserDisconnect

    it "should put UserText TChan" $ do
      tm <- newTChanIO
      (read, write) <- createLinkedHandles

      hPutStr write "Why hello there\n"

      receive read tm

      msg <- atomically $ readTChan tm
      msg `shouldBe` (UserText "Why hello there")


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
