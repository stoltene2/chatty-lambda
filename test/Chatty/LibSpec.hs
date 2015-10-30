module Chatty.LibSpec where

import ClassyPrelude

import Chatty.Lib

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
