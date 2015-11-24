module Chatty.LibSpec where

import ClassyPrelude

import Control.Concurrent
import Control.Concurrent.Async (race_)

import Network

import System.IO hiding (hPutStrLn, hGetLine)
import System.Posix.IO (createPipe, fdToHandle)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Chatty.Lib (runServer, receive, respond, textToMessage, messageToText, UserMessage(..), Message(..))


------------------------------------------------------------------------------
spec :: Spec
spec = describe "LibSpec" $ do

  describe "textToMessage" $ do
    it "shoul create UserText from '/msg username msg text'" $ do
      textToMessage "myUserName" "/msg foo bar bang" `shouldBe` UserMessage (UserText "foo bar bang") "myUserName"


    it "should create UserJoin message from '/join username'" $ do
      textToMessage "myUserName" "/join" `shouldBe` UserMessage UserJoin "myUserName"


    it "should create UserDisconnect message from '/quit'" $ do
      textToMessage "myUserName" "/quit" `shouldBe` UserMessage UserDisconnect "myUserName"


    it "should create UserInvalidCommand from anything else" $ do
      textToMessage "myUserName" "anything" `shouldBe` UserMessage UserInvalidCommand "myUserName"


  describe "receive" $ do
    it "should put incoming UserJoin on the TChan" $ do
      tm <- newTChanIO
      runWithLinkedHandles $ \(read, write) -> do
        hPutStr write "/join\n"
        longRunning (receive "myUserName" read tm) $ do
          msg <- atomically $ readTChan tm
          msg `shouldBe` UserMessage UserJoin "myUserName"


    it "should put incoming UserDisconnect on the TChan" $ do
      tm <- newTChanIO
      runWithLinkedHandles $ \(read, write) -> do
        hPutStr write "/quit\n"
        longRunning (receive "myUserName" read tm) $ do

          msg <- atomically $ readTChan tm
          msg `shouldBe` UserMessage UserDisconnect "myUserName"


    it "should put UserText TChan" $ do
      tm <- newTChanIO

      runWithLinkedHandles $ \(read, write) -> do
        hPutStr write "/msg Why hello there\n"
        longRunning (receive "myUserName" read tm) $ do

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


  describe "full integration" $ do
    it "should echo commands to myself" $ do
      longRunning runServer $ do
        threadDelay 10000
        me <- connectTo "127.0.0.1" (PortNumber 9000)

        hPutStrLn me ("me" :: Text)
        hPutStrLn me ("/join me" :: Text)

        l <- hGetLine me :: IO Text
        l `shouldBe` "*** User me Connected ***"

        hPutStrLn me ("/msg hello" :: Text)
        msg <- hGetLine me :: IO Text
        msg `shouldBe` "me: hello"


    it "should allow two people to connect" $ do
      longRunning runServer $ do
        threadDelay 10000
        me <- connectTo "127.0.0.1" (PortNumber 9000)
        you <- connectTo "127.0.0.1" (PortNumber 9000)

        hPutStrLn me ("me" :: Text)
        hPutStrLn you ("you" :: Text)

        hPutStrLn me ("/join" :: Text)
        hPutStrLn you ("/join" :: Text)

        m1 <- hGetLine me :: IO Text
        m1 `shouldBe` "*** User me Connected ***"

        m2 <- hGetLine me :: IO Text
        m2 `shouldBe` "*** User you Connected ***"

        y1 <- hGetLine you :: IO Text
        y1 `shouldBe` "*** User you Connected ***"


    it "should broadcast messages to all connected clients" $ do
      longRunning runServer $ do
        -- Need to wait for the server to be setup
        -- What is a better pattern here?
        -- I could use a callback function to runServer to know when I connected
        threadDelay 10000

        me <- connectTo "127.0.0.1" (PortNumber 9000)
        you <- connectTo "127.0.0.1" (PortNumber 9000)
        them <- connectTo "127.0.0.1" (PortNumber 9000)


        hPutStrLn me   ("me" :: Text)
        hPutStrLn you  ("you" :: Text)
        hPutStrLn them ("them" :: Text)

        hPutStrLn me   ("/join" :: Text)
        hPutStrLn you  ("/join" :: Text)
        hPutStrLn them ("/join" :: Text)

        -- User connect messages
        hGetLine me  :: IO Text
        hGetLine me  :: IO Text
        hGetLine me  :: IO Text

        hGetLine you :: IO Text
        hGetLine you :: IO Text

        hGetLine them :: IO Text

        hPutStrLn me ("/msg hello you" :: Text)

        -- All should get the same messages
        m <- hGetLine me   :: IO Text
        y <- hGetLine you  :: IO Text
        t <- hGetLine them :: IO Text

        m `shouldBe` y
        y `shouldBe` t

        hClose me
        hClose you
        hClose them

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
