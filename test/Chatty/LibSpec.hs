module Chatty.LibSpec where

import ClassyPrelude hiding (race_)

import Control.Concurrent
import Control.Concurrent.Async (race_)

import Network.Socket (tupleToHostAddress, close, socket, connect, PortNumber(..), SockAddr(..), Family(..), SocketType(..), Socket)
import Network.Socket.ByteString (sendAll, recv)

import qualified Data.Text.IO as TIO
import System.IO hiding (putStrLn, hPutStrLn, hGetLine, hClose, hSetBuffering)
import System.Posix.IO (createPipe, fdToHandle)

import Test.Hspec

import Chatty.Lib (runServer, startServer, receive, respond, textToMessage, messageToText, UserMessage(..), Message(..))


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
          line <- TIO.hGetLine read
          line `shouldBe` "username: hello"

    describe "full integration" $ do
      around withServerAndUsers $ do
        it "should echo commands to myself" $ \(_, me, _, _) -> do
            sendAll me "me\n"
            sendAll me "/join me\n"

            recv me 4096 `shouldReturn` "*** User me Connected ***\n"

            sendAll me "/msg hello\n"
            recv me 4096 `shouldReturn` "me: hello\n"

        it "should allow two people to connect" $ \(tid, me, you, _) -> do
            sendAll me "me\n"
            sendAll me "/join\n"
            recv me 4096 `shouldReturn` "*** User me Connected ***\n"

            sendAll you "you\n"
            sendAll you "/join\n"
            recv me 4096 `shouldReturn` "*** User you Connected ***\n"
            recv you 4096 `shouldReturn` "*** User you Connected ***\n"

        it "should broadcast messages to all connected clients" $ \(tid, me, you, them) -> do
            sendAll me   "me\n"
            sendAll me   "/join\n"

            sendAll you  "you\n"
            sendAll you  "/join\n"

            sendAll them "them\n"
            sendAll them "/join\n"

            threadDelay 10000 -- Wait 10ms for all users to connect

            -- flush User connect messages
            recv me 4096
            recv you 4096
            recv them 4096

            sendAll me "/msg hello you\n"

            -- All should get the same messages
            m <- recv me 4096
            y <- recv you 4096
            t <- recv them 4096

            m `shouldBe` y
            y `shouldBe` t

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


withServer = bracket startServer killThread

withServerDebug action = bracket
  (putStrLn "withServer: Server starting" >> startServer)
  (\tid -> putStrLn "withServer: Server stopping" >> killThread tid)
  (const (putStrLn "withServer: running action" >> action))

withServerAndUsers = bracket
  (do
      tid <- startServer
      threadDelay 10000
      me <- simpleConnect (9000 :: PortNumber)
      you <- simpleConnect (9000 :: PortNumber)
      them <- simpleConnect (9000 :: PortNumber)
      return (tid, me, you, them)
  )
  (\(tid, me, you, them) -> do
      close me
      close you
      close them
      killThread tid
  )


simpleConnect :: PortNumber -> IO Socket
simpleConnect port = do
  sock <- socket AF_INET Stream 6
  connect sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
  return sock
