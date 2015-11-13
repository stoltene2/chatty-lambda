module Main where

import Chatty.Lib
import ClassyPrelude

------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Welcome to chatty-lambda"
  putStrLn "Connect to host on port 9000"
  runServer
