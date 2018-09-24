{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (helloWorld)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/metrics" $ text helloWorld
