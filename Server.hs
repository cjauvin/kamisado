{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson as A
import Web.Scotty as S
import Kamisado

main = S.scotty 3000 $ do
  get "/" $ do
    S.json "Hello World!?"
