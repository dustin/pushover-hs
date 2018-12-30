{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Default         (def)
import           Data.Text            (pack)
import           Data.Time.Clock      (UTCTime, addUTCTime, getCurrentTime)
import           System.Environment   (getArgs)

import           Network.API.Pushover

main :: IO ()
main = do
  a <- getArgs
  let [tok,usr,msg] = pack <$> a
  now <- getCurrentTime
  let m = (message tok usr msg)
          {_title="woot", _timestamp=(Just (addUTCTime (-900) now))}
  print m
  print =<< sendMessage m
