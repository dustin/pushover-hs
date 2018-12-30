{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson             (eitherDecode)
import qualified Data.ByteString.Lazy   as LB
import           Data.Text              (Text, pack, toLower, unpack)
import           Data.Time              (UTCTime)
import           Data.Time.Clock.POSIX  (utcTimeToPOSIXSeconds)
import           Data.Time.Clock.System (SystemTime (..), systemToUTCTime)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.API.Pushover

testRcptParse :: Assertion
testRcptParse = assertEqual "" (Right (Receipt {_acknowledged = Just (Acknowledgment {_ackAt = ts 1546120551,
                                                                                      _ackBy = "someuser",
                                                                                      _ackOn = "somedev"}),
                                                _lastDeliveredAt = Just (ts 1546120527),
                                                _expired = True, _expiresAt = Just (ts 1546124097),
                                                _calledBackAt = Nothing}))
                (d $ mconcat ["{\"status\":1,\"acknowledged\":1,",
                              "\"acknowledged_at\":1546120551,",
                              "\"acknowledged_by\":\"someuser\",",
                              "\"acknowledged_by_device\":\"somedev\",",
                              "\"last_delivered_at\":1546120527,",
                              "\"expired\":1,\"expires_at\":1546124097,",
                              "\"called_back\":0,\"called_back_at\":0,",
                              "\"request\":\"reqid\"}"])

  where d :: LB.ByteString -> Either String Receipt
        d = eitherDecode

        ts = systemToUTCTime . flip MkSystemTime 0

testGoodParse :: Assertion
testGoodParse = assertEqual "" (Right (Response{_status=1,
                                                _request="647d2300-702c-4b38-8b2f-d56326ae460b",
                                                _receipt="",
                                                _errors=[]}))
                (parseResponse "{\"status\":1,\"request\":\"647d2300-702c-4b38-8b2f-d56326ae460b\"}")

testGoodParseRcpt :: Assertion
testGoodParseRcpt = assertEqual "" (Right (Response{_status=1,
                                                    _request="647d2300-702c-4b38-8b2f-d56326ae460b",
                                                    _receipt="thisisareceipt",
                                                    _errors=[]}))
                (parseResponse "{\"status\":1,\"request\":\"647d2300-702c-4b38-8b2f-d56326ae460b\",\"receipt\":\"thisisareceipt\"}")

testErrorParse :: Assertion
testErrorParse = assertEqual "" (Left (Response{_status=0,
                                                _request="5042853c-402d-4a18-abcb-168734a801de",
                                                _receipt="",
                                                _errors=["user identifier is invalid"]}))
                (parseResponse "{\"user\":\"invalid\",\"errors\":[\"user identifier is invalid\"],\"status\":0,\"request\":\"5042853c-402d-4a18-abcb-168734a801de\"}")


tests :: [TestTree]
tests = [
  testCase "good response parsing" testGoodParse,
  testCase "good response parsing w/ receipt" testGoodParseRcpt,
  testCase "error response parsing" testErrorParse,
  testCase "parse receipt" testRcptParse
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
