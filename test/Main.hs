module Main (main) where

import Data.Binary (Binary (..), getWord8)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Dxedrine.Hlists
import Dxedrine.Model
import Dxedrine.Parsing
import Dxedrine.Words
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

dxParamChangeBytes :: BL.ByteString
dxParamChangeBytes = BL.pack
  [ 0xF0, 0x43, 0x10, 0x19, 0x4D, 0x00, 0xF7
  ]

dxParamChangeMsg :: DxParamChange
dxParamChangeMsg = DxParamChange
  { _dpcManf       = Word7 0x43
  , _dpcDevice     = Word7 0x00
  , _dpcParamGroup = Word7 0x19
  , _dpcParam      = Word7 0x4D
  , _dpcData       = Word7 0x00
  }

dxBulkDumpBytes :: BL.ByteString
dxBulkDumpBytes = BL.pack
  [ 0xF0, 0x43, 0x00, 0x62, 0x00, 0x05
  , 0x03, 0x00, 0x01, 0x0C, 0x32, 0x3E, 0xF7
  ]

dxBulkDumpMsg :: DxBulkDump
dxBulkDumpMsg = DxBulkDump
  { _dbdManf   = Word7 0x43
  , _dbdDevice = Word7 0x00
  , _dbdFormat = Word7 0x62
  , _dbdData   = BS.pack [0x03, 0x00, 0x01, 0x0C, 0x32]
  }

dx200ParamChangeBytes :: BL.ByteString
dx200ParamChangeBytes = BL.pack
  [ 0xF0, 0x43, 0x10, 0x62, 0x21, 0x7F
  , 0x00, 0x03, 0x00, 0x01, 0x0C, 0x32, 0xF7
  ]

dx200ParamChangeMsg :: Dx200ParamChange
dx200ParamChangeMsg = Dx200ParamChange
  { _d2pcManf   = Word7 0x43
  , _d2pcDevice = Word7 0x00
  , _d2pcModel  = Word7 0x62
  , _d2pcAddr   = Address (Word7 0x21, Word7 0x7F, Word7 0x00)
  , _d2pcData   = BS.pack [0x03, 0x00, 0x01, 0x0C, 0x32]
  }

dx200BulkDumpBytes :: BL.ByteString
dx200BulkDumpBytes = BL.pack
  [ 0xF0, 0x43, 0x00, 0x62, 0x00, 0x05, 0x21, 0x7F
  , 0x00, 0x03, 0x00, 0x01, 0x0C, 0x32, 0x19, 0xF7
  ]

dx200BulkDumpMsg :: Dx200BulkDump
dx200BulkDumpMsg = Dx200BulkDump
  { _d2bdManf   = Word7 0x43
  , _d2bdDevice = Word7 0x00
  , _d2bdModel  = Word7 0x62
  , _d2bdAddr   = Address (Word7 0x21, Word7 0x7F, Word7 0x00)
  , _d2bdData   = BS.pack [0x03, 0x00, 0x01, 0x0C, 0x32]
  }

dxPackedBytes :: BL.ByteString
dxPackedBytes =
  dxParamChangeBytes <>
  dxBulkDumpBytes <>
  dx200ParamChangeBytes <>
  dx200BulkDumpBytes

dxPackedMsgs :: DxUnionList
dxPackedMsgs = DxUnionList
  [ DPC  dxParamChangeMsg
  , DBD  dxBulkDumpMsg
  , D2PC dx200ParamChangeMsg
  , D2BD dx200BulkDumpMsg
  ]

decodes :: (Binary a, Eq a, Show a) => String -> BL.ByteString -> a -> TestTree
decodes name bytes msg = testCase ("decodes " ++ name) $ do
  let decoded = runGetOrError get bytes
  decoded @?= Right msg

encodes :: Binary a => String -> BL.ByteString -> a -> TestTree
encodes name bytes msg = testCase ("encodes " ++ name) $ do
  let encoded = runPut $ put msg
  BL.unpack encoded @?= BL.unpack bytes

parses :: (Binary a, Eq a, Show a) => String -> BL.ByteString -> a -> TestTree
parses name bytes msg = testGroup ("parses " ++ name)
  [ decodes name bytes msg
  , encodes name bytes msg
  ]

testGetN :: TestTree
testGetN = testCase "getN" $
  runGetOrError (getN getWord8 3) (BL.pack [1,2,3,4]) @?= Right [1,2,3]

testGetUntil :: TestTree
testGetUntil = testCase "getUntil" $ do
  runGetOrError (getUntil getWord8 (== 3)) (BL.pack []) @?= Left "not enough bytes"
  runGetOrError (getUntil getWord8 (== 3)) (BL.pack [1,2,3,4]) @?= Right ([1,2], 3)
  runGetOrError (getUntil getWord8 (== 3)) (BL.pack [1,2]) @?= Left "not enough bytes"

oneEntry :: [Entry]
oneEntry =
  [ entry (OneR 0x00 0x60) (oneV 0x10) "one"
  ]

twoEntry :: [Entry]
twoEntry =
  [ entry (TwoR (OneR 0x00 0x01) (OneR 0x00 0x7F)) (twoV 0x3C) "two"
  ]

enumEntry :: [Entry]
enumEntry =
  [ entry (EnumR [0x01, 0x03]) (oneV 0x03) "enum"
  ]

multiEntry :: [Entry]
multiEntry =
  [ entry (MultiR (OneR 0x00 0x60) (EnumR [0x70, 0x80])) (oneV 0x10) "multi"
  ]

ignoreEntry :: [Entry]
ignoreEntry =
  [ reserved 2
  ]

testDefaultHlist :: TestTree
testDefaultHlist = testCase "defaultHlist" $ do
  defaultHlist oneEntry @?= Hlist [("one", oneV 0x10)]
  defaultHlist twoEntry @?= Hlist [("two", twoV 0x3C)]
  defaultHlist enumEntry @?= Hlist [("enum", oneV 0x03)]
  defaultHlist multiEntry @?= Hlist [("multi", oneV 0x10)]
  defaultHlist ignoreEntry @?= Hlist []

testPackHlist :: TestTree
testPackHlist = testCase "packHlist" $ do
  packHlist oneEntry (Hlist []) @?= Left "field \"one\" missing"
  packHlist oneEntry (Hlist [("one", oneV 0x12)]) @?= Right [Word7 0x12]
  packHlist oneEntry (Hlist [("one", oneV 0x70)]) @?= Left "field \"one\" invalid: 112 outside range [0, 96]"
  packHlist twoEntry (Hlist []) @?= Left "field \"two\" missing"
  packHlist twoEntry (Hlist [("two", twoV 0x34)]) @?= Right [Word7 0x00, Word7 0x34]
  packHlist enumEntry (Hlist []) @?= Left "field \"enum\" missing"
  packHlist enumEntry (Hlist [("enum", oneV 0x01)]) @?= Right [Word7 0x01]
  packHlist multiEntry (Hlist []) @?= Left "field \"multi\" missing"
  packHlist multiEntry (Hlist [("multi", oneV 0x09)]) @?= Right [Word7 0x09]
  packHlist multiEntry (Hlist [("multi", oneV 0x70)]) @?= Right [Word7 0x70]
  packHlist ignoreEntry (Hlist []) @?= Right []
  packHlist (oneEntry ++ twoEntry) (Hlist [("one", oneV 0x12), ("two", twoV 0x34)]) @?= Right [Word7 0x12, Word7 0x00, Word7 0x34]

testUnpackHlist :: TestTree
testUnpackHlist = testCase "unpackHlist" $ do
  unpackHlist ignoreEntry [] @?= Left "not enough bytes"
  unpackHlist ignoreEntry [Word7 1] @?= Left "not enough bytes"
  unpackHlist ignoreEntry [Word7 1, Word7 2] @?= Right (Hlist [], [])
  unpackHlist ignoreEntry [Word7 1, Word7 2, Word7 3] @?= Right (Hlist [], [Word7 3])
  unpackHlist oneEntry [] @?= Left "not enough bytes"
  unpackHlist oneEntry [Word7 1] @?= Right (Hlist [("one", oneV 1)], [])
  unpackHlist oneEntry [Word7 1, Word7 2] @?= Right (Hlist [("one", oneV 1)], [Word7 2])
  unpackHlist oneEntry [Word7 0x70] @?= Left "112 outside range [0, 96]"
  unpackHlist twoEntry [] @?= Left "not enough bytes"
  unpackHlist twoEntry [Word7 0x00] @?= Left "not enough bytes"
  unpackHlist twoEntry [Word7 0x00, Word7 0x34] @?= Right (Hlist [("two", twoV 0x34)], [])
  unpackHlist twoEntry [Word7 0x00, Word7 0x34, Word7 0x55] @?= Right (Hlist [("two", twoV 0x34)], [Word7 0x55])
  unpackHlist twoEntry [Word7 0xF7, Word7 0x34] @?= Left "Not a Word7: 247"
  unpackHlist enumEntry [] @?= Left "not enough bytes"
  unpackHlist enumEntry [Word7 0x01] @?= Right (Hlist [("enum", oneV 0x01)], [])
  unpackHlist enumEntry [Word7 0x01, Word7 0x55] @?= Right (Hlist [("enum", oneV 0x01)], [Word7 0x55])
  unpackHlist enumEntry [Word7 0x55] @?= Left "85 not an element of [1,3]"
  unpackHlist (oneEntry ++ enumEntry) [Word7 0x01, Word7 0x03, Word7 0x66] @?= Right (Hlist [("one", oneV 0x01), ("enum", oneV 0x03)], [Word7 0x66])
  unpackHlist multiEntry [] @?= Left "not enough bytes"
  unpackHlist multiEntry [Word7 0x09] @?= Right (Hlist [("multi", oneV 0x09)], [])
  unpackHlist multiEntry [Word7 0x70] @?= Right (Hlist [("multi", oneV 0x70)], [])
  unpackHlist multiEntry [Word7 0x71] @?= Left "both 113 outside range [0, 96] and 113 not an element of [112,128]"

tests :: TestTree
tests = testGroup "Tests"
  [ testGetN
  , testGetUntil
  , testDefaultHlist
  , testPackHlist
  , testUnpackHlist
  , parses "dx param change" dxParamChangeBytes dxParamChangeMsg
  , parses "dx bulk dump" dxBulkDumpBytes dxBulkDumpMsg
  , parses "dx200 param change" dx200ParamChangeBytes dx200ParamChangeMsg
  , parses "dx200 native bulk dump" dx200BulkDumpBytes dx200BulkDumpMsg
  , parses "union1" dxParamChangeBytes (DPC dxParamChangeMsg)
  , parses "union2" dx200BulkDumpBytes (D2BD dx200BulkDumpMsg)
  , parses "single" dxParamChangeBytes (DxUnionList [DPC dxParamChangeMsg])
  , parses "another" dx200BulkDumpBytes (DxUnionList [D2BD dx200BulkDumpMsg])
  , parses "packed" dxPackedBytes dxPackedMsgs
  ]

main :: IO ()
main = defaultMain tests
