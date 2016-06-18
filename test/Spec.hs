import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Dxedrine
import Test.Tasty
import Test.Tasty.HUnit

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
  , 0x03, 0x00, 0x01, 0x0C, 0x32, 0x39, 0xF7
  ]

dxBulkDumpMsg :: DxBulkDump
dxBulkDumpMsg = DxBulkDump
  { _dbdManf   = Word7 0x43
  , _dbdDevice = Word7 0x00
  , _dbdFormat = Word7 0x62
  , _dbdData   = Word7 <$> [0x03, 0x00, 0x01, 0x0C, 0x32]
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
  , _d2pcAddr   = (Word7 0x21, Word7 0x7F, Word7 0x00)
  , _d2pcData   = Word7 <$> [0x03, 0x00, 0x01, 0x0C, 0x32]
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
  , _d2bdAddr   = (Word7 0x21, Word7 0x7F, Word7 0x00)
  , _d2bdData   = Word7 <$> [0x03, 0x00, 0x01, 0x0C, 0x32]
  }

decodes :: (Binary a, Eq a, Show a) => String -> BL.ByteString -> a -> TestTree
decodes name bytes msg = testCase ("decodes " ++ name) $ do
  let decoded = runGetOrError get bytes
  decoded @?= Right msg

encodes :: Binary a => String -> BL.ByteString -> a -> TestTree
encodes name bytes msg = testCase ("encodes " ++ name) $ do
  let encoded = runPut $ put msg
  (BL.unpack encoded) @?= (BL.unpack bytes)

parses :: (Binary a, Eq a, Show a) => String -> BL.ByteString -> a -> TestTree
parses name bytes msg = testGroup ("parses " ++ name)
  [ decodes name bytes msg
  , encodes name bytes msg
  ]

testGetN :: TestTree
testGetN = testCase "getN" $
  (runGetOrError (getN getWord8 3) (BL.pack [1,2,3,4])) @?= Right [1,2,3]

testGetUntil :: TestTree
testGetUntil = testCase "getUntil" $ do
  (runGetOrError (getUntil getWord8 (== 3)) (BL.pack [])) @?= Left "not enough bytes"
  (runGetOrError (getUntil getWord8 (== 3)) (BL.pack [1,2,3,4])) @?= Right ([1,2], 3)
  (runGetOrError (getUntil getWord8 (== 3)) (BL.pack [1,2])) @?= Left "not enough bytes"

tests :: TestTree
tests = testGroup "Tests"
  [ testGetN
  , testGetUntil
  , parses "dx param change" dxParamChangeBytes dxParamChangeMsg
  , parses "dx bulk dump" dxBulkDumpBytes dxBulkDumpMsg
  , parses "dx200 param change" dx200ParamChangeBytes dx200ParamChangeMsg
  , parses "dx200 native bulk dump" dx200BulkDumpBytes dx200BulkDumpMsg
  ]

main :: IO ()
main = defaultMain tests
