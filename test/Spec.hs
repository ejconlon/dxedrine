import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Dxedrine
import Test.Tasty
import Test.Tasty.HUnit

dxParamChangeBytes :: BL.ByteString
dxParamChangeBytes = BL.pack [0xF0, 0x43, 0x10, 0x19, 0x4D, 0x00, 0xF7]

data Msg = Msg

dxParamChangeMsg :: DxParamChange
dxParamChangeMsg = DxParamChange
  { _dpcManfId = 0x43
  , _dpcDevice = 0x00
  , _dpcParamGroup = 0x19
  , _dpcParam = 0x4D
  , _dpcData = 0x00
  }

-- dx200NativeBulkDumpBytes :: BL.ByteString
-- dx200NativeBulkDumpBytes = BL.pack [
--   0xF0, 0x43, 0x00, 0x62, 0x00, 0x05, 0x21, 0x7F,
--   0x00, 0x03, 0x00, 0x01, 0x0C, 0x32, 0x19, 0xF7]

-- dx200NativeBulkDumpMsg :: Msg
-- dx200NativeBulkDumpMsg = Msg

runGetOrError :: Get a -> BL.ByteString -> Either String a
runGetOrError g bs =
  case runGetOrFail g bs of
    Left (_, _, s) -> Left s
    Right (_, _, a) -> Right a

decodes :: (Binary a, Eq a, Show a) => String -> BL.ByteString -> a -> TestTree
decodes name bytes msg = testCase ("decodes " ++ name) $ do
  let decoded = runGetOrError get bytes
  decoded @?= Right msg

encodes :: (Binary a, Eq a, Show a) => String -> BL.ByteString -> a -> TestTree
encodes name bytes msg = testCase ("encodes " ++ name) $ do
  return ()

parses :: (Binary a, Eq a, Show a) => String -> BL.ByteString -> a -> TestTree
parses name bytes msg = testGroup ("parses " ++ name)
  [ decodes name bytes msg
  , encodes name bytes msg
  ]

tests :: TestTree
tests = testGroup "Tests"
  [ parses "dx param change" dxParamChangeBytes dxParamChangeMsg
  --, parses "dx200 native bulk dump" dx200NativeBulkDumpBytes dx200NativeBulkDumpMsg
  ]

main :: IO ()
main = defaultMain tests
