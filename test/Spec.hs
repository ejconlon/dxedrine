import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Dxedrine
import Test.Tasty
import Test.Tasty.HUnit

dxParamChangeBytes :: [Word8]
dxParamChangeBytes = [0xF0, 0x43, 0x10, 0x19, 0x4D, 0x00, 0xF7]

data Msg = Msg

dxParamChangeMsg :: DxParamChange
dxParamChangeMsg = DxParamChange
  { _dpcManfId = 0x43
  , _dpcDevice = 0x00
  , _dpcParamGroup = 0x19
  , _dpcParam = 0x4D
  , _dpcData = 0x00
  }

--dx200NativeBulkDumpBytes :: [Word8]
--dx200NativeBulkDumpBytes = [0xF0, 0x43, 0x00, 0x62, 0x00, 0x05, 0x21, 0x7F,
--                            0x00, 0x03, 0x00, 0x01, 0x0C, 0x32, 0x19, 0xF7]

--dx200NativeBulkDumpMsg :: Msg
--dx200NativeBulkDumpMsg = Msg

runGetOrError :: Get a -> BL.ByteString -> Either String a
runGetOrError g bs =
  case runGetOrFail g bs of
    Left (_, _, s) -> Left s
    Right (_, _, a) -> Right a

-- TODO change to MonadFail constraint
unwrap :: Monad m => Either String a -> m a
unwrap (Left s) = fail s
unwrap (Right a) = return a

decodes :: (Binary a, Eq a, Show a) => String -> [Word8] -> a -> TestTree
decodes name bytes msg = testCase name $ do
  decoded <- unwrap $ runGetOrError get $ BL.pack bytes
  decoded @?= msg

tests :: TestTree
tests = testGroup "Tests" [
    decodes "dx param change" dxParamChangeBytes dxParamChangeMsg
  --, parses "dx200 native bulk dump" dx200NativeBulkDumpBytes dx200NativeBulkDumpMsg
  ]

main :: IO ()
main = defaultMain tests
