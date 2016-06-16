import Dxedrine
import Test.Tasty
import Test.Tasty.HUnit

dxParamChangeBytes :: [Word]
dxParamChangeBytes = [0xF0, 0x43, 0x10, 0x19, 0x4D, 0x00, 0xF7]

data Msg = Msg

dxParamChangeMsg :: Msg
dxParamChangeMsg = Msg

dx200NativeBulkDumpBytes :: [Word]
dx200NativeBulkDumpBytes = [0xF0, 0x43, 0x00, 0x62, 0x00, 0x05, 0x21, 0x7F,
                            0x00, 0x03, 0x00, 0x01, 0x0C, 0x32, 0x19, 0xF7]

dx200NativeBulkDumpMsg :: Msg
dx200NativeBulkDumpMsg = Msg

parses :: String -> [Word] -> Msg -> TestTree
parses name bytes msg = testCase name $ do
  return ()

tests :: TestTree
tests = testGroup "Tests" [
    parses "dx param change" dxParamChangeBytes dxParamChangeMsg
  , parses "dx200 native bulk dump" dx200NativeBulkDumpBytes dx200NativeBulkDumpMsg
  ]

main :: IO ()
main = defaultMain tests
