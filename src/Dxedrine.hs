module Dxedrine where

import Control.Monad (unless)
import Data.Binary
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.Word

data DxParamChange = DxParamChange
  { _dpcManfId :: Word8
  , _dpcDevice :: Word8
  , _dpcParamGroup :: Word8
  , _dpcParam :: Word8
  , _dpcData :: Word8
  } deriving (Show, Eq)

data Dx200BulkDump = Dx200BulkDump
  { _d2bdManfId :: Word8
  , _d2bdDevice :: Word8
  , _d2bdModelId :: Word8
  , _d2bdAddr :: (Word8, Word8, Word8)
  , _d2bdData :: [Word8]
  } deriving (Show, Eq)

sysexStart :: Word8
sysexStart = 0xF0

sysexEnd :: Word8
sysexEnd = 0xF7

yamahaMfrId :: Word8
yamahaMfrId = 0x43

system1ModelId :: Word8
system1ModelId = 0x62

system2ModelId :: Word8
system2ModelId = 0x6D

instance Binary DxParamChange where
  get = do
    start <- getWord8
    unless (start == sysexStart) $ fail "must start with sysex start, 0xF0"
    manfId <- getWord8
    deviceRaw <- getWord8
    unless (deviceRaw .&. 0x10 == 0x10) $ fail "device fails 0x10 test"
    paramGroup <- getWord8
    param <- getWord8
    dataa <- getWord8
    end <- getWord8
    unless (end == sysexEnd) $ fail "must end with sysex end, 0xF7"
    return DxParamChange
      { _dpcManfId = manfId
      , _dpcDevice = deviceRaw .&. 0x0F
      , _dpcParamGroup = paramGroup
      , _dpcParam = param
      , _dpcData = dataa
      }

  put dpc = do
    putWord8 sysexStart
    putWord8 $ _dpcManfId dpc
    putWord8 $ ((_dpcDevice dpc) .|. 0x10)
    putWord8 $ _dpcParamGroup dpc
    putWord8 $ _dpcParam dpc
    putWord8 $ _dpcData dpc
    putWord8 sysexEnd

instance Binary Dx200BulkDump where
  get = do
    undefined

  put d2nbd = do
    undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"
