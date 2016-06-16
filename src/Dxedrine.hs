module Dxedrine where

import Control.Monad (forM_, unless)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.Word

data DxParamChange = DxParamChange
  { _dpcManf :: Word8
  , _dpcDevice :: Word8
  , _dpcParamGroup :: Word8
  , _dpcParam :: Word8
  , _dpcData :: Word8
  } deriving (Show, Eq)

data Dx200BulkDump = Dx200BulkDump
  { _d2bdManf :: Word8
  , _d2bdDevice :: Word8
  , _d2bdModel :: Word8
  , _d2bdAddr :: (Word8, Word8, Word8)
  , _d2bdData :: [Word8]
  } deriving (Show, Eq)

sysexStart :: Word8
sysexStart = 0xF0

sysexEnd :: Word8
sysexEnd = 0xF7

yamahaManf :: Word8
yamahaManf = 0x43

system1Model :: Word8
system1Model = 0x62

system2Model :: Word8
system2Model = 0x6D

instance Binary DxParamChange where
  get = do
    start <- getWord8
    unless (start == sysexStart) $ fail "must start with sysex start, 0xF0"
    manf <- getWord8
    deviceRaw <- getWord8
    unless (deviceRaw .&. 0xF0 == 0x10) $ fail "device fails 0xF0 test"
    paramGroup <- getWord8
    param <- getWord8
    dataa <- getWord8
    end <- getWord8
    unless (end == sysexEnd) $ fail "must end with sysex end, 0xF7"
    return DxParamChange
      { _dpcManf = manf
      , _dpcDevice = deviceRaw .&. 0x0F
      , _dpcParamGroup = paramGroup
      , _dpcParam = param
      , _dpcData = dataa
      }

  put dpc = do
    putWord8 sysexStart
    putWord8 $ _dpcManf dpc
    putWord8 $ ((_dpcDevice dpc) .|. 0x10)
    putWord8 $ _dpcParamGroup dpc
    putWord8 $ _dpcParam dpc
    putWord8 $ _dpcData dpc
    putWord8 sysexEnd

getN :: Get a -> Integer -> Get [a]
getN _ 0 = return []
getN g i = do
  x <- g
  xs <- getN g (i - 1)
  return $ x : xs

makeD2bdChecksum :: Dx200BulkDump -> Word8
makeD2bdChecksum d2bd =
  let dataa = _d2bdData d2bd
      count = (fromIntegral (length (dataa))) :: Word16
      countMSB = (fromIntegral (count `shiftR` 8)) :: Word8
      countLSB = (fromIntegral (count .&. 0x00FF)) :: Word8
      (addrHigh, addrMid, addrLow) = _d2bdAddr d2bd
      value = addrHigh + addrMid + addrLow + countMSB + countLSB + (sum dataa)
  in ((0xFF `xor` value) + 1) .&. 0x7F

instance Binary Dx200BulkDump where
  get = do
    start <- getWord8
    unless (start == sysexStart) $ fail "must start with sysex start, 0xF0"
    manf <- getWord8
    deviceRaw <- getWord8
    unless (deviceRaw .&. 0xF0 == 0x00) $ fail "device fails 0xF0 test"
    model <- getWord8
    count <- getWord16be
    addrHigh <- getWord8
    addrMid <- getWord8
    addrLow <- getWord8
    dataa <- getN getWord8 $ toInteger count
    checksum <- getWord8
    -- check checksum
    end <- getWord8
    unless (end == sysexEnd) $ fail "must end with sysex end, 0xF7"
    return Dx200BulkDump
      { _d2bdManf = manf
      , _d2bdDevice = deviceRaw
      , _d2bdModel = model
      , _d2bdAddr = (addrHigh, addrMid, addrLow)
      , _d2bdData = dataa
      }

  put d2bd = do
    putWord8 sysexStart
    putWord8 $ _d2bdManf d2bd
    putWord8 $ _d2bdDevice d2bd
    putWord8 $ _d2bdModel d2bd
    let dataa = _d2bdData d2bd
    let count = (fromIntegral (length dataa)) :: Word16
    putWord16be count
    let (addrHigh, addrMid, addrLow) = _d2bdAddr d2bd
    putWord8 addrHigh
    putWord8 addrMid
    putWord8 addrLow
    forM_ dataa putWord8
    putWord8 $ makeD2bdChecksum d2bd
    putWord8 sysexEnd

someFunc :: IO ()
someFunc = putStrLn "someFunc"
