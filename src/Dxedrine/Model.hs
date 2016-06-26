module Dxedrine.Model where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless)
import Data.Binary
import Data.Binary.Get (isEmpty)
import Data.Bits ((.&.), (.|.), xor)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8(..), Word16(..))
import Dxedrine.Hlists
import Dxedrine.Parsing
import Dxedrine.Words

sysexStart :: Word8
sysexStart = 0xF0

sysexEnd :: Word8
sysexEnd = 0xF7

yamahaManf :: Word7
yamahaManf = Word7 0x43

system1Model :: Word7
system1Model = Word7 0x62

system2Model :: Word7
system2Model = Word7 0x6D

newtype Address = Address
  { unAddress :: (Word7, Word7, Word7)
  } deriving (Show, Eq)

mkAddress :: Word8 -> Word8 -> Word8 -> Address
mkAddress i8 j8 k8 =
  let i7 = word7FromIntegral i8
      j7 = word7FromIntegral j8
      k7 = word7FromIntegral k8
  in Address (i7, j7, k7)

data DxParamChange = DxParamChange
  { _dpcManf :: Word7
  , _dpcDevice :: Word7
  , _dpcParamGroup :: Word7
  , _dpcParam :: Word7
  , _dpcData :: Word7
  } deriving (Show, Eq)

data DxBulkDump = DxBulkDump
  { _dbdManf :: Word7
  , _dbdDevice :: Word7
  , _dbdFormat :: Word7
  , _dbdData :: BL.ByteString
  } deriving (Show, Eq)

data Dx200ParamChange = Dx200ParamChange
  { _d2pcManf :: Word7
  , _d2pcDevice :: Word7
  , _d2pcModel :: Word7
  , _d2pcAddr :: Address
  , _d2pcData :: BL.ByteString
  } deriving (Show, Eq)

data Dx200BulkDump = Dx200BulkDump
  { _d2bdManf :: Word7
  , _d2bdDevice :: Word7
  , _d2bdModel :: Word7
  , _d2bdAddr :: Address
  , _d2bdData :: BL.ByteString
  } deriving (Show, Eq)

data DxUnion =
    DPC  DxParamChange
  | DBD  DxBulkDump
  | D2PC Dx200ParamChange
  | D2BD Dx200BulkDump
  deriving (Show, Eq)

newtype DxUnionList = DxUnionList
  { unDxUnionList :: [DxUnion]
  } deriving (Show, Eq)

makeDbdChecksum :: DxBulkDump -> Word7
makeDbdChecksum m =
  let dataa = _dbdData m
      value = sum (BL.unpack dataa)
  in Word7 $ ((0xFF `xor` value) + 1) .&. 0x7F

makeD2bdChecksum :: Dx200BulkDump -> Word7
makeD2bdChecksum m =
  let dataa = _d2bdData m
      count = unWord14 $ word14FromIntegral (BL.length dataa)
      countMSB = unWord7 (fst count)
      countLSB = unWord7 (snd count)
      (addrHigh, addrMid, addrLow) = unAddress $ _d2bdAddr m
      value = unWord7 addrHigh +
              unWord7 addrMid +
              unWord7 addrLow +
              countMSB + countLSB + sum (BL.unpack dataa)
  in Word7 $ ((0xFF `xor` value) + 1) .&. 0x7F

instance Binary DxParamChange where
  get = do
    start <- getWord8
    unless (start == sysexStart) $ fail "no sysex start"
    manf <- getWord7
    deviceRaw <- getWord8
    unless (deviceRaw .&. 0xF0 == 0x10) $ fail "device fails 0xF0 test"
    paramGroup <- getWord7
    param <- getWord7
    dataa <- getWord7
    end <- getWord8
    unless (end == sysexEnd) $ fail "no sysex end"
    return DxParamChange
      { _dpcManf = manf
      , _dpcDevice = Word7 $ deviceRaw .&. 0x0F
      , _dpcParamGroup = paramGroup
      , _dpcParam = param
      , _dpcData = dataa
      }

  put m = do
    putWord8 sysexStart
    putWord7 $ _dpcManf m
    putWord8 $ unWord7 (_dpcDevice m) .|. 0x10
    putWord7 $ _dpcParamGroup m
    putWord7 $ _dpcParam m
    putWord7 $ _dpcData m
    putWord8 sysexEnd

instance Binary DxBulkDump where
  get = do
    start <- getWord8
    unless (start == sysexStart) $ fail "no sysex start"
    manf <- getWord7
    deviceRaw <- getWord8
    unless (deviceRaw .&. 0xF0 == 0x00) $ fail "device fails 0xF0 test"
    format <- getWord7
    count <- getWord14
    dataa <- getN getWord7 $ word14ToInteger count
    checksum <- getWord7
    end <- getWord8
    unless (end == sysexEnd) $ fail "no sysex end"
    let m = DxBulkDump
            { _dbdManf = manf
            , _dbdDevice = Word7 deviceRaw
            , _dbdFormat = format
            , _dbdData = BL.pack $ unWord7 <$> dataa
            }
        checkChecksum = makeDbdChecksum m
    if checksum == checkChecksum
      then return m
      else fail ("failed checksum: expected " ++
                 show checksum ++
                 " but was " ++
                 show checkChecksum)

  put m = do
    putWord8 sysexStart
    putWord7 $ _dbdManf m
    putWord7 $ _dbdDevice m
    putWord7 $ _dbdFormat m
    let dataa = _dbdData m
    putWord14 $ word14FromIntegral $ BL.length dataa
    forM_ (BL.unpack dataa) putWord8
    putWord7 $ makeDbdChecksum m
    putWord8 sysexEnd

instance Binary Dx200ParamChange where
  get = do
    start <- getWord8
    unless (start == sysexStart) $ fail "no sysex start"
    manf <- getWord7
    deviceRaw <- getWord8
    unless (deviceRaw .&. 0xF0 == 0x10) $ fail "device fails 0xF0 test"
    model <- getWord7
    addrHigh <- getWord7
    addrMid <- getWord7
    addrLow <- getWord7
    (dataa, end) <- getUntil getWord8 $ \x -> x == sysexEnd
    unless (end == sysexEnd) $ fail "no sysex end"
    return Dx200ParamChange
      { _d2pcManf = manf
      , _d2pcDevice = Word7 $ deviceRaw .&. 0x0F
      , _d2pcModel = model
      , _d2pcAddr = Address (addrHigh, addrMid, addrLow)
      , _d2pcData = BL.pack dataa
      }

  put m = do
    putWord8 sysexStart
    putWord7 $ _d2pcManf m
    putWord8 $ unWord7 (_d2pcDevice m) .|. 0x10
    putWord7 $ _d2pcModel m
    let (addrHigh, addrMid, addrLow) = unAddress $ _d2pcAddr m
    putWord7 addrHigh
    putWord7 addrMid
    putWord7 addrLow
    let dataa = _d2pcData m
    forM_ (BL.unpack dataa) putWord8
    putWord8 sysexEnd

instance Binary Dx200BulkDump where
  get = do
    start <- getWord8
    unless (start == sysexStart) $ fail "no sysex start"
    manf <- getWord7
    deviceRaw <- getWord8
    unless (deviceRaw .&. 0xF0 == 0x00) $ fail "device fails 0xF0 test"
    model <- getWord7
    count <- getWord14
    addrHigh <- getWord7
    addrMid <- getWord7
    addrLow <- getWord7
    dataa <- getN getWord7 $ word14ToInteger count
    checksum <- getWord7
    end <- getWord8
    unless (end == sysexEnd) $ fail "no sysex end"
    let m = Dx200BulkDump
            { _d2bdManf = manf
            , _d2bdDevice = Word7 deviceRaw
            , _d2bdModel = model
            , _d2bdAddr = Address (addrHigh, addrMid, addrLow)
            , _d2bdData = BL.pack $ unWord7 <$> dataa
            }
        checkChecksum = makeD2bdChecksum m
    if checksum == checkChecksum
      then return m
      else fail ("failed checksum: expected " ++
                 show checksum ++
                 " but was " ++
                 show checkChecksum)

  put m = do
    putWord8 sysexStart
    putWord7 $ _d2bdManf m
    putWord7 $ _d2bdDevice m
    putWord7 $ _d2bdModel m
    let dataa = _d2bdData m
    putWord14 $ word14FromIntegral $ BL.length dataa
    let (addrHigh, addrMid, addrLow) = unAddress $ _d2bdAddr m
    putWord7 addrHigh
    putWord7 addrMid
    putWord7 addrLow
    forM_ (BL.unpack dataa) putWord8
    putWord7 $ makeD2bdChecksum m
    putWord8 sysexEnd

instance Binary DxUnion where
  get = (DPC  <$> get)
    <|> (DBD  <$> get)
    <|> (D2PC <$> get)
    <|> (D2BD <$> get)

  put m = case m of
    DPC m  -> put m
    DBD m  -> put m
    D2PC m -> put m
    D2BD m -> put m

instance Binary DxUnionList where
  get = DxUnionList . reverse <$> go []
    where
      go xs = do
        e <- isEmpty
        if e
          then return xs
          else get >>= \x -> go (x : xs)

  put (DxUnionList us) = forM_ us put
