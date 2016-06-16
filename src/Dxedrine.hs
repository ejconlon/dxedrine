module Dxedrine where

import Control.Monad (forM_, unless)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.Word

-- Put isn't a monad fail so...
unsafeGet :: Maybe a -> a
unsafeGet (Just a) = a
unsafeGet Nothing = undefined

newtype Word7 = Word7 { unWord7 :: Word8 } deriving (Show, Eq)

word7FromIntegral :: Integral a => a -> Maybe Word7
word7FromIntegral i =
  let w8 = fromIntegral i :: Word8
  in if w8 .&. 0x80 == 0
    then Just $ Word7 w8
    else Nothing

word7ToInteger :: Word7 -> Integer
word7ToInteger (Word7 w8) = toInteger w8

newtype Word14 = Word14 { unWord14 :: (Word7, Word7) } deriving (Show, Eq)

word14FromIntegral :: Integral a => a -> Maybe Word14
word14FromIntegral i =
  let w16 = fromIntegral i :: Word16
      msb8 = (fromIntegral (w16 `shiftR` 7)) :: Word8
      lsb8 = (fromIntegral (w16 .&. 0x00FF)) :: Word8
  in do
    msb <- word7FromIntegral msb8
    lsb <- word7FromIntegral lsb8
    return $ Word14 (msb, lsb)

word14ToInteger :: Word14 -> Integer
word14ToInteger (Word14 (Word7 msb8, Word7 lsb8)) =
  let msb16 = fromIntegral msb8 :: Word16
      lsb16 = fromIntegral lsb8 :: Word16
      w16 = (msb16 `shiftL` 7) .|. lsb16
  in toInteger w16

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
  , _dbdData :: [Word7]
  }

data Dx200ParamChange = Dx200ParamChange
  { _d2pcManf :: Word7
  , _d2pcDevice :: Word7
  , _d2pcModel :: Word7
  , _d2pcAddr :: (Word7, Word7, Word7)
  , _d2pcData :: [Word7]
  }

data Dx200BulkDump = Dx200BulkDump
  { _d2bdManf :: Word7
  , _d2bdDevice :: Word7
  , _d2bdModel :: Word7
  , _d2bdAddr :: (Word7, Word7, Word7)
  , _d2bdData :: [Word7]
  } deriving (Show, Eq)

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

getWord7 :: Get Word7
getWord7 = do
  x <- getWord8
  if (x .&. 0x80 == 0)
    then (return $ Word7 x)
    else (fail $ "Not a Word7: " ++ (show x))

putWord7 :: Word7 -> Put
putWord7 (Word7 w8) = putWord8 $ 0x7F .&. w8

getWord14 :: Get Word14
getWord14 = do
  msb <- getWord7
  lsb <- getWord7
  return $ Word14 (msb, lsb)

putWord14 :: Word14 -> Put
putWord14 (Word14 (msb, lsb)) = do
  putWord7 msb
  putWord7 lsb

getN :: Get a -> Integer -> Get [a]
getN _ 0 = return []
getN g i = do
  x <- g
  xs <- getN g (i - 1)
  return $ x : xs

getUntil :: Get a -> (a -> Bool) -> Get ([a], a)
getUntil g p = do
  e <- isEmpty
  if e
    then (fail "empty")
    else do
      f <- g
      (a, b) <- go [] f
      return (reverse a, b)
  where
    go xs z | p z = return (xs, z)
            | otherwise = g >>= go (z:xs)

runGetOrError :: Get a -> BL.ByteString -> Either String a
runGetOrError g bs =
  case runGetOrFail g bs of
    Left (_, _, s) -> Left s
    Right (_, _, a) -> Right a

-- TODO
makeDbdChecksum :: DxBulkDump -> Word7
makeDbdChecksum m = Word7 0

makeD2bdChecksum :: Dx200BulkDump -> Word7
makeD2bdChecksum m =
  let dataa = _d2bdData m
      count = (fromIntegral (length (dataa))) :: Word16
      countMSB = (fromIntegral (count `shiftR` 8)) :: Word8
      countLSB = (fromIntegral (count .&. 0x00FF)) :: Word8
      (addrHigh, addrMid, addrLow) = _d2bdAddr m
      value = (unWord7 addrHigh) +
              (unWord7 addrMid) +
              (unWord7 addrLow) +
              countMSB + countLSB + (sum (unWord7 <$> dataa))
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
    putWord8 $ (unWord7 (_dpcDevice m)) .|. 0x10
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
    -- TODO check checksum
    end <- getWord8
    unless (end == sysexEnd) $ fail "no sysex end"
    return DxBulkDump
      { _dbdManf = manf
      , _dbdDevice = Word7 deviceRaw
      , _dbdFormat = format
      , _dbdData = dataa
      }

  put m = do
    putWord8 sysexStart
    putWord7 $ _dbdManf m
    putWord7 $ _dbdDevice m
    putWord7 $ _dbdFormat m
    let dataa = _dbdData m
    putWord14 $ unsafeGet $ word14FromIntegral $ length dataa
    forM_ dataa putWord7
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
      , _d2pcAddr = (addrHigh, addrMid, addrLow)
      , _d2pcData = Word7 <$> dataa
      }

  put m = do
    putWord8 sysexStart
    putWord7 $ _d2pcManf m
    putWord8 $ unWord7 (_d2pcDevice m) .|. 0x10
    putWord7 $ _d2pcModel m
    let (addrHigh, addrMid, addrLow) = _d2pcAddr m
    putWord7 addrHigh
    putWord7 addrMid
    putWord7 addrLow
    let dataa = _d2pcData m
    forM_ dataa putWord7
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
    -- TODO check checksum
    end <- getWord8
    unless (end == sysexEnd) $ fail "no sysex end"
    return Dx200BulkDump
      { _d2bdManf = manf
      , _d2bdDevice = Word7 $ deviceRaw
      , _d2bdModel = model
      , _d2bdAddr = (addrHigh, addrMid, addrLow)
      , _d2bdData = dataa
      }

  put m = do
    putWord8 sysexStart
    putWord7 $ _d2bdManf m
    putWord7 $ _d2bdDevice m
    putWord7 $ _d2bdModel m
    let dataa = _d2bdData m
    putWord14 $ unsafeGet $ word14FromIntegral $ length dataa
    let (addrHigh, addrMid, addrLow) = _d2bdAddr m
    putWord7 addrHigh
    putWord7 addrMid
    putWord7 addrLow
    forM_ dataa putWord7
    putWord7 $ makeD2bdChecksum m
    putWord8 sysexEnd

blIsEmpty :: BL.ByteString -> Bool
blIsEmpty s = BL.uncons s == Nothing

getRepeated :: Get a -> BL.ByteString -> [a]
getRepeated g s =
  if blIsEmpty s
    then []
    else
      case runGetOrFail g s of
        Left (t, _, _) -> getRepeated g t
        Right (t, _, a) -> a : getRepeated g t

--getFramed :: Word8 -> Word8 -> Get a -> Get [BL.ByteString]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
