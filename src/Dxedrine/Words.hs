module Dxedrine.Words where

import Data.Binary (Get, Put, getWord8, putWord8)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word16, Word8)

newtype Word7 = Word7 { unWord7 :: Word8 } deriving (Show, Eq)

word7FromIntegral :: Integral a => a -> Word7
word7FromIntegral i =
  let w8 = fromIntegral i :: Word8
  in Word7 (w8 .&. 0x7F)

word7ToInteger :: Word7 -> Integer
word7ToInteger (Word7 w8) = toInteger w8

getWord7 :: Get Word7
getWord7 = do
  x <- getWord8
  if x .&. 0x80 == 0
    then return $ Word7 x
    else fail $ "Not a Word7: " ++ show x

putWord7 :: Word7 -> Put
putWord7 (Word7 w8) = putWord8 $ 0x7F .&. w8

newtype Word14 = Word14 { unWord14 :: (Word7, Word7) } deriving (Show, Eq)

word14FromIntegral :: Integral a => a -> Word14
word14FromIntegral i =
  let w16 = fromIntegral i :: Word16
      msb8 = fromIntegral (w16 `shiftR` 7) :: Word8
      lsb8 = fromIntegral (w16 .&. 0x007F) :: Word8
  in Word14 (word7FromIntegral msb8, word7FromIntegral lsb8)

word14ToInteger :: Word14 -> Integer
word14ToInteger (Word14 (Word7 msb8, Word7 lsb8)) =
  let msb16 = fromIntegral msb8 :: Word16
      lsb16 = fromIntegral lsb8 :: Word16
      w16 = (msb16 `shiftL` 7) .|. lsb16
  in toInteger w16

getWord14 :: Get Word14
getWord14 = do
  msb <- getWord7
  lsb <- getWord7
  return $ Word14 (msb, lsb)

putWord14 :: Word14 -> Put
putWord14 (Word14 (msb, lsb)) = do
  putWord7 msb
  putWord7 lsb
