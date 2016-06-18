module TwoHundo where

import Data.Word
import Dxedrine

data DataType =
    IsWord7 Word8 Word8
  | IsWord14 Word8 Word8 Word8 Word8
  deriving (Show, Eq)

data Entry = Entry
  { _entryName :: String
  , _entryDataType :: DataType
  } deriving (Show, Eq)

data Block = Block
  { _blockAddress :: Address
  , _blockEntries :: [Maybe Entry]
  } deriving (Show, Eq)

system2Block :: Block
system2Block = Block
  { _blockAddress = mkAddress 0x00 0x00 0x07
  , _blockEntries =
    [ Nothing
    , Just Entry
      { _entryName = "Velocity Curve"
      , _entryDataType = IsWord7 0x00 0x06
      }
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Just Entry
      { _entryName = "32 Bulk Receive Block"
      , _entryDataType = IsWord7 0x00 0x03
      }
    , Nothing
    ]
  }

currentVoiceCommon1 :: Block
currentVoiceCommon1 = Block
  { _blockAddress = mkAddress 0x10 0x00 0x00
  , _blockEntries =
    [ Just Entry
      { _entryName = "Distortion: Off/On"
      , _entryDataType = IsWord7 0x00 0x01
      }
    ]
  }

{-
[ DX200 NATIVE PARAMETER CHANGE ]
1) System1 parameter change
2) System2 parameter change
3) Current Voice Common1 parameter change
4) Current Voice Common2 parameter change
5) Current Voice FreeEG parameter change
6) Currnet Voice Step Seq. parameter change
7) Currnet Rhythm Step Seq. parameter change
8) Current Effect parameter change
9) Current Mulit Part parameter change

[ DX PARAMETER CHANGE ]
1) VCED parameter change
2) ACED parameter chang

[ DX200 NATIVE BULK DUMP ]
1) System1 bulk dump
2) System2 bulk dump
3) Current Voice Common1 bulk dump
4) Current Voice Common2 bulk dump
5) Current Voice FreeEG bulk dump
6) Current Voice Scene1 bulk dump
7) Current Voice Scene2 bulk dump
8) Currnet Voice Step Seq. bulk dump
9) Currnet Rhythm Step Seq. bulk dump
10) Current Effect bulk dump
11) Current Mulit Part bulk dump
12) User Pattern Voice Common1 1...128
13) User Pattern Voice Common2 1...128
14) User Pattern Voice Scene1 1...128
15) User Pattern Voice Scene2 1...128
16) User Pattern Voice FreeEG 1...128
17) User Pattern Voice Step Seq. 1...128
18) User Pattern Effect 1 ... 128
19) User Pattern Multi Part 1 ... 128
20) User Pattern Rhythm Step Seq 1 ... 128
21) User Song 1 ... 10

[ DX BULK DUMP ]
1) VCED
2) ACED
3) VMEM
4) AMEM
-}

