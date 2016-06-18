module TwoHundo where

import Data.Word
import Dxedrine

data DataType =
    Ignore Int
  | One Word8 Word8
  | Two Word8 Word8 Word8 Word8
  deriving (Show, Eq)

data Entry = Entry
  { _entryName :: String
  , _entryDataType :: DataType
  } deriving (Show, Eq)

data Block = Block
  { _blockName :: String
  , _blockAddress :: Address
  , _blockEntries :: [Entry]
  } deriving (Show, Eq)

reserved :: Int -> Entry
reserved i = Entry "reserved" (Ignore i)

entry :: DataType -> String -> Entry
entry = flip Entry

system2Block :: Block
system2Block = Block
  { _blockName = "system2"
  , _blockAddress = mkAddress 0x00 0x00 0x07
  , _blockEntries =
    [ reserved 1
    , entry (One 0x00 0x06) "velocityCurve"
    , reserved 1
    , reserved 1
    , reserved 1
    , reserved 1
    , reserved 1
    , entry (One 0x00 0x03) "bulkReceiveBlock"
    , reserved 1
    ]
  }

currentCommonVoice1Block :: Block
currentCommonVoice1Block = Block
  { _blockName = "currentCommonVoice1"
  , _blockAddress = mkAddress 0x10 0x00 0x00
  , _blockEntries =
    [ entry (One 0x00 0x01) "distortionOffOn"
    , entry (One 0x00 0x64) "distortionDrive"
    , entry (One 0x00 0x03) "distortionAmpType"
    , entry (One 0x22 0x3C) "distortionLpfCutoff"
    , entry (One 0x00 0x64) "distortionOutLevel"
    , entry (One 0x01 0x7F) "distortionDryWet"
    , entry (One 0x04 0x28) "eqLowFreq"
    , entry (One 0x34 0x4C) "eqLowGain"
    , entry (One 0x0E 0x36) "eqMidFreq"
    , entry (One 0x34 0x4C) "eqMidGain"
    , entry (One 0x0A 0x78) "eqMidResonance"
    , reserved 1
    , entry (One 0x00 0x7F) "filterCutoff"
    , entry (One 0x00 0x74) "filterResonance"
    , entry (One 0x00 0x05) "filterType"
    , entry (One 0x00 0x7F) "filterCutoffScalingDepth"
    , entry (One 0x00 0x63) "filterCutoffModulationDepth"
    , entry (One 0x34 0x4C) "filterInputGain"
    , entry (One 0x00 0x7F) "fegAttack"
    , entry (One 0x00 0x7F) "fegDecay"
    , entry (One 0x00 0x7F) "fegSustain"
    , entry (One 0x00 0x7F) "fegRelease"
    , entry (One 0x00 0x7F) "fegDepth"
    , entry (One 0x00 0x7F) "fegDepthVelocitySense"
    , reserved 1
    , entry (One 0x00 0x0F) "noiseOscType"
    , entry (One 0x00 0x7F) "mixerVoiceLevel"
    , entry (One 0x00 0x7F) "mixerNoiseLevel"
    , entry (One 0x00 0x7F) "modulator1Harmonic"
    , entry (One 0x00 0x7F) "modulator2Harmonic"
    , entry (One 0x00 0x7F) "modulator3Harmonic"
    , entry (One 0x00 0x7F) "modulator1FmDepth"
    , entry (One 0x00 0x7F) "modulator2FmDepth"
    , entry (One 0x00 0x7F) "modulator3FmDepth"
    , entry (One 0x00 0x7F) "modulator1EgDecay"
    , entry (One 0x00 0x7F) "modulator2EgDecay"
    , entry (One 0x00 0x7F) "modulator3EgDecay"
    , entry (One 0x00 0x7F) "aegAttack"
    , entry (One 0x00 0x7F) "aegDecay"
    , entry (One 0x00 0x7F) "aegSustain"
    , entry (One 0x00 0x7F) "aegRelease"
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

