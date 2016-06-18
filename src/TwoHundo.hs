module TwoHundo where

import Data.Bits ((.&.))
import Data.Word
import Dxedrine

data Range =
    IgnoreR Int
  | OneR Word8 Word8
  | TwoR Range Range
  | EnumR [Word8]
  | MultiR Range Range
  deriving (Show, Eq)

data Value =
    IgnoreV
  | OneV Word8
  | TwoV Word16
  deriving (Show, Eq)

data Entry = Entry
  { _entryName :: String
  , _entryRange :: Range
  , _entryDefault :: Value
  } deriving (Show, Eq)

data Block = Block
  { _blockName :: String
  , _blockAddress :: Address
  , _blockEntries :: [Entry]
  } deriving (Show, Eq)

reserved :: Int -> Entry
reserved i = Entry "reserved" (IgnoreR i) IgnoreV

entry :: Range -> Value -> String -> Entry
entry range value name = Entry name range value

system2Block :: Block
system2Block = Block
  { _blockName = "system2"
  , _blockAddress = mkAddress 0x00 0x00 0x07
  , _blockEntries =
    [ reserved 1
    , entry (OneR 0x00 0x06) (OneV 0x01) "velocityCurve"
    , reserved 5
    , entry (OneR 0x00 0x03) (OneV 0x00) "bulkReceiveBlock"
    , reserved 1
    ]
  }

voiceCommon1Entries :: [Entry]
voiceCommon1Entries =
  [ entry (OneR 0x00 0x01) (OneV 0x01) "distortionOffOn"
  , entry (OneR 0x00 0x64) (OneV 0x40) "distortionDrive"
  , entry (OneR 0x00 0x03) (OneV 0x01) "distortionAmpType"
  , entry (OneR 0x22 0x3C) (OneV 0x30) "distortionLpfCutoff"
  , entry (OneR 0x00 0x64) (OneV 0x3C) "distortionOutLevel"
  , entry (OneR 0x01 0x7F) (OneV 0x01) "distortionDryWet"
  , entry (OneR 0x04 0x28) (OneV 0x11) "eqLowFreq"
  , entry (OneR 0x34 0x4C) (OneV 0x40) "eqLowGain"
  , entry (OneR 0x0E 0x36) (OneV 0x28) "eqMidFreq"
  , entry (OneR 0x34 0x4C) (OneV 0x40) "eqMidGain"
  , entry (OneR 0x0A 0x78) (OneV 0x0A) "eqMidResonance"
  , reserved 1
  , entry (OneR 0x00 0x7F) (OneV 0x7F) "filterCutoff"
  , entry (OneR 0x00 0x74) (OneV 0x10) "filterResonance"
  , entry (OneR 0x00 0x05) (OneV 0x00) "filterType"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "filterCutoffScalingDepth"
  , entry (OneR 0x00 0x63) (OneV 0x00) "filterCutoffModulationDepth"
  , entry (OneR 0x34 0x4C) (OneV 0x40) "filterInputGain"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "fegAttack"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "fegDecay"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "fegSustain"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "fegRelease"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "fegDepth"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "fegDepthVelocitySense"
  , reserved 1
  , entry (OneR 0x00 0x0F) (OneV 0x00) "noiseOscType"
  , entry (OneR 0x00 0x7F) (OneV 0x7F) "mixerVoiceLevel"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "mixerNoiseLevel"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator1Harmonic"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator2Harmonic"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator3Harmonic"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator1FmDepth"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator2FmDepth"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator3FmDepth"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator1EgDecay"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator2EgDecay"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator3EgDecay"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "aegAttack"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "aegDecay"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "aegSustain"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "aegRelease"
  ]

voiceCommon2Entries :: [Entry]
voiceCommon2Entries =
  [ entry (OneR 0x00 0x03) (OneV 0x03) "modulatorSelect"
  , entry (OneR 0x00 0x03) (OneV 0x00) "sceneControl"
  , entry (TwoR (OneR 0x00 0x4A) (OneR 0x00 0x7F)) (TwoV 0x8C) "commonTempo"
  , entry (OneR 0x32 0x53) (OneV 0x32) "playEffectSwing"
  ]

voiceSceneEntries :: [Entry]
voiceSceneEntries =
  [ entry (OneR 0x00 0x7F) (OneV 0x7F) "filterCutoff"
  , entry (OneR 0x00 0x74) (OneV 0x10) "filterResonance"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "fegAttack"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "fegDecay"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "fegSustain"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "fegRelease"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "fegDepth"
  , entry (OneR 0x00 0x05) (OneV 0x00) "filterType"
  , entry (OneR 0x00 0x63) (OneV 0x00) "lfoSpeed"
  , entry (OneR 0x00 0x63) (OneV 0x00) "portamentoTime"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "mixerNoiseLevel"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator1Harmonic"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator2Harmonic"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator3Harmonic"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator1FmDepth"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator2FmDepth"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator3FmDepth"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator1EgDecay"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator2EgDecay"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "modulator3EgDecay"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "aegAttack"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "aegDecay"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "aegSustain"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "aegRelease"
  , entry (OneR 0x00 0x7F) (OneV 0x64) "volume"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "pan"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "effectSend"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "effectParameter"
  ]

trackParamEntries :: [Entry]
trackParamEntries = do
  i <- [1 .. 4]
  j <- [ entry (OneR 0x00 0x1F) (OneV 0x00) ("freeEgTrackParam" ++ show i)
       , entry (OneR 0x00 0x01) (OneV 0x00) ("freeEgTrackSceneSwitch" ++ show i)
       ]
  return j

trackDataEntries :: [Entry]
trackDataEntries = do
  i <- [1 .. 4]
  j <- [1 .. 192]
  return $ entry (TwoR (OneR 0x00 0x01) (OneR 0x00 0x7F)) (TwoV 0x0100) ("freeEgTrack" ++ show i ++ "Data" ++ show j)

voiceFreeEgEntries :: [Entry]
voiceFreeEgEntries =
  [ entry (OneR 0x00 0x03) (OneV 0x03) "freeEgTrigger"
  , entry (OneR 0x00 0x04) (OneV 0x01) "freeEgLoopType"
  , entry (OneR 0x02 0x60) (OneV 0x05) "freeEgLength"
  , entry (OneR 0x00 0x7F) (OneV 0x40) "freeEgKeyboardTrack"
  ] ++ trackParamEntries ++ trackDataEntries

multiply :: Int -> Entry -> [Entry]
multiply lim e = do
  i <- [1 .. lim]
  return $ e { _entryName = _entryName e ++ show i }

voiceStepSeqEntries :: [Entry]
voiceStepSeqEntries =
  [ entry (EnumR [0x04, 0x06, 0x07]) (OneV 0x07) "stepSeqBaseUnit"
  , entry (EnumR [0x08, 0x0C, 0x10]) (OneV 0x10) "stepSeqLength"
  , reserved 4
  ] ++ multiply 16 (entry (OneR 0x00 0xF7) (OneV 0x3C) "stepSeqNote")
    ++ multiply 16 (entry (OneR 0x00 0xF7) (OneV 0x64) "stepSeqVelocity")
    ++ multiply 16 (entry (OneR 0x00 0xF7) (OneV 0x3C) "stepSeqGateTimeLsb")
    ++ multiply 16 (entry (OneR 0x00 0xF7) (OneV 0x00) "stepSeqControlChange")
    ++ multiply 16 (entry (OneR 0x00 0xF7) (OneV 0x00) "stepSeqGateTimeMsb")
    ++ multiply 16 (entry (OneR 0x00 0x01) (OneV 0x00) "stepSeqMute")

effectEntries :: [Entry]
effectEntries =
  [ entry (TwoR (EnumR [0x00, 0x01, 0x02, 0x03]) (EnumR [0x00, 0x01, 0x02, 0x03])) (TwoV 0x00) "effectType"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "effectParameter"
  ]

partMixEntries :: [Entry]
partMixEntries =
  [ reserved 5
  , entry (OneR 0x00 0x7F) (OneV 0x00) "volume"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "pan"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "effect1Send"
  , reserved 5
  , entry (OneR 0x00 0x7F) (OneV 0x00) "filterCutoffFrequency"
  , entry (OneR 0x00 0x7F) (OneV 0x00) "filterResonance"
  , reserved 3
  ]

rhythmStepSeqEntries :: [Entry]
rhythmStepSeqEntries =
  [ reserved 6
  ] ++ multiply 16 (entry (OneR 0x00 0x78) (OneV 0x00) "stepSeqInstrument")
    ++ multiply 16 (entry (OneR 0x00 0x7F) (OneV 0x64) "stepSeqVelocity")
    ++ multiply 16 (entry (OneR 0x00 0x7F) (OneV 0x3C) "stepSeqGateTimeLsb")
    ++ multiply 16 (entry (OneR 0x00 0x7F) (OneV 0x00) "stepSeqPitch")
    ++ multiply 16 (entry (OneR 0x00 0xF7) (OneV 0x00) "stepSeqGateTimeMsb")
    ++ multiply 16 (entry (OneR 0x00 0x01) (OneV 0x00) "stepSeqMute")

songEntries :: [Entry]
songEntries =
  [ entry (TwoR (OneR 0x00 0x7F) (OneR 0x00 0x7F)) (TwoV 0x00) "patternNum"
  , entry (TwoR (OneR 0x00 0x7F) (OneR 0x00 0x7F)) (TwoV 0x8C) "bpm"
  , entry (TwoR (OneR 0x00 0x7F) (OneR 0x00 0x7F)) (TwoV 0x64) "playFxGateTime"
  , entry (EnumR [0x00, 0x01, 0x02, 0x03, 0x7F]) (OneV 0x00) "beat"
  , entry (MultiR (OneR 0x32 0x53) (EnumR [0x7F])) (OneV 0x50) "swing"
  , entry (MultiR (OneR 0x28 0x58) (EnumR [0x7F])) (OneV 0x00) "pitch"
  , entry (EnumR [0x00, 0x01, 0x7F]) (OneV 0x00) "loopType"
  , entry (MultiR (OneR 0x00 0x0F) (EnumR [0x7F])) (OneV 0x00) "trackMute"
  ]


data Context = Context
  { _contextSong :: Maybe Word8
  , _contextMeasure :: Maybe Word16
  , _contextPart :: Maybe Word8
  , _contextPattern :: Maybe Word8
  }

emptyContext :: Context
emptyContext = Context Nothing Nothing Nothing Nothing

match62 :: Word8 -> Maybe [Entry]
match62 0x20 = Just voiceCommon1Entries
match62 0x21 = Just voiceCommon2Entries
match62 i | i == 0x40 || i == 0x41 = Just voiceSceneEntries
match62 i | i >= 0x30 && i < 0x40 = Just voiceFreeEgEntries
match62 0x50 = Just voiceStepSeqEntries
match62 _ = Nothing

match6D :: Word8 -> Word8 -> Maybe (Context, [Entry])
match6D i j | i >= 0x20 && i < 0x30 =
  Just (emptyContext { _contextPart = Just (i .&. 0x0F), _contextPattern = Just j }, rhythmStepSeqEntries)
match6D i j | i == 0x30 =
  Just (emptyContext { _contextPattern = Just j }, effectEntries)
match6D i j | i >= 0x30 && i < 0x50 =
  Just (emptyContext { _contextPart = Just (i .&. 0x0F), _contextPattern = Just j }, partMixEntries)
match6D i j | i >= 0x60 && i < 0x70 =
  Just (emptyContext { _contextSong = Just (i .&. 0x0F), _contextMeasure = Just (fromIntegral j :: Word16) }, songEntries)
match6D i j | i >= 0x70 && i < 0x80 =
  Just (emptyContext { _contextSong = Just (i .&. 0x0F), _contextMeasure = Just ((fromIntegral j :: Word16) + 0x7F) }, songEntries)
match6D _ _ = Nothing

getEntries :: Word7 -> Address -> Maybe (Context, [Entry])
getEntries modelId address =
  case low8 of
    0 -> Nothing
    _ ->
      case model8 of
        0x62 -> match62 hi8 >>= (\es -> Just (emptyContext, es))
        0x6D -> match6D hi8 mid8
        _ -> Nothing
  where
    Address (Word7 hi8, Word7 mid8, Word7 low8) = address
    (Word7 model8) = modelId

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

