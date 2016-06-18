module TwoHundo where

import Dxedrine

data AnnoType =
    Pattern
  | Part
  | Song
  | Measure
  deriving (Show, Eq)

data DataType =
    VoiceCommon1
  | VoiceCommon2
  | VoiceScene
  | VoiceFreeEg
  | VoiceStepSeq
  | Effect
  | PartMix
  | RhythmStepSeq
  | SongData
  deriving (Show, Eq)

