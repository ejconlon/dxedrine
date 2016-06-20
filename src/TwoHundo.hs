module TwoHundo where

import Data.Bits ((.&.))
import Data.Maybe (fromMaybe)
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
  | OneV Word7
  | TwoV Word14
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

-- TODO move to Word7/Word16
data Context = Context
  { _contextSong :: Maybe Word8
  , _contextMeasure :: Maybe Word16
  , _contextPart :: Maybe Word8
  , _contextPattern :: Maybe Word8
  } deriving (Show, Eq)

emptyContext :: Context
emptyContext = Context Nothing Nothing Nothing Nothing

newtype Hlist = Hlist
  { unHlist :: [(String, Value)]
  } deriving (Show, Eq)

defaultHlist :: [Entry] -> Hlist
defaultHlist entries = Hlist hvalues
  where
    hvalues = do
      e <- entries
      case (_entryRange e) of
        IgnoreR _ -> mempty
        _ -> return (_entryName e, _entryDefault e)

packValue' :: Range -> Value -> Either String [Word7]
packValue' r v =
  case (r, v) of
    (OneR s e, OneV w@(Word7 x)) ->
      if (x >= s && x <= e) then Right [w] else Left $ show x ++ " outside range [" ++ show s ++ ", " ++ show e ++ "]"
    (TwoR e1 e2, TwoV w@(Word14 (x, y))) -> do
      xs <- packValue' e1 (OneV x)
      ys <- packValue' e2 (OneV y)
      return $ xs ++ ys
    (EnumR vals, OneV w@(Word7 x)) ->
      if (x `elem` vals) then Right [w] else Left $ show x ++ " not an element of " ++ show vals
    (MultiR e1 e2, OneV w@(Word7 x)) ->
      case packValue' e1 v of
        Right xs -> Right xs
        Left r1 ->
          case packValue' e2 v of
            Right ys -> Right ys
            Left r2 -> Left $ "both " ++ r1 ++ " and " ++ r2
    (IgnoreR i, IgnoreV) -> Right $ replicate i $ Word7 0x00
    _ -> Left "wrong byte length"

packValue :: Entry -> Value -> Either String [Word7]
packValue e v = packValue' (_entryRange e) v

packHlist :: Bool -> [Entry] -> Hlist -> Either String [Word7]
packHlist useDefault entries hlist = go entries []
  where
    go [] xs = Right xs
    go (e:es) xs =
      let v = lookup (_entryName e) (unHlist hlist)
      in case (useDefault, v) of
        (False, Nothing) ->
          case _entryRange e of
            IgnoreR i -> go es (xs ++ (replicate i (Word7 0x00)))
            _ -> Left $ "field \"" ++ _entryName e ++ "\" missing"
        _ -> let vv = fromMaybe (_entryDefault e) v
             in case packValue e vv of
               Left r -> Left $ "field \"" ++ _entryName e ++ "\" invalid: " ++ r
               Right ys -> go es (xs ++ ys)

unpackHlist' :: Entry -> [Word7] -> Either String (Maybe (String, Value), [Word7])
unpackHlist' e ws =
  case (_entryRange e) of
    IgnoreR i ->
      if length ws >= i
        then Right (Nothing, drop i ws)
        else Left $ "not enough bytes: " ++ show (length ws) ++ " of " ++ show i


unpackHlist :: [Entry] -> [Word7] -> Either String (Hlist, [Word7])
unpackHlist = go []
  where
    go hl [] ws = Right (Hlist (reverse hl), ws)
    go hl (e:es) ws =
      case unpackHlist' e ws of
        Left r -> Left $ "error unpacking \"" ++ _entryName e ++ "\": " ++ r
        Right (p, xs) ->
          case p of
            Nothing -> go hl es xs
            Just pp -> go (pp : hl) es xs

reserved :: Int -> Entry
reserved i = Entry "reserved" (IgnoreR i) IgnoreV

entry :: Range -> Value -> String -> Entry
entry range value name = Entry name range value

oneV :: Word8 -> Value
oneV = OneV . word7FromIntegral

twoV :: Word16 -> Value
twoV = TwoV . word14FromIntegral

system2Block :: Block
system2Block = Block
  { _blockName = "system2"
  , _blockAddress = mkAddress 0x00 0x00 0x07
  , _blockEntries =
    [ reserved 1
    , entry (OneR 0x00 0x06) (oneV 0x01) "velocityCurve"
    , reserved 5
    , entry (OneR 0x00 0x03) (oneV 0x00) "bulkReceiveBlock"
    , reserved 1
    ]
  }

voiceCommon1Entries :: [Entry]
voiceCommon1Entries =
  [ entry (OneR 0x00 0x01) (oneV 0x01) "distortionOffOn"
  , entry (OneR 0x00 0x64) (oneV 0x40) "distortionDrive"
  , entry (OneR 0x00 0x03) (oneV 0x01) "distortionAmpType"
  , entry (OneR 0x22 0x3C) (oneV 0x30) "distortionLpfCutoff"
  , entry (OneR 0x00 0x64) (oneV 0x3C) "distortionOutLevel"
  , entry (OneR 0x01 0x7F) (oneV 0x01) "distortionDryWet"
  , entry (OneR 0x04 0x28) (oneV 0x11) "eqLowFreq"
  , entry (OneR 0x34 0x4C) (oneV 0x40) "eqLowGain"
  , entry (OneR 0x0E 0x36) (oneV 0x28) "eqMidFreq"
  , entry (OneR 0x34 0x4C) (oneV 0x40) "eqMidGain"
  , entry (OneR 0x0A 0x78) (oneV 0x0A) "eqMidResonance"
  , reserved 1
  , entry (OneR 0x00 0x7F) (oneV 0x7F) "filterCutoff"
  , entry (OneR 0x00 0x74) (oneV 0x10) "filterResonance"
  , entry (OneR 0x00 0x05) (oneV 0x00) "filterType"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "filterCutoffScalingDepth"
  , entry (OneR 0x00 0x63) (oneV 0x00) "filterCutoffModulationDepth"
  , entry (OneR 0x34 0x4C) (oneV 0x40) "filterInputGain"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "fegAttack"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "fegDecay"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "fegSustain"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "fegRelease"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "fegDepth"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "fegDepthVelocitySense"
  , reserved 1
  , entry (OneR 0x00 0x0F) (oneV 0x00) "noiseOscType"
  , entry (OneR 0x00 0x7F) (oneV 0x7F) "mixerVoiceLevel"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "mixerNoiseLevel"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator1Harmonic"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator2Harmonic"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator3Harmonic"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator1FmDepth"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator2FmDepth"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator3FmDepth"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator1EgDecay"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator2EgDecay"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator3EgDecay"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "aegAttack"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "aegDecay"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "aegSustain"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "aegRelease"
  ]

voiceCommon2Entries :: [Entry]
voiceCommon2Entries =
  [ entry (OneR 0x00 0x03) (oneV 0x03) "modulatorSelect"
  , entry (OneR 0x00 0x03) (oneV 0x00) "sceneControl"
  , entry (TwoR (OneR 0x00 0x4A) (OneR 0x00 0x7F)) (twoV 0x8C) "commonTempo"
  , entry (OneR 0x32 0x53) (oneV 0x32) "playEffectSwing"
  ]

voiceSceneEntries :: [Entry]
voiceSceneEntries =
  [ entry (OneR 0x00 0x7F) (oneV 0x7F) "filterCutoff"
  , entry (OneR 0x00 0x74) (oneV 0x10) "filterResonance"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "fegAttack"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "fegDecay"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "fegSustain"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "fegRelease"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "fegDepth"
  , entry (OneR 0x00 0x05) (oneV 0x00) "filterType"
  , entry (OneR 0x00 0x63) (oneV 0x00) "lfoSpeed"
  , entry (OneR 0x00 0x63) (oneV 0x00) "portamentoTime"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "mixerNoiseLevel"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator1Harmonic"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator2Harmonic"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator3Harmonic"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator1FmDepth"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator2FmDepth"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator3FmDepth"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator1EgDecay"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator2EgDecay"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "modulator3EgDecay"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "aegAttack"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "aegDecay"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "aegSustain"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "aegRelease"
  , entry (OneR 0x00 0x7F) (oneV 0x64) "volume"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "pan"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "effectSend"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "effectParameter"
  ]

trackParamEntries :: [Entry]
trackParamEntries = do
  i <- [1 .. 4]
  j <- [ entry (OneR 0x00 0x1F) (oneV 0x00) ("freeEgTrackParam" ++ show i)
       , entry (OneR 0x00 0x01) (oneV 0x00) ("freeEgTrackSceneSwitch" ++ show i)
       ]
  return j

trackDataEntries :: [Entry]
trackDataEntries = do
  i <- [1 .. 4]
  j <- [1 .. 192]
  return $ entry (TwoR (OneR 0x00 0x01) (OneR 0x00 0x7F)) (twoV 0x0100) ("freeEgTrack" ++ show i ++ "Data" ++ show j)

voiceFreeEgEntries :: [Entry]
voiceFreeEgEntries =
  [ entry (OneR 0x00 0x03) (oneV 0x03) "freeEgTrigger"
  , entry (OneR 0x00 0x04) (oneV 0x01) "freeEgLoopType"
  , entry (OneR 0x02 0x60) (oneV 0x05) "freeEgLength"
  , entry (OneR 0x00 0x7F) (oneV 0x40) "freeEgKeyboardTrack"
  ] ++ trackParamEntries ++ trackDataEntries

multiply :: Int -> Entry -> [Entry]
multiply lim e = do
  i <- [1 .. lim]
  return $ e { _entryName = _entryName e ++ show i }

voiceStepSeqEntries :: [Entry]
voiceStepSeqEntries =
  [ entry (EnumR [0x04, 0x06, 0x07]) (oneV 0x07) "stepSeqBaseUnit"
  , entry (EnumR [0x08, 0x0C, 0x10]) (oneV 0x10) "stepSeqLength"
  , reserved 4
  ] ++ multiply 16 (entry (OneR 0x00 0xF7) (oneV 0x3C) "stepSeqNote")
    ++ multiply 16 (entry (OneR 0x00 0xF7) (oneV 0x64) "stepSeqVelocity")
    ++ multiply 16 (entry (OneR 0x00 0xF7) (oneV 0x3C) "stepSeqGateTimeLsb")
    ++ multiply 16 (entry (OneR 0x00 0xF7) (oneV 0x00) "stepSeqControlChange")
    ++ multiply 16 (entry (OneR 0x00 0xF7) (oneV 0x00) "stepSeqGateTimeMsb")
    ++ multiply 16 (entry (OneR 0x00 0x01) (oneV 0x00) "stepSeqMute")

effectEntries :: [Entry]
effectEntries =
  [ entry (TwoR (EnumR [0x00, 0x01, 0x02, 0x03]) (EnumR [0x00, 0x01, 0x02, 0x03])) (twoV 0x00) "effectType"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "effectParameter"
  ]

partMixEntries :: [Entry]
partMixEntries =
  [ reserved 5
  , entry (OneR 0x00 0x7F) (oneV 0x00) "volume"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "pan"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "effect1Send"
  , reserved 5
  , entry (OneR 0x00 0x7F) (oneV 0x00) "filterCutoffFrequency"
  , entry (OneR 0x00 0x7F) (oneV 0x00) "filterResonance"
  , reserved 3
  ]

rhythmStepSeqEntries :: [Entry]
rhythmStepSeqEntries =
  [ reserved 6
  ] ++ multiply 16 (entry (OneR 0x00 0x78) (oneV 0x00) "stepSeqInstrument")
    ++ multiply 16 (entry (OneR 0x00 0x7F) (oneV 0x64) "stepSeqVelocity")
    ++ multiply 16 (entry (OneR 0x00 0x7F) (oneV 0x3C) "stepSeqGateTimeLsb")
    ++ multiply 16 (entry (OneR 0x00 0x7F) (oneV 0x00) "stepSeqPitch")
    ++ multiply 16 (entry (OneR 0x00 0xF7) (oneV 0x00) "stepSeqGateTimeMsb")
    ++ multiply 16 (entry (OneR 0x00 0x01) (oneV 0x00) "stepSeqMute")

songEntries :: [Entry]
songEntries =
  [ entry (TwoR (OneR 0x00 0x7F) (OneR 0x00 0x7F)) (twoV 0x00) "patternNum"
  , entry (TwoR (OneR 0x00 0x7F) (OneR 0x00 0x7F)) (twoV 0x8C) "bpm"
  , entry (TwoR (OneR 0x00 0x7F) (OneR 0x00 0x7F)) (twoV 0x64) "playFxGateTime"
  , entry (EnumR [0x00, 0x01, 0x02, 0x03, 0x7F]) (oneV 0x00) "beat"
  , entry (MultiR (OneR 0x32 0x53) (EnumR [0x7F])) (oneV 0x50) "swing"
  , entry (MultiR (OneR 0x28 0x58) (EnumR [0x7F])) (oneV 0x00) "pitch"
  , entry (EnumR [0x00, 0x01, 0x7F]) (oneV 0x00) "loopType"
  , entry (MultiR (OneR 0x00 0x0F) (EnumR [0x7F])) (oneV 0x00) "trackMute"
  ]


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

getEntries' :: Word7 -> Address -> Maybe (Context, [Entry])
getEntries' modelId address =
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

getEntries :: DxUnion -> Maybe (Context, [Entry])
getEntries (DPC m) = Nothing
getEntries (DBD m) = Nothing
getEntries (D2PC m) = getEntries' (_d2pcModel m) (_d2pcAddr m)
getEntries (D2BD m) = getEntries' (_d2bdModel m) (_d2bdAddr m)

