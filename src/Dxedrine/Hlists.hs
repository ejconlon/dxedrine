module Dxedrine.Hlists where

import Control.Monad (forM_, replicateM_)
import Data.Binary (Get, Put, getWord8, putWord8)
import Data.Binary.Get (runGetOrFail)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)
import Dxedrine.Words

data Range =
    IgnoreR !Int
  | OneR !Word8 !Word8
  | TwoR !Range !Range
  | EnumR ![Word8]
  | MultiR !Range !Range
  deriving (Show, Eq)

data Value =
    IgnoreV !Int
  | OneV !Word7
  | TwoV !Word14
  deriving (Show, Eq)

data Entry = Entry
  { _entryName :: !String
  , _entryRange :: !Range
  , _entryDefault :: !Value
  } deriving (Show, Eq)

newtype Hlist = Hlist
  { unHlist :: [(String, Value)]
  } deriving (Show, Eq)

validate :: Range -> Value -> Either String ()
validate r v =
  case (r, v) of
    (OneR s e, OneV (Word7 x)) ->
      if x >= s && x <= e
        then pure ()
        else Left $ show x ++ " outside range [" ++ show s ++ ", " ++ show e ++ "]"
    (TwoR e1 e2, TwoV (Word14 (x, y))) -> do
      _ <- validate e1 (OneV x)
      _ <- validate e2 (OneV y)
      pure ()
    (EnumR vals, OneV (Word7 x)) ->
      if x `elem` vals
        then pure ()
        else Left $ show x ++ " not an element of " ++ show vals
    (MultiR e1 e2, OneV _) -> do
      case validate e1 v of
        Right _ -> pure ()
        Left r1 ->
          case validate e2 v of
            Right _ -> pure ()
            Left r2 -> Left $ "both " ++ r1 ++ " and " ++ r2
    (IgnoreR i, IgnoreV j) ->
      if i == j
        then pure ()
        else Left $ "Unmatched ignore lengths: expected " ++ show i ++ " but was " ++ show j
    _ -> Left "wrong byte length"

validateHlist :: [Entry] -> Hlist -> Either String ()
validateHlist es (Hlist rootHs) = go es
  where
    go [] = pure ()
    go (e:es') =
      let r = _entryRange e
      in case r of
        IgnoreR _ -> go es'
        _ -> let n = _entryName e
          in case lookup n rootHs of
            Nothing -> Left $ "field \"" ++ n ++ "\" missing"
            Just v -> do
              case validate (_entryRange e) v of
                Left reason -> Left $ "field \"" ++ n ++ "\" invalid: " ++ reason
                _ -> go es'

addDefaults :: [Entry] -> Hlist -> Hlist
addDefaults es (Hlist rootHs) = Hlist (go es rootHs)
  where
    go [] hs = hs
    go (e:es') hs =
      case _entryRange e of
        IgnoreR _ -> go es' hs
        _ -> let n = _entryName e
             in (n, fromMaybe (_entryDefault e) (lookup n rootHs)) : go es' hs

defaultHlist :: [Entry] -> Hlist
defaultHlist es = addDefaults es (Hlist [])

getValue :: Entry -> Get Value
getValue e =
  let r = _entryRange e
  in case r of
    IgnoreR i -> do
      replicateM_ i getWord8
      pure (IgnoreV i)
    TwoR _ _ -> do
      w <- getWord14
      let v = TwoV w
      case validate r v of
        Right _ -> pure v
        Left reason -> fail reason
    _ -> do
      w <- getWord7
      let v = OneV w
      case validate r v of
        Right _ -> pure v
        Left reason -> fail reason

putValue :: Value -> Put
putValue v =
  case v of
    IgnoreV i -> replicateM_ i $ putWord8 0x00
    OneV v' -> putWord7 v'
    TwoV v' -> putWord14 v'

getHlist :: [Entry] -> Get Hlist
getHlist es = Hlist . reverse <$> go [] es
  where
    go hs [] = pure hs
    go hs (e:es') = do
      h <- getValue e
      let n = _entryName e
      go ((n, h):hs) es'

putHlist :: Hlist -> Put
putHlist (Hlist hs) = forM_ hs (\(_, h) -> putValue h)

packValue :: Entry -> Value -> Either String [Word7]
packValue e v =
  let r = _entryRange e
  in case validate r v of
    Left reason -> Left reason
    _ -> pure (fmap Word7 (BL.unpack (runPut (putValue v))))

packHlist :: [Entry] -> Hlist -> Either String [Word7]
packHlist entries hlist = do
  _ <- validateHlist entries hlist
  pure (fmap Word7 (BL.unpack (runPut (putHlist hlist))))

unpackHlist :: [Entry] -> [Word7] -> Either String (Hlist, [Word7])
unpackHlist es ws =
  unpack $ runGetOrFail (getHlist es) (BL.pack $ unWord7 <$> ws)
  where
    unpack (Left (_, _, e)) = Left e
    unpack (Right (left, _, h)) = Right (nonIgnored h, Word7 <$> BL.unpack left)
    nonIgnored (Hlist hs) = Hlist $ filter (\(_, h) -> shouldKeep h) hs
    shouldKeep (IgnoreV _) = False
    shouldKeep _ = True

reserved :: Int -> Entry
reserved i = Entry "reserved" (IgnoreR i) (IgnoreV i)

entry :: Range -> Value -> String -> Entry
entry range value name = Entry name range value

oneV :: Word8 -> Value
oneV = OneV . word7FromIntegral

twoV :: Word16 -> Value
twoV = TwoV . word14FromIntegral
