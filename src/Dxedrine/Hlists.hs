module Dxedrine.Hlists where

import Control.Monad (forM_, replicateM_)
import Dxedrine.Words
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Word (Word8(..), Word16(..))

data Range =
    IgnoreR Int
  | OneR Word8 Word8
  | TwoR Range Range
  | EnumR [Word8]
  | MultiR Range Range
  deriving (Show, Eq)

data Value =
    IgnoreV Int
  | OneV Word7
  | TwoV Word14
  deriving (Show, Eq)

data Entry = Entry
  { _entryName :: String
  , _entryRange :: Range
  , _entryDefault :: Value
  } deriving (Show, Eq)

newtype Hlist = Hlist
  { unHlist :: [(String, Value)]
  } deriving (Show, Eq)

validate :: Range -> Value -> Either String ()
validate r v =
  case (r, v) of
    (OneR s e, OneV w@(Word7 x)) ->
      if (x >= s && x <= e)
        then return ()
        else Left $ show x ++ " outside range [" ++ show s ++ ", " ++ show e ++ "]"
    (TwoR e1 e2, TwoV w@(Word14 (x, y))) -> do
      _ <- validate e1 (OneV x)
      _ <- validate e2 (OneV y)
      return ()
    (EnumR vals, OneV w@(Word7 x)) ->
      if (x `elem` vals)
        then return ()
        else Left $ show x ++ " not an element of " ++ show vals
    (MultiR e1 e2, OneV w@(Word7 x)) -> do
      case validate e1 v of
        Right _ -> return ()
        Left r1 ->
          case validate e2 v of
            Right _ -> return ()
            Left r2 -> Left $ "both " ++ r1 ++ " and " ++ r2
    (IgnoreR i, IgnoreV j) ->
      if i == j
        then return ()
        else Left $ "Unmatched ignore lengths: expected " ++ show i ++ " but was " ++ show j
    _ -> Left "wrong byte length"

addDefaults :: [Entry] -> Hlist -> Hlist
addDefaults es (Hlist hs) = Hlist $ go es hs
  where
    go [] hs = hs
    go (e:es) hs =
      case (_entryRange e) of
        IgnoreR i -> go es hs
        _ -> let n = _entryName e
             in (n, fromMaybe (_entryDefault e) (lookup n hs)):(go es hs)

defaultHlist :: [Entry] -> Hlist
defaultHlist es = addDefaults es (Hlist [])

getValue :: Entry -> Get Value
getValue e =
  let r = _entryRange e
  in case r of
    IgnoreR i -> do
      replicateM_ i getWord8
      return (IgnoreV i)
    TwoR _ _ -> do
      w <- getWord14
      let v = TwoV w
      case validate r v of
        Right _ -> return v
        Left reason -> fail reason
    _ -> do
      w <- getWord7
      let v = OneV w
      case validate r v of
        Right _ -> return v
        Left reason -> fail reason

putValue :: Value -> Put
putValue v =
  case v of
    IgnoreV i -> replicateM_ i $ putWord8 0x00
    OneV v -> putWord7 v
    TwoV v -> putWord14 v

getHlist :: [Entry] -> Get Hlist
getHlist es = Hlist . reverse <$> go [] es
  where
    go hs [] = return hs
    go hs (e:es) = do
      h <- getValue e
      let n = _entryName e
      go ((n, h):hs) es

putHlist :: Hlist -> Put
putHlist (Hlist hs) = forM_ hs (\(_, h) -> putValue h)

packValue' :: Range -> Value -> Either String [Word7]
packValue' r v =
  case validate r v of
    Left reason -> Left reason
    _ -> Right $ Word7 <$> (BL.unpack $ runPut $ putValue v)

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
    TwoR _ _ ->
      if length ws >= 2
        then let v = TwoV (Word14 ((head ws), (head (tail ws))))
             in do
                _ <- packValue' (_entryRange e) v
                return (Just (_entryName e, v), drop 2 ws)
        else Left $ "not enough bytes: " ++ show (length ws) ++ " of 2"
    _ ->
      if length ws >= 1
        then let v = OneV (head ws)
              in do
                _ <- packValue' (_entryRange e) v
                return (Just (_entryName e, v), drop 1 ws)
        else Left $ "empty"

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
reserved i = Entry "reserved" (IgnoreR i) (IgnoreV i)

entry :: Range -> Value -> String -> Entry
entry range value name = Entry name range value

oneV :: Word8 -> Value
oneV = OneV . word7FromIntegral

twoV :: Word16 -> Value
twoV = TwoV . word14FromIntegral
