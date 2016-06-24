module Dxedrine.Hlists where

import Dxedrine.Words
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
    IgnoreV
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

validate :: Range -> Value -> Maybe String
validate r v =
  case packValue' r v of
    Left x -> Just x
    _ -> Nothing

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
reserved i = Entry "reserved" (IgnoreR i) IgnoreV

entry :: Range -> Value -> String -> Entry
entry range value name = Entry name range value

oneV :: Word8 -> Value
oneV = OneV . word7FromIntegral

twoV :: Word16 -> Value
twoV = TwoV . word14FromIntegral
