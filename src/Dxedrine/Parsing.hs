module Dxedrine.Parsing where

import Data.Binary.Get (ByteOffset, Get, runGetOrFail)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isNothing)

newtype ParseResult a = ParseResult
  { unParseResult :: [(Either String a, ByteOffset)]
  } deriving (Show, Eq)

keepSuccessful :: ParseResult a -> [a]
keepSuccessful (ParseResult rs) = do
  (e, _) <- rs
  case e of
    Right a -> return a
    _ -> mempty

keepFailed :: ParseResult a -> [(String, ByteOffset)]
keepFailed (ParseResult rs) = do
  (e, o) <- rs
  case e of
    Left r -> return (r, o)
    _ -> mempty

blIsEmpty :: BL.ByteString -> Bool
blIsEmpty s = isNothing $ BL.uncons s

adjust :: [(x, ByteOffset)] -> [(x, ByteOffset)]
adjust = go 0
  where
    go _ [] = []
    go i ((z, o):ys) = let j = o + i in (z, j) : go j ys

getRepeated :: Get a -> BL.ByteString -> ParseResult a
getRepeated g s = ParseResult (adjust (go s))
  where
    go u =
      if blIsEmpty u
        then []
        else
          case runGetOrFail g u of
            Left (t, o, e) -> (Left e, o) : go t
            Right (t, o, a) -> (Right a, o) : go t

getN :: Get a -> Integer -> Get [a]
getN _ 0 = return []
getN g i = do
  x <- g
  xs <- getN g (i - 1)
  return $ x : xs

getUntil :: Get a -> (a -> Bool) -> Get ([a], a)
getUntil g p = do
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
