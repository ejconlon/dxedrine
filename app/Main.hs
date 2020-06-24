module Main (main) where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Dxedrine.Blocks
import Dxedrine.Model
import Dxedrine.Parsing

parseDxUnions :: MonadIO m => m (ParseResult DxUnion)
parseDxUnions = do
  contents <- liftIO BL.getContents
  return $ getRepeated get contents

main :: IO ()
main = do
  putStrLn "parsing"
  e <- parseDxUnions
  let numFailed = length $ keepFailed e
  unless (numFailed == 0) $ fail $ "number of failed messages: " ++ show numFailed
  let unions = keepSuccessful e
  forM_ unions $ \x -> do
    case getEntries x of
      Nothing -> return ()
      Just (context, entries) ->
        putStrLn $ show (x, context, entries)
  putStrLn "done"
  return ()
