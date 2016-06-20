module Main where

import Control.Monad (forM_, unless)
import Dxedrine
import TwoHundo

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
      Just p@(context, entries) ->
        putStrLn $ show (x, context, entries)
  putStrLn "done"
  return ()
