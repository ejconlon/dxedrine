module Main where

import Control.Monad (forM_, unless)
import Dxedrine

main :: IO ()
main = do
  putStrLn "parsing"
  e <- parseDxUnions
  let numFailed = length $ keepFailed e
  unless (numFailed == 0) $ fail $ "number of failed messages: " ++ show numFailed
  let unions = keepSuccessful e
  forM_ unions $ \x -> do
    putStrLn $ show x
  putStrLn "done"
  return ()
