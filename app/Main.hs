module Main where

import Dxedrine

main :: IO ()
main = do
  putStrLn "parsing"
  e <- parseDxUnions
  putStrLn $ show $ keepFailed e
  --putStrLn $ show e
  putStrLn "done"
  return ()
