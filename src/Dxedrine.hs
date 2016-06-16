module Dxedrine
    ( someFunc
    ) where

import Data.Binary
import Data.ByteString hiding (putStrLn)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
