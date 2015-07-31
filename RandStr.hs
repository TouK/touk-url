module RandStr where

import Control.Monad
import System.Random

randomStringWithLen :: Int -> IO (String)
randomStringWithLen len = liftM (take len . randomRs ('a', 'z')) newStdGen