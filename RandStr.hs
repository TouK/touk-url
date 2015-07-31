module RandStr where

import Control.Monad
import System.Random

randomString :: IO (String)
randomString = liftM (take 10 . randomRs ('a', 'z')) newStdGen