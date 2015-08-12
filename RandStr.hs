module RandStr where

import Prelude
import Control.Monad
import System.Random
import qualified Data.Text as T

randomStringWithLen :: Int -> IO T.Text
randomStringWithLen len = liftM (T.pack . take len . randomRs ('a', 'z')) newStdGen
