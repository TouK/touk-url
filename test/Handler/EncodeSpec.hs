module Handler.EncodeSpec (spec) where

import Handler.Encode
import TestImport
import Text.Parsec
import Test.Hspec

parseURL = parse parser "url"

urls :: [Text]
urls = "http://valid.pl"
       : "http://lol-valid.pl"
       : "https://touk.pl/?token=as2df"
       : "http://192.168.1.10/home/"
       : "http://192.10.10.10:12/home"
       : "http://touk.pl/admin/admin/admin/?pw=tajne"
       : []

spec :: Spec
spec = do
  describe "Handler.Encode parser" $ do
    forM_ urls $ \url ->
      it (unpack url) $ parseURL url `shouldBe` Right ()
