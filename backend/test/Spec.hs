{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.Map (fromList)
import Data.Set qualified as Set
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TL
import Parse
import Test.Hspec
import Types
import Prelude

main :: IO ()
main = do
  dockerOutput1 <- liftIO $ Prelude.readFile "test_files/docker-output-1.txt"
  expected1 <- liftIO $ Prelude.readFile "test_files/expected-1.json"
  hspec $ do
    describe "Wireguard-Status" $ do
      it "can read a wireguard docker output" $ do
        let actual = TL.unpack . TL.decodeUtf8 $ encode $ parseStatus invertedPeerNames dockerOutput1
        actual `shouldBe` expected1
  where
    invertedPeerNames :: InvertedPeerNameMap
    invertedPeerNames =
      fromList
        [ ("8Sw8SACyGK6+i+B3sonIr4iKx9OJ2vp+vaiGdD/hXhs=", Set.singleton "Anna"),
          ("mwwhIx4XYCo172dndNInME42kSjfnRM+wGH5/XZhnkI=", Set.singleton "Bob"),
          ("zhwBStLYVjtZhPtZAl9YOVsR12Ib5ohPkIwcxTkTM2g=", Set.singleton "Charlie"),
          ("kckHnlzSHFs+FD46zeMyMRFRfQ1VWHs4E2zJxAQvSC4=", Set.singleton "Eve"),
          ("AAJCYjebY5VTQoVaewzaRbItFpnvGPVoum24tgHOWCM=", Set.singleton "Frank"),
          ("o/+Z9s/hbSFlgFl9ntTxTxU2U8LVQhXP6By3DwD1oDY=", Set.singleton "Grace"),
          ("+m7W6QIbzjLc5JH5UH34FhCI5ZDnaw7CIJjGvYSTiW0=", Set.singleton "Heidi"),
          ("pzT0eVxo+mJ5PBj5kzn7pcsBOqsMo/GtdQ/qv7a4ZyE=", Set.singleton "Ivan"),
          ("6/aL+XZ03D3ceroL4B8U1JHnLnz63rOjirgjerJNLnc=", Set.singleton "Judy")
        ]
