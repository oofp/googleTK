module Main where

import Protolude
import TokenParser


-- use your token
fbToken :: Text
fbToken = "eyJh..."

-- use your token
gToken :: Text
gToken = "eyJhbG...."

main :: IO ()
main = do
  tp <- initTokenParser
  authDataFB <- parseToken fbToken tp
  authDataGG <- parseToken gToken tp
  putStrLn (show authDataFB::Text)
  putStrLn (show authDataGG::Text)
  return ()
