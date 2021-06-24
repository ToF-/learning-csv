{-# LANGUAGE OverloadedStrings #-}

import Data.Vector as V (Vector, forM_)
import Data.Csv         (Field, FromField, FromNamedRecord, Header, Parser, parseField, parseNamedRecord, decodeByName, (.:))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy as BS (readFile)
import Data.ByteString.Lazy.Char8 as BS (putStrLn)
import Data.ByteString.Char8 as BS (strip, tail, last, length, head, init)
import Data.ByteString.Lazy.UTF8 (toString)
import Control.Monad
import Control.Applicative

data Label = Label { theLabel :: String }
    deriving Show

data Number = Number { theNumber :: Integer }
    deriving Show

data Content = Content { label :: Label, number :: Number }
    deriving Show

instance FromField Label where
    parseField t = Label <$> parseField (BS.strip t)

instance FromField Number where
    parseField t = Number <$> (parseNegative st <|> parseField st)
        where st = BS.strip t

checkField :: (Field -> Bool) -> String -> Field -> Parser Field
checkField cond msg t = if cond t then pure t else fail msg

parseNegative :: Field -> Parser Integer
parseNegative t = do
    st <- checkField (\st
        -> BS.length st > 2
        && BS.head st == '('
        && BS.last st == ')') "missing number" (BS.strip t)
    negate <$> parseField (BS.init (BS.tail st))


instance FromNamedRecord Content where
    parseNamedRecord r = Content <$> r .: "label" <*> r .: "number"

main :: IO ()
main = do
    csvData <- BS.readFile "labels.csv"
    case decodeByName csvData :: Either String (Header, Vector Content) of
      Left err -> Prelude.putStrLn err
      Right (h, v) -> do
          Prelude.putStrLn $  "source:"
          Prelude.putStrLn $ toString csvData
          Prelude.putStrLn $ "header: " ++ show h
          Prelude.putStrLn $ "values:"
          V.forM_ v $ \ l -> Prelude.putStrLn $ show l
