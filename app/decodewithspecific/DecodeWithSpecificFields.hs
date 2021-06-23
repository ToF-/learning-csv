{-# LANGUAGE OverloadedStrings #-}

import Data.Vector as V (Vector, forM_)
import Data.Csv
import Data.ByteString.Lazy as BS (readFile)
import Data.Char as C (isDigit)
import qualified Data.Text as T          ( pack , strip , unpack )
import qualified Data.Text.Encoding as E ( decodeUtf8 , encodeUtf8 )

import Data.ByteString.Lazy (fromStrict)
fieldToString :: Field -> String
fieldToString = T.unpack . E.decodeUtf8

stringToField :: String -> Field
stringToField = E.encodeUtf8 . T.pack

data Person = Person { name :: String, salary :: Salary }
    deriving Show

data Salary = Salary Int
    deriving Show

instance FromField Salary
    where
        parseField t = Salary <$> (parseField (stringToField (digits (fieldToString t))) :: Parser Int)

digits :: String -> String
digits = filter C.isDigit

instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> r .: "name" <*> r .: "salary"

main :: IO ()
main = do
    csvData <- BS.readFile "specific-salaries.csv"
    case decodeByName csvData :: Either String (Header, Vector Person) of
      Left err -> Prelude.putStrLn err
      Right (_, v) -> V.forM_ v $ \ p ->
          Prelude.putStrLn $ show p

