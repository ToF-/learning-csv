{-# LANGUAGE OverloadedStrings #-}

import Data.Vector as V (Vector, forM_)
import Data.Csv         (Field, FromField, FromNamedRecord, Header, Parser, parseField, parseNamedRecord, decodeByName, (.:))
import Data.ByteString.Lazy as BS (readFile, unpack)
import Data.ByteString.Lazy.Char8 as BS (putStrLn)
import Data.Char as C (isDigit)
import qualified Data.Text as T          ( pack , strip , unpack )
import qualified Data.Text.Encoding as E ( decodeUtf8 , encodeUtf8 )

import Data.ByteString.Lazy (fromStrict)

-- convert a Field (i.e a lazy ByteString) to a String
-- decoding the bytes to a Text value, then unpacking that to a String
fieldToString :: Field -> String
fieldToString = T.unpack . E.decodeUtf8

-- convert a String to a Field
-- packing the String into a Text value then encoding that to a ByteString
stringToField :: String -> Field
stringToField = E.encodeUtf8 . T.pack

-- a simple domain type, but the csv file has specfic format for salary :e.g "$ 40 000"

data Person = Person { name :: String, salary :: Salary }
    deriving Show

data Salary = Salary Int
    deriving Show

-- to parse a field into a Salary value
-- keep only the digit and parse an Int from that
-- then give it to the Salary function (or else fail with parse error)
instance FromField Salary
    where
        parseField t = Salary <$> (parseField (stringToField (digits (fieldToString t))) :: Parser Int)

digits :: String -> String
digits = filter C.isDigit

-- to parse a record into a Person value
-- parse the field named "name" into a String
-- parse the field named "salary" into a Salary
-- then give that to the Person function (or else fail with parse error)
instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> r .: "name" <*> r .: "salary"

main :: IO ()
main = do
    csvData <- BS.readFile "specific-salaries.csv"
    case decodeByName csvData :: Either String (Header, Vector Person) of
      Left err -> Prelude.putStrLn err
      Right (h, v) -> do
          BS.putStrLn $  csvData
          Prelude.putStrLn "-----------"
          Prelude.putStrLn $ show h
          V.forM_ v $ \ p -> Prelude.putStrLn $ show p

