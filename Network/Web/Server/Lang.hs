module Network.Web.Server.Lang (parseLang) where

import Data.List
import Data.Ord
import Parsec

parseLang :: String -> [String]
parseLang xs = case parse acceptLanguage "" xs of
    Left   _ -> []
    Right ls -> map fst $ sortBy detrimental ls
  where
    detrimental = flip (comparing snd)

----------------------------------------------------------------

acceptLanguage :: Parser [(String,Int)]
acceptLanguage = rangeQvalue `sepBy1` (spaces *> char ',' *> spaces)

rangeQvalue :: Parser (String,Int)
rangeQvalue = (,) <$> languageRange <*> quality

languageRange :: Parser String
languageRange = (++) <$> language <*> sublang

language :: Parser String
language = many1 letter

sublang :: Parser String
sublang = option "" ((:) <$> char '-' <*> many1 letter)

quality :: Parser Int
quality = option 1000 (string ";q=" *> qvalue)

qvalue :: Parser Int
qvalue = 1000  <$  (char '1' *> optional (char '.' *> range 0 3 digit))
     <|> read3 <$> (char '0' *> option "0" (char '.' *> range 0 3 digit))
  where
    read3 n = read . take 3 $ n ++ repeat '0'

----------------------------------------------------------------

range :: Int -> Int -> GenParser tok st a -> GenParser tok st [a]
range n m p = (++) <$> count n p <*> upto (m - n) p

upto :: Int -> GenParser tok st a -> GenParser tok st [a]
upto 0 _ = return []
upto n p = (:) <$> p <*> upto (n - 1) p <|> return []
