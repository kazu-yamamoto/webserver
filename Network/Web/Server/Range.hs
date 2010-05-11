module Network.Web.Server.Range (skipAndSize) where

import Parsec

skipAndSize :: String -> Integer -> Maybe (Integer,Integer)
skipAndSize str size = case parseRange str of
  Just [(mbeg,mend)] -> adjust mbeg mend size
  _                  -> Nothing

adjust :: Maybe Integer -> Maybe Integer -> Integer -> Maybe (Integer,Integer)
adjust (Just beg) (Just end) siz
  | beg <= end && end <= siz     = Just (beg, end - beg + 1)
  | otherwise                    = Nothing
adjust (Just beg) Nothing    siz
  | beg <= siz                   = Just (beg, siz - beg)
  | otherwise                    = Nothing
adjust Nothing    (Just end) siz
  | end <= siz                   = Just (siz - end, end)
  | otherwise                    = Nothing
adjust Nothing    Nothing    _   = Nothing

type Range = (Maybe Integer, Maybe Integer)

parseRange :: String -> Maybe [Range]
parseRange xs = case parse byteRange "" xs of
  Left  _ -> Nothing
  Right x -> Just x

byteRange :: Parser [Range]
byteRange = string "bytes=" *> (ranges <* eof)

ranges :: Parser [Range]
ranges = sepBy1 (range <|> suffixRange) (spaces >> char ',' >> spaces)

range :: Parser Range
range = (,) <$> ((Just <$> num) <* char '-')
            <*> option Nothing (Just <$> num)

suffixRange :: Parser Range
suffixRange = (,) Nothing <$> (char '-' *> (Just <$> num))

num :: Parser Integer
num = read <$> many1 digit
