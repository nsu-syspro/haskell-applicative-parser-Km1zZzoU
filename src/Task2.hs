{-# OPTIONS_GHC -Wall #-}
module Task2 where
-- The above pragma enables all warnings

import Parser
import ParserCombinators
import Control.Applicative ((<|>))
import Data.Char (isDigit)

-- | Date representation
--
-- Date parts are expected to be in following ranges
--
-- 'Day' in @[1..31]@
-- 'Month' in @[1..12]@
-- 'Year' is any non-negative integer
--
data Date = Date Day Month Year deriving (Show, Eq)
newtype Day   = Day   Int deriving (Show, Eq)
newtype Month = Month Int deriving (Show, Eq)
newtype Year  = Year  Int deriving (Show, Eq)

digit, nonZeroDigit :: Parser Char
digit = satisfy isDigit
nonZeroDigit = satisfy (\c -> c >= '1' && c <= '9')

usDay :: Parser Int
usDay = read <$> ( string "30" <|> string "31"
               <|> ((:) <$> char '1' <*> ((:[]) <$> digit))
               <|> ((:) <$> char '2' <*> ((:[]) <$> digit))
               <|> ((:[]) <$> nonZeroDigit) )

day :: Parser Int
day = read <$> ( string "30" <|> string "31"
             <|> ((:) <$> char '0' <*> ((:[]) <$> nonZeroDigit))
             <|> ((:) <$> satisfy (`elem` ['1','2']) <*> ((:[]) <$> digit)) )

month :: Parser Int
month = read <$> ( string "10" <|> string "11" <|> string "12"
               <|> ((:) <$> char '0' <*> ((:[]) <$> nonZeroDigit)) )

year :: Parser Int
year = read <$> some digit

monthName :: Parser Int
monthName = choice
  [ 1<$ string "Jan",
    2<$ string "Feb",
    3<$ string "Mar",
    4<$ string "Apr",
    5<$ string "May",
    6<$ string "Jun",
    7<$ string "Jul",
    8<$ string "Aug",
    9<$ string "Sep",
    10<$ string "Oct",
    11<$ string "Nov",
    12<$ string "Dec"
  ]

dotFormat, hyphenFormat, usFormat :: Parser Date
dotFormat    = Date . Day <$> day <* char '.' <*> (Month <$> month) <* char '.' <*> (Year <$> year)
hyphenFormat = Date . Day <$> day <* char '-' <*> (Month <$> month) <* char '-' <*> (Year <$> year)
usFormat = (\m d y -> Date (Day d) (Month m) (Year y))
           <$> (monthName <* char ' ')
           <*> (usDay <* char ' ')
           <*> year :: Parser Date

-- | Parses date in one of three formats given as BNF
--
-- @
-- date ::= dotFormat | hyphenFormat | usFormat
--
-- dotFormat ::= day "." month "." year
-- hyphenFormat ::= day "-" month "-" year
-- usFormat ::= monthName " " usDay " " year
--
-- usDay ::= nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- day ::= "0" nonZeroDigit | "1" digit | "2" digit | "30" | "31"
-- month ::= "0" nonZeroDigit | "10" | "11" | "12"
-- year ::= number
--
-- number ::= digit | number digit
-- digit ::= "0" | nonZeroDigit
-- nonZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
-- monthName ::= "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
-- @
--
-- Usage example:
--
-- >>> parse date "01.01.2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "12.12.2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "12-12-2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 10 "")
-- >>> parse date "Dec 12 2012"
-- Parsed (Date (Day 12) (Month 12) (Year 2012)) (Input 11 "")
-- >>> parse date "Jan 1 2012"
-- Parsed (Date (Day 1) (Month 1) (Year 2012)) (Input 10 "")
-- >>> parse date "Feb 31 2012"
-- Parsed (Date (Day 31) (Month 2) (Year 2012)) (Input 11 "")
-- >>> parse date "12/12/2012"
-- Failed [PosError 2 (Unexpected '/'),PosError 0 (Unexpected '1')]
--
date :: Parser Date
date = dotFormat <|> hyphenFormat <|> usFormat
