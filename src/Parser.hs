{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- The above pragma temporarily disables warnings about Parser constructor and runParser not being used

module Parser
  ( -- * Important note
    -- 
    -- | The implementation of 'Parser' is intentionally
    -- hidden to other modules to encourage use of high level
    -- combinators like 'satisfy' and the ones from 'ParserCombinators'
    Parser
  , parse
  , parseMaybe
  , satisfy
  , Error(..)
  , Position(..)
  , Parsed(..)
  , Input
  ) where

import Control.Applicative
import Data.List (nubBy)

-- | Value annotated with position of parsed input starting from 0
data Position a = Position Int a
 deriving (Show, Eq)

-- | Parser input encapsulating remaining string to be parsed with current position
type Input = Position String

-- | Parsing error
data Error =
    Unexpected Char -- ^ Unexpected character
  | EndOfInput      -- ^ Unexpected end of input
 deriving (Show, Eq)

-- | Parsing result of value of type @a@
data Parsed a =
    Parsed a Input           -- ^ Successfully parsed value of type @a@ with remaining input to be parsed
  | Failed [Position Error]  -- ^ Failed to parse value of type @a@ with accumulated list of errors
 deriving Show

-- | Parser of value of type @a@
newtype Parser a = Parser { runParser :: Input -> Parsed a }

-- | Runs given 'Parser' on given input string
parse :: Parser a -> String -> Parsed a
parse p s = runParser p (Position 0 s)

-- | Runs given 'Parser' on given input string with erasure of @Parsed a@ to @Maybe a@
parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case parse p s of
  Parsed a _ -> Just a
  Failed _   -> Nothing

instance Functor Parser where
  fmap f (Parser p) = Parser $ \inp -> case p inp of
    Parsed a inp' -> Parsed (f a) inp'
    Failed errs   -> Failed errs

instance Applicative Parser where
  pure a = Parser $ \inp -> Parsed a inp

  Parser pf <*> Parser pa = Parser $ \inp -> case pf inp of
    Failed errs -> Failed errs
    Parsed f inp' -> case pa inp' of
      Failed errs' -> Failed errs'
      Parsed a inp'' -> Parsed (f a) inp''

instance Alternative Parser where
  empty = Parser $ \(Position pos _) -> Failed [Position pos EndOfInput]

  Parser p <|> Parser q = Parser $ \inp -> case p inp of
    Parsed a inp' -> Parsed a inp'
    Failed errs1 -> case q inp of
      Parsed a inp' -> Parsed a inp'
      Failed errs2 -> Failed (dedup (errs1 ++ errs2))
    where
      dedup = nubBy (\(Position p1 e1) (Position p2 e2) -> p1 == p2 && e1 == e2)

-- | Parses single character satisfying given predicate
--
-- Usage example:
--
-- >>> parse (satisfy (>= 'b')) "foo"
-- Parsed 'f' (Position 1 "oo")
-- >>> parse (satisfy (>= 'b')) "bar"
-- Parsed 'b' (Position 1 "ar")
-- >>> parse (satisfy (>= 'b')) "abc"
-- Failed [Position 0 (Unexpected 'a')]
-- >>> parse (satisfy (>= 'b')) ""
-- Failed [Position 0 EndOfInput]
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \(Position pos cs) -> case cs of
  []     -> Failed [Position pos EndOfInput]
  (c:cs') -> if p c
             then Parsed c (Position (pos + 1) cs')
             else Failed [Position pos (Unexpected c)]
