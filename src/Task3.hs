{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import ParserCombinators
import Control.Applicative ((<|>))
import Data.Char (chr, isDigit, toLower)
import Data.List (intercalate)
import Text.Read (readMaybe)

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

json :: Parser JValue
json = spaces *> jvalue

jvalue :: Parser JValue
jvalue = spaces *> (jobject <|> jarray <|> jstring <|> jnumber <|> jbool <|> jnull)

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p

symbol :: String -> Parser String
symbol s = lexeme (string s)

comma, colon, openBrace, closeBrace, openBracket, closeBracket :: Parser String
comma = symbol ","
colon = symbol ":"
openBrace = symbol "{"
closeBrace = symbol "}"
openBracket = symbol "["
closeBracket = symbol "]"

jobject :: Parser JValue
jobject = JObject <$> (openBrace *> pair `sepBy` comma <* closeBrace)
  where pair = (,) <$> (spaces *> stringContent <* colon) <*> jvalue

jarray :: Parser JValue
jarray = JArray <$> (openBracket *> jvalue `sepBy` comma <* closeBracket)

stringContent :: Parser String
stringContent = char '"' *> manyTill jchar (char '"')
  where
    jchar = normalChar <|> escapeChar
    normalChar = satisfy (\c -> c /= '"' && c /= '\\')
    escapeChar = char '\\' *> (escapeMap <|> unicodeEscape)
    escapeMap = choice
      [ '"'  <$ char '"',  '\\' <$ char '\\', '/' <$ char '/'
      , '\n' <$ char 'n',  '\r' <$ char 'r',  '\f' <$ char 'f'
      , '\t' <$ char 't',  '\b' <$ char 'b'
      ]
    unicodeEscape = char 'u' *> (hexToChar <$> count 4 hexDigit)
    hexToChar hex = case readMaybe ("0x" ++ hex) of
                      Just c -> chr c
                      _      -> error "Invalid unicode escape"

jstring :: Parser JValue
jstring = JString <$> stringContent

hexDigit :: Parser Char
hexDigit = satisfy (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))

jnumber :: Parser JValue
jnumber = JNumber <$> lexeme number
  where
    number :: Parser Double
    number = read <$> ( (++) <$> signPart
                        <*> ( (++) <$> intPart
                              <*> ( (++) <$> fracPart
                                    <*> expPart ) ) )
    signPart = option "" (("-" <$ char '-') <|> ("" <$ char '+'))
    intPart = (char '0' *> pure "0") <|> ((:) <$> nonZeroDigit <*> many digit)
    fracPart = option "" $ (:) <$> char '.' <*> some digit
    expPart  = option "" $
                 (:) <$> (char 'e' <|> char 'E')
                     <*> ( (++) <$> option "" (("-" <$ char '-') <|> ("+" <$ char '+'))
                               <*> some digit )

nonZeroDigit :: Parser Char
nonZeroDigit = satisfy (\c -> c >= '1' && c <= '9')

digit :: Parser Char
digit = satisfy isDigit

jbool :: Parser JValue
jbool = JBool <$> (True <$ string "true" <|> False <$ string "false")

jnull :: Parser JValue
jnull = JNull <$ string "null"

-- combinators
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go where go = (end *> pure []) <|> ((:) <$> p <*> go)

option :: a -> Parser a -> Parser a
option x p = p <|> pure x

count :: Int -> Parser a -> Parser [a]
count n p | n <= 0    = pure []
          | otherwise = (:) <$> p <*> count (n-1) p

render :: JValue -> String
render = concatMap readable . renderTokens
  where readable ":" = ": "
        readable "," = ", "
        readable s   = s

renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ escapeString s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair (k, v) = ["\"" ++ escapeString k ++ "\"", ":"] ++ renderTokens v

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\f' = "\\f"
    escapeChar '\t' = "\\t"
    escapeChar '\b' = "\\b"
    escapeChar c    = [c]

renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed e)   = show e

renderJSONFile :: String -> IO String
renderJSONFile = fmap renderParsed . parseJSONFile

parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile f = parse json <$> readFile f
