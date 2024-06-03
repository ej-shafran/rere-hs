module Bi.Parser (
    blobFieldP',
    blobFieldP,
    anyBlobField,
    intFieldP',
    intFieldP,
    anyIntField,
) where

import Text.Parsec (
    anyChar,
    char,
    count,
    digit,
    letter,
    many1,
    string,
 )
import Text.Parsec.String (Parser)

blobFieldP' :: Parser String -> Parser (String, String)
blobFieldP' nameParser = do
    _ <- string ":b "
    name <- nameParser
    _ <- char ' '
    len <- read <$> many1 digit
    _ <- char '\n'
    value <- count len anyChar
    _ <- char '\n'
    return (name, value)

anyBlobField :: Parser (String, String)
anyBlobField = blobFieldP' (many1 letter)

blobFieldP :: String -> Parser String
blobFieldP name = snd <$> blobFieldP' (string name)

intFieldP' :: Parser String -> Parser (String, Int)
intFieldP' nameParser = do
    _ <- string ":i "
    name <- nameParser
    _ <- char ' '
    value <- read <$> many1 digit
    _ <- char '\n'
    return (name, value)

anyIntField :: Parser (String, Int)
anyIntField = intFieldP' (many1 letter)

intFieldP :: String -> Parser Int
intFieldP name = snd <$> intFieldP' (string name)
