module ParserHelpers where

import Text.Parsec
import Text.Parsec.ByteString ( Parser )

skipSpaces :: Parser a -> Parser a
skipSpaces p = spaces *> p <* spaces

optQuotes :: Parser a -> Parser a
-- TODO: use the label generated for p and don't generate any separate error message here
optQuotes p = p <|> between (char '\"') (char '\"') p
