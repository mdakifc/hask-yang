{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module HaskYang where

-- TODO: Support YANG 1.1
-- We woud want to use the same types but annotate some of the types
-- with additional alternatives or fields of alternatives
-- We don't want to duplicate work rather, we would want to just "annotate"
-- pyang uses an additional field in an alternative tuple called "$1.1"
-- to annotate the existing grammar while differentiating between the 2
-- syntax.
-- Could we use template haskell to generate 2 grammars and 2 parsers
-- without duplicating work?

import Control.Monad (join)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Functor (unzip)

import Grammar
import ParserHelpers
import Prelude hiding (unzip)
import Text.Parsec
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Perm

--------------------------------------------------------------------------------
---------------------------------- Parsers -------------------------------------
--------------------------------------------------------------------------------
parseYang :: Parser Yang
parseYang = undefined

parseYangModule :: Parser YangModule
parseYangModule = undefined

-- TODO: You need to be careful when using identifier having the same name as
--       a keyword. Look at the entry for try
-- An identifier MUST NOT start with (('X'|'x') ('M'|'m') ('L'|'l'))
-- identifier          = (ALPHA / "_")
--                       *(ALPHA / DIGIT / "_" / "-" / ".")
-- TODO: Can we generate a "good" error message when we are not able to
--       match an identifier?
parseId :: String -> Parser Id
parseId s =
  let pId = (flip (<?>) "identifier") $ do
        x <- letter <|> char '_'
        xs <- many (letter <|> char '_' <|> digit <|> char '-' <|> char '.')
        pure $ BS.pack (x:xs)
  in skipSpaces $ do
    string s >> spaces
    res <- optQuotes pId
    pure res

parseYangVersion :: Parser ByteString
parseYangVersion = skipSpaces $ do
  string "yang-version" >> spaces
  version <- many (digit <|> char '.')
  pure $ BS.pack version

parseYangModHeader :: Parser YangModHeader
parseYangModHeader = do
  let
     -- TODO: generate an error if version isn't "1" or "1.1"
     parseVersion = parseYangVersion <* (char ';')
     -- TODO: parse uri
     parseNamespace = parseId "namespace" <* (char ';')
     parsePrefix = parseId "prefix" <* (char ';')
  -- order agnostic
  permute $ YangModHeader
            -- TODO: why does optionMaybe work in this case?
            <$$> optionMaybe parseVersion
            <||> parseNamespace
            <||> parsePrefix

parseYangModLinkage :: Parser YangModLinkage
parseYangModLinkage =
  let
    -- TODO: date
    parseRevisionDate = parseId "revision-date" <* (char ';') <* spaces
    parseImportBlock :: Parser (Id, Maybe Date)
    parseImportBlock =
      skipSpaces $ do
        let
          parsePrefix = parseId "prefix" <* (char ';') <* spaces
        -- Note: can't use optionMaybe => even though the type system allows it
        --       the functionality doesn't match
        permute $ ((,)) <$$> parsePrefix <|?> (Nothing, fmap Just parseRevisionDate)
    parseIncludeBlock :: Parser (Maybe Date)
    parseIncludeBlock = skipSpaces $ optionMaybe parseRevisionDate
    parseImport =
      skipSpaces $ do
        importId <- parseId "import" <* spaces
        mBlock <- optionMaybe $ between (char '{' >> spaces) (spaces >> char '}') parseImportBlock
        pure . uncurry (YangImport importId) . fmap join . unzip $ mBlock
    parseInclude =
      skipSpaces $ do
        includeId <- parseId "include" <* spaces
        mBlock <- optionMaybe $ between (char '{' >> spaces) (spaces >> char '}') parseIncludeBlock
        pure $ YangInclude includeId (join mBlock)
  in try parseImport <|> parseInclude
