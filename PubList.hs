{-# LANGUAGE NamedFieldPuns, RecordWildCards, GeneralizedNewtypeDeriving
           , DeriveDataTypeable #-}

module PubList where

import qualified Text.BibTeX.Entry as Bib
import qualified Text.BibTeX.Format as Bib
import qualified Text.BibTeX.Parse as Bib

import Text.Parsec ( parse, skipMany1 )
import Text.Parsec.Char ( space )
import Text.Read ( readMaybe )

import Data.IxSet ( ixGen, ixFun, ixSet, Indexable, Proxy ( Proxy ), IxSet )
import qualified Data.IxSet as Ix ( empty, fromList, union )
import Data.String ( IsString )
import Data.Typeable ( Typeable )
import Data.Data ( Data )
import Data.Maybe ( maybe, fromMaybe )
import Data.Char ( isSpace )
import Data.List ( foldl' )

import Control.Arrow
import Control.Applicative

-- | remove hyphenation hint (\-) from text
fixHyphenation :: String -> String
fixHyphenation [] = []
fixHyphenation ('\\':'-':xs) = fixHyphenation xs
fixHyphenation (x:xs) = x : fixHyphenation xs

parseBibFiles :: [FilePath] -> IO (Either String (IxSet BibEntry))
parseBibFiles fs = fmap (foldl' Ix.union Ix.empty) . sequence <$> mapM parseBibFile fs

-- | Parse bibliography from file
parseBibFile :: FilePath -> IO (Either String (IxSet BibEntry))
parseBibFile path = parseBib path <$> readFile path 

-- | Parse bibliography from file contents; given file path (for parsec info)
-- and file contents it retuns either parse error or representation in for of
-- list of @Text.BibTeX.T@ entries
parseBib' :: FilePath -> String -> Either String [Bib.T]
parseBib' path = fixHyphenation >>>
                 parse (Bib.skippingLeadingSpace Bib.file) path >>>
                 show +++ map Bib.lowerCaseFieldNames

-- | Parse bibliography from file contents; given file path (for parsec info)
-- and file contents it retuns either parse error or indexed set fo 'BibEntry'
parseBib :: FilePath -> String -> Either String (IxSet BibEntry)
parseBib path = parseBib' path >>> right (map convert >>> Ix.fromList)
  where
    convert Bib.Cons { entryType = t, .. } = BibEntry {..}
      where
        entryId = Id identifier
        entryType = Type t
        authors = maybe [] parseAuthors (lookup "author" fields)
        name = Name $ fromMaybe "" (lookup "title" fields)
        year = Year $ (lookup "year" fields >>= readMaybe)
               <|> (lookup "date" fields >>= parseYear)
               <|> (lookup "issue_date" fields >>= parseYear)
        entries = fields

-- | we expect that year is last part of date, and it is separated by either
-- space, comma, or dash
parseYear :: String -> Maybe Int
parseYear = reverse >>>
            takeWhile (/= ' ') >>> takeWhile (/= ',') >>> takeWhile (/= '-') >>>
            reverse >>>
            readMaybe

-- | parse @"and"@ separated authors
parseAuthors :: String -> [String]
parseAuthors str = case pa (trimR (conv str)) "" of
    ([], []) -> []
    (a, [])  -> [trim a]
    ([], as) -> parseAuthors as
    (a, as)  -> trim a : parseAuthors as
  where
    pa [] acc = (reverse acc, [])
    pa " and" acc = (reverse acc, [])
    pa (' ':'a':'n':'d':' ':xs) acc = (reverse acc, ' ':xs)
    pa (x:xs) acc = pa xs (x:acc)

    -- | convert all whitespace to spaces
    conv :: String -> String
    conv = map conv1
      where
    conv1 x
      | isSpace x = ' '
      | otherwise = x

-- | remove tailing and leading whitespace
trim :: String -> String
trim = trimR >>> dropWhile isSpace

trimR = reverse >>> dropWhile isSpace >>> reverse

newtype Id = Id String deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype Type = Type String deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype Name = Name String deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype Year = Year (Maybe Int) deriving ( Eq, Ord, Show, Data, Typeable )
newtype Author = Author String deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype FirstAuthor = FirstAuthor String deriving ( IsString, Eq, Ord, Show, Data, Typeable )

data BibEntry = BibEntry
    { entryId   :: Id
    , entryType :: Type
    , authors   :: [String]
    , name      :: Name
    , year      :: Year
    , entries   :: [(String, String)]
    } deriving ( Show, Eq, Ord, Data, Typeable )

instance Indexable BibEntry where
    empty = ixSet
              [ ixGen (Proxy :: Proxy Id)
              , ixGen (Proxy :: Proxy Type)
              , ixGen (Proxy :: Proxy Year)
              , ixGen (Proxy :: Proxy Name)
              , ixFun $ \BibEntry {..} -> if null authors then [] else [FirstAuthor (head authors)]
              , ixFun $ \BibEntry {..} -> map Author authors
              ]
