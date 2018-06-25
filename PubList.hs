{-# LANGUAGE NamedFieldPuns, RecordWildCards, GeneralizedNewtypeDeriving
           , DeriveDataTypeable #-}

module PubList where

import qualified Text.BibTeX.Entry as Bib ( T (..), lowerCaseFieldNames )
import qualified Text.BibTeX.Format as Bib ( entry )
import qualified Text.BibTeX.Parse as Bib ( skippingLeadingSpace, file )

import Text.Parsec ( parse, skipMany1 )
import Text.Parsec.Char ( space )
import Text.Read ( readMaybe )

import Data.IxSet ( ixGen, ixFun, ixSet, Indexable, Proxy ( Proxy ), IxSet )
import qualified Data.IxSet as Ix ( empty, fromList, union )
import Data.Set ( Set )
import qualified Data.Set as Set ( empty, fromList, union )

import Data.String ( IsString )
import Data.Typeable ( Typeable )
import Data.Data ( Data )
import Data.Maybe ( maybe, fromMaybe )
import Data.Char ( isSpace, toLower )
import Data.List ( foldl' )
import Data.Function ( on )
import Data.Semigroup ( Semigroup, (<>) )
import Data.Monoid ( mempty, mappend, mconcat )
import qualified Data.Text as T ( pack )
import qualified Data.Text.ICU.Normalize as T ( compare )

import Control.Arrow
import Control.Applicative

-- | remove hyphenation hint (\-) from text
fixHyphenation :: String -> String
fixHyphenation [] = []
fixHyphenation ('\\':'-':xs) = fixHyphenation xs
fixHyphenation (x:xs) = x : fixHyphenation xs

parseBibFiles :: [FilePath] -> IO (Either String Bibliography)
parseBibFiles fs = fmap mconcat . sequence <$> mapM parseBibFile fs

-- | Parse bibliography from file
parseBibFile :: FilePath -> IO (Either String Bibliography)
parseBibFile path = parseBib path <$> readFile path 

-- | Parse bibliography from file contents; given file path (for parsec info)
-- and file contents it retuns either parse error or representation in for of
-- list of @Text.BibTeX.T@ entries
parseBib' :: FilePath -> String -> Either String [Bib.T]
parseBib' path = fixHyphenation >>>
                 parse (Bib.skippingLeadingSpace Bib.file) path >>>
                 show +++ map Bib.lowerCaseFieldNames

-- | Parse bibliography from file contents; given file path (for parsec info)
-- and file contents it retuns either parse error or 'Bibliography'
parseBib :: FilePath -> String -> Either String Bibliography
parseBib path = parseBib' path >>> right (map convert >>> bib)
  where
    convert Bib.Cons { entryType = t, .. } = BibEntry {..}
      where
        entryId = Id identifier
        entryType = Type t
        authors = maybe [] parseAuthors (lookup "author" fields)
        name = Name $ fromMaybe "" (debrace <$> lookup "title" fields)
        year = Year $ (lookup "year" fields >>= readMaybe)
               <|> (lookup "date" fields >>= parseYear)
               <|> (lookup "issue_date" fields >>= parseYear)
        keywords = maybe [] parseKeywords (lookup "keywords" fields)
        entries = fields
    bib bs = Bibliography { database = Ix.fromList bs
                          , allKeywords = Set.fromList (concatMap keywords bs)
                          , allAuthors = Set.fromList (concatMap authors bs)
                          }

-- | we expect that year is last part of date, and it is separated by either
-- space, comma, or dash
parseYear :: String -> Maybe Int
parseYear = reverse >>>
            takeWhile (/= ' ') >>> takeWhile (/= ',') >>> takeWhile (/= '-') >>>
            reverse >>>
            readMaybe

-- | Parse comma or semicolon separated list of keywords, converts them to
-- lowercase
parseKeywords :: String -> [Keyword]
parseKeywords = pk >>> map trim >>> filter (not . null) >>> map (words >>> unwords >>> map toLower >>> debrace >>> Keyword)
  where
    pk xs = case break (`elem` ",;") xs of
              (x, [])   -> [x]
              (x, _:xs) -> x : pk xs                      

-- | parse @"and"@ separated authors
parseAuthors :: String -> [Author]
parseAuthors = parse >>> map trim >>> filter (not . null) >>> map (words >>> unwords >>> debrace >>> author)
  where
    parse str = case pa (trimR (conv str)) "" of
        (a, [])  -> [trim a]
        (a, as)  -> trim a : parse as
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

newtype Id          = Id          { getId :: String }
                      deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype Type        = Type        { getType :: String }
                      deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype Name        = Name        { getName :: String }
                      deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype Year        = Year        { getYear :: Maybe Int }
                      deriving ( Eq, Ord, Show, Data, Typeable )
data    Author      = Author      { getSurname :: String, getAuthor :: String }
                      deriving ( Eq, Show, Data, Typeable )
newtype FirstAuthor = FirstAuthor { getFirstAuthor :: Author }
                      deriving ( Eq, Ord, Show, Data, Typeable )
newtype Keyword     = Keyword     { getKeyword :: String }
                      deriving ( IsString, Eq, Ord, Show, Data, Typeable )

instance Ord Author where
    compare a b = case cmp (getSurname a) (getSurname b) of
                      EQ -> cmp (getAuthor a) (getAuthor b)
                      x  -> x
      where
        cmp = T.compare [] `on` T.pack

data BibEntry = BibEntry
    { entryId   :: Id
    , entryType :: Type
    , authors   :: [Author]
    , name      :: Name
    , year      :: Year
    , keywords  :: [Keyword]
    , entries   :: [(String, String)]
    } deriving ( Show, Eq, Ord, Data, Typeable )

instance Indexable BibEntry where
    empty = ixSet
              [ ixGen (Proxy :: Proxy Id)
              , ixGen (Proxy :: Proxy Type)
              , ixGen (Proxy :: Proxy Year)
              , ixGen (Proxy :: Proxy Name)
              , ixFun $ \BibEntry {..} -> if null authors then [] else [FirstAuthor (head authors)]
              , ixFun $ \BibEntry {..} -> authors
              , ixFun $ \BibEntry {..} -> keywords
              ]

-- | Construct 'Author' from string, using 'surname' to get surname
author str = Author { getSurname = surname str, getAuthor = str }

-- | return authors surname (the last name before fist comma, or full name if
-- there is nothing before first comma)
surname :: String -> String
surname str = ($ str) $ takeWhile (/= ',') >>> words >>> last' >>> fromMaybe str
  where
    last' [] = Nothing
    last' xs = Just (last xs)

data Bibliography = Bibliography
    { database    :: IxSet BibEntry
    , allKeywords :: Set Keyword
    , allAuthors  :: Set Author
    } deriving ( Show, Eq, Ord, Data, Typeable )

instance Semigroup Bibliography where
    a <> b = Bibliography { database = database a `Ix.union` database b
                          , allAuthors = allAuthors a `Set.union` allAuthors b
                          , allKeywords = allKeywords a `Set.union` allKeywords b
                          }

instance Monoid Bibliography where
    mempty = Bibliography { database = Ix.empty, allKeywords = Set.empty, allAuthors = Set.empty }
    mappend = (<>)
    mconcat xs = foldl' mappend mempty xs
    
-- | Remove braces ('{', '}')
debrace :: String -> String
debrace = filter (`notElem` ['{', '}'])

formatBib :: BibEntry -> String
formatBib BibEntry { entryType = t, ..} = Bib.entry $ Bib.Cons {..}
  where
    entryType = getType t
    identifier = getId entryId
    fields = entries
