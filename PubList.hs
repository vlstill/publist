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
parseKeywords :: String -> [String]
parseKeywords = pk >>> map trim >>> filter (not . null) >>> map (words >>> unwords >>> map toLower >>> debrace)
  where
    pk xs = case break (`elem` ",;") xs of
              (x, [])   -> [x]
              (x, _:xs) -> x : pk xs                      

-- | parse @"and"@ separated authors
parseAuthors :: String -> [String]
parseAuthors = parse >>> map trim >>> filter (not . null) >>> map (words >>> unwords >>> debrace)
  where
    parse str = case pa (trimR (conv str)) "" of
        (a, [])  -> [trim a]
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

newtype Id = Id { getId :: String }
             deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype Type = Type { getType :: String }
               deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype Name = Name { getName :: String }
               deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype Year = Year { getYear :: Maybe Int }
               deriving ( Eq, Ord, Show, Data, Typeable )
newtype Author = Author { getAuthor ::  String }
                 deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype FirstAuthor = FirstAuthor { getFirstAuthor :: String }
                      deriving ( IsString, Eq, Ord, Show, Data, Typeable )
newtype Keyword = Keyword { getKeyword :: String }
                  deriving ( IsString, Eq, Ord, Show, Data, Typeable )

data BibEntry = BibEntry
    { entryId   :: Id
    , entryType :: Type
    , authors   :: [String]
    , name      :: Name
    , year      :: Year
    , keywords  :: [String]
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
              , ixFun $ \BibEntry {..} -> map Keyword keywords
              ]

data Bibliography = Bibliography
    { database    :: IxSet BibEntry
    , allKeywords :: Set String
    , allAuthors  :: Set String
    } deriving ( Show, Eq, Ord, Data, Typeable )

instance Monoid Bibliography where
    mempty = Bibliography { database = Ix.empty, allKeywords = Set.empty, allAuthors = Set.empty }
    mappend a b = Bibliography { database = database a `Ix.union` database b
                               , allAuthors = allAuthors a `Set.union` allAuthors b
                               , allKeywords = allKeywords a `Set.union` allKeywords b
                               }
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
