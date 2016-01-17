--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, LambdaCase
           , NamedFieldPuns, RecordWildCards #-}

import           Data.Monoid
import           Data.Maybe
import           Data.Ord
import           Data.Char
import qualified Data.Map as M
import           Data.List
import           Data.Foldable ( toList )

import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Time.LocalTime

import           Data.Typeable ( Typeable )
import           Data.Data ( Data )
import           GHC.Generics ( Generic )
import           Data.Binary
import           Data.ByteString.Lazy ( ByteString )
import           Data.Char

import           System.IO.Unsafe ( unsafePerformIO )
import           System.FilePath
import           System.IO ( hPutStrLn, stderr )
import           System.Exit

import           Control.Monad
import           Control.Applicative
import           Control.Arrow

import           Text.Pandoc.Options

import           Hakyll

import           PubList
import           Data.IxSet ( IxSet, (@=), groupDescBy, toDescList, Proxy ( Proxy ) )
import           Data.Set ( Set )


--------------------------------------------------------------------------------
main :: IO ()
main = do
    let bibdir = "bib"
    files <- map (bibdir </>) <$> getRecursiveContents (pure . (/= ".bib") . takeExtension) bibdir
    bs <- parseBibFiles files
    case bs of
        Right bibs -> do
            hPutStrLn stderr $ "Authors: " ++ intercalate "; " (toList (allAuthors bibs))
            hPutStrLn stderr $ "Keywords: " ++ intercalate "; " (toList (allKeywords bibs))
            runHakyll bibs
        Left emsg  -> do
            hPutStrLn stderr $ unlines
                [ "Bibliography error: "
                , emsg
                , ""
                , "Refusing to re-generated pages, please fix your BIBs"
                ]
            exitFailure

runHakyll :: Bibliography -> IO ()
runHakyll Bibliography {..} = hakyll $ do

    match "css/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pdf/*" $ do
        route idRoute
        compile copyFileCompiler

    createList allKeywords (\k -> database @= Keyword k) ("Keyword: " ++) "keywords/*.md"
    createList allAuthors (\a -> database @= Author a) id "authors/*.md"
    createList ["index"] (const database) id "*.md"

    create ["keywords/index.md"] $ do
        route $ setExtension "html"
        compile $ makeItem (mklist "keywords" allKeywords) >>=
                  render >>=
                  loadAndApplyTemplate "templates/default.html" (constField "title" "Keyword List" <> defContext) >>=
                  relativizeUrls

    create ["authors/index.md"] $ do
        route $ setExtension "html"
        compile $ makeItem (mklist "authors" allAuthors) >>=
                  render >>=
                  loadAndApplyTemplate "templates/default.html" (constField "title" "List of Authors" <> defContext) >>=
                  relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

-- | render pandoc list of links from a list of keywords/authors
mklist :: Foldable f => String -> f String -> String
mklist pathprefix = toList >>> map link >>> unlines
  where
    link x = "*   [" ++ x ++ "](/" ++ pathprefix ++ "/" ++ ident x ++ ".html)"

createList :: Foldable f => f String -> (String -> IxSet BibEntry) -> (String -> String) -> Pattern -> Rules ()
createList keys getSet toName pat = do
    createMany keys pat $ \a -> do
        route idRoute
        compile $ makeItem (biblist (getSet a))

    createMany keys pat $ \a -> version "html" $ do
        route $ setExtension "html"
        compile $ makeItem (biblist (getSet a)) >>=
                  render >>=
                  loadAndApplyTemplate "templates/default.html" (constField "title" a <> defContext) >>=
                  relativizeUrls


idReplaceExtension :: String -> Identifier -> Identifier
idReplaceExtension ext = toFilePath >>> flip replaceExtension ext >>> fromFilePath

render = renderPandocWith defaultHakyllReaderOptions (defaultHakyllWriterOptions { writerHtml5 = True })

-- | Render markdown for a keyword
keyword :: String -> IxSet BibEntry -> String
keyword k db = biblist (db @= Keyword k) ++ "\n"

-- | Render markdown for an author
author :: String -> IxSet BibEntry -> String
author k db = biblist (db @= Author k) ++ "\n"

-- | Render bibliography database in markdown, descending by year
biblist :: IxSet BibEntry -> String
biblist = groupDescBy >>> map renderYear >>> intercalate "\n\n"

renderYear :: (Year, [BibEntry]) -> String
renderYear (Year y, es) = "## " ++ year ++ "\n\n" ++ intercalate "\n\n" (map renderBib (sortBy autsort es))
  where
    year = case y of
              Just yy -> show yy
              Nothing -> "No Year"
    autsort = comparing authors

-- | capitalize first letters for heading
heading :: String -> String
heading = words >>> map fu >>> unwords
  where
    fu [] = []
    fu (x:xs) = toUpper x : xs

-- | render single 'BibEntry' into markdown
renderBib :: BibEntry -> String
renderBib (BibEntry {..}) = unlines $
    [ aut authors ++ ":"
    , "**" ++ (nam name) ++ "**"
    , intercalate ", " (book ++ publisher ++ year' ++ volume ++ pages) ++ "."
    , "[" ++ intercalate ", " links ++ "]" ]
  where
    aut = intercalate ", "
    nam = getName >>> filter (`notElem` ['{', '}'])

    book = maybe [] (wrap "*") (lookup "booktitle" entries)
    publisher = maybe [] (wrap "") (lookup "publisher" entries)
    volume = maybe [] v $ (,) <$> lookup "volume" entries <*> lookup "series" entries
    v :: (String, String) -> [String]
    v (vol, ser) = ["volume " ++ vol ++ " of " ++ ser]
    pages = maybe [] (wrap "") (lookup "pages" entries)
    year' = maybe [] ((:[]) . show) (getYear year)

    path ext = "/" ++ ext ++ "/" ++ getId entryId ++ "." ++ ext
    links =
        [ "[bibtex](" ++ path "bib" ++ ")"
        , "[pdf](" ++ path "pdf" ++ ")"
        ] ++ doi
    doi = case lookup "doi" entries of
            Nothing -> []
            Just x  -> [ "[url](http://dx.doi.org/" ++ x ++ ")" ]

    wrap x xs = [x ++ filter (`notElem` ['{', '}']) xs ++ x]

-- | Create a set of rules based on set of string values and patterns, the
-- hole in pattern will be filled by values from the string list (after 'ident'
-- is applied to them), and each time rule callback will get original value
-- from list as its parameter
createMany :: Foldable f => f String -> Pattern -> (String -> Rules ()) -> Rules ()
createMany xs pat rules = forM_ (toList xs) $ \x -> create [fromCapture pat (ident x)] (rules x)

-- | compile pandoc, but apply it (on itself) as template first
pandocTemplateCompiler :: Context String -> Compiler (Item String)
pandocTemplateCompiler ctx = getResourceBody >>= applyAsTemplate ctx >>= render
  where
    render = renderPandocWith defaultHakyllReaderOptions
        (defaultHakyllWriterOptions { writerHtml5 = True })

mapRoute :: (FilePath -> FilePath) -> Routes
mapRoute f = customRoute (f . toFilePath)

ident :: String -> String
ident = map (\x -> fromMaybe x (x `M.lookup` tr)) . map toLower
  where
    tr :: M.Map Char Char
    tr = M.fromList $
        [ (' ', '-'), ('\n', '-'), ('*', '-'), ('\t', '-')
        , (',', '-'), (';', '-')
        ] ++ lchrs ++ map (toUpper *** toUpper) lchrs
    lchrs = [ ('ě', 'e'), ('š', 's'), ('č', 'c'), ('ř', 'r'), ('ž', 'z')
            , ('ý', 'y'), ('á', 'a'), ('í', 'i'), ('é', 'e'), ('ú', 'u')
            , ('ů', 'u'), ('ó', 'o')
            ]

down :: (a -> a -> Ordering) -> a -> a -> Ordering
down cmp a b = case cmp a b of
                  EQ -> EQ
                  LT -> GT
                  GT -> LT

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

defContext :: Context String
defContext = constField "years" years <> defaultContext
  where
    (year, _, _) = toGregorian $ localDay timestamp
    years = if year == firstyear then show firstyear
                                 else show firstyear ++ " – " ++ show year
    firstyear = 2015

timestamp :: LocalTime
timestamp = unsafePerformIO $ do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    return $ utcToLocalTime timezone now

debug :: Show a => a -> a
debug x = unsafePerformIO (print x) `seq` x
