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
import           System.Directory ( getDirectoryContents )

import           Control.Monad
import           Control.Applicative
import           Control.Arrow

import           Text.Pandoc.Options

import           Hakyll

import           PubList
import           Data.IxSet ( IxSet, (@=), groupDescBy, toDescList, Proxy ( Proxy ) )
import qualified Data.IxSet as Ix ( toList, toAscList )
import           Data.Set ( Set )
import qualified Data.Set as Set ( fromList, member )


--------------------------------------------------------------------------------
main :: IO ()
main = do
    let bibdir = "bib"
        pdfdir = "pdf"
    pdfs <- filter ((&&) <$> (/= ".") <*> (/= "..")) <$> getDirectoryContents pdfdir
    files <- map (bibdir </>) <$> getRecursiveContents (pure . (/= ".bib") . takeExtension) bibdir
    bs <- parseBibFiles files
    case bs of
        Right bibs -> do
            hPutStrLn stderr $ "Authors: " ++ intercalate "; " (map getAuthor (toList (allAuthors bibs)))
            hPutStrLn stderr $ "Keywords: " ++ intercalate "; " (map getKeyword (toList (allKeywords bibs)))
            runHakyll bibs (Set.fromList pdfs)
        Left emsg  -> do
            hPutStrLn stderr $ unlines
                [ "Bibliography error: "
                , emsg
                , ""
                , "Refusing to re-generated pages, please fix your BIBs"
                ]
            exitFailure

runHakyll :: Bibliography -> Set FilePath -> IO ()
runHakyll Bibliography {..} pdfs = hakyll $ do

    match "css/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pdf/*" $ do
        route idRoute
        compile copyFileCompiler

    match "bib/*.bib" $ do
        compile getResourceBody

    createList allKeywords (database @=) getKeyword (("Keyword: " ++) . heading) pdfs "keywords/*.md"
    createList allAuthors (database @=) getAuthor id pdfs "authors/*.md"
    createList ["index"] (const database) id (const "Publications") pdfs "*.md"

    bibdep $ create ["keywords/index.md"] $ do
        route $ setExtension "html"
        compile $ makeItem (mklist "keywords" (getKeyword <$> toList allKeywords)) >>=
                  render >>=
                  loadAndApplyTemplate "templates/default.html" (constField "title" "Keyword List" <> defContext) >>=
                  relativizeUrls

    bibdep $ create ["authors/index.md"] $ do
        route $ setExtension "html"
        -- here, toList is important to preserve sort by surname:
        compile $ makeItem (mklist "authors" (getAuthor <$> toList allAuthors)) >>=
                  render >>=
                  loadAndApplyTemplate "templates/default.html" (constField "title" "List of Authors" <> defContext) >>=
                  relativizeUrls

    createBibs database "bib/*.bib"

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

-- | create bib export for each entry in database, and uses "all" pattern for
-- list of all entries (sorthed by first author)
createBibs :: IxSet BibEntry -> Pattern -> Rules ()
createBibs db pat = do
    bibdep $ create [ fromCapture pat "all" ] $ do
        route idRoute
        compile $ makeItem (intercalate "\n\n" $ map formatBib (Ix.toAscList (Proxy :: Proxy FirstAuthor) db))
    forM_ (Ix.toList db) $ \e@BibEntry {..} -> do
        bibdep $ create [ fromCapture pat (getId entryId) ] $ do
            route idRoute
            compile $ makeItem (formatBib e)


-- | render pandoc list of links from a list of keywords/authors
mklist :: Foldable f => String -> f String -> String
mklist pathprefix = toList >>> map link >>> unlines
  where
    link x = "*   [" ++ x ++ "](/" ++ pathprefix ++ "/" ++ ident x ++ ".html)"

createList :: Foldable f => f a -> (a -> IxSet BibEntry) -> (a -> String) -> (String -> String)-> Set FilePath -> Pattern -> Rules ()
createList keys getSet keyToStr toName pdfs pat = bibdep $ do
    createMany keys keyToStr pat $ \a -> do
        route idRoute
        compile $ makeItem (biblist pdfs (getSet a))

    createMany keys keyToStr pat $ \a -> version "html" $ do
        route $ setExtension "html"
        compile $ makeItem (biblist pdfs (getSet a)) >>=
                  render >>=
                  loadAndApplyTemplate "templates/default.html" (constField "title" (toName . keyToStr $ a) <> defContext) >>=
                  relativizeUrls

bibdep r = makePatternDependency "bib/*.bib" >>= \d -> rulesExtraDependencies [d] r

idReplaceExtension :: String -> Identifier -> Identifier
idReplaceExtension ext = toFilePath >>> flip replaceExtension ext >>> fromFilePath

render = renderPandocWith defaultHakyllReaderOptions (defaultHakyllWriterOptions { writerHtml5 = True })

-- | Render bibliography database in markdown, descending by year
biblist :: Set FilePath -> IxSet BibEntry -> String
biblist pdfs = groupDescBy >>> map (renderYear pdfs) >>> intercalate "\n\n"

renderYear :: Set FilePath -> (Year, [BibEntry]) -> String
renderYear pdfs (Year y, es) = "## " ++ year ++ "\n\n" ++ intercalate "\n\n" (map (renderBib pdfs) (sortBy autsort es))
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
renderBib :: Set FilePath -> BibEntry -> String
renderBib pdfs (BibEntry {..}) = unlines $
    [ aut authors
    , "**" ++ (nam name) ++ "**, "
    , intercalate ", " (book ++ publisher ++ year' ++ volume ++ pages) ++ "."
    , "[" ++ intercalate ", " links ++ "]" ]
  where
    aut = map (getAuthor >>> autLink) >>> autList
    autLink a = "[" ++ a ++ "](/authors/" ++ ident a ++ ".html)"
    autList :: [String] -> String
    autList [] = ""
    autList [x] = x ++ ": "
    autList [x, y] = x ++ " and " ++ y ++ ": "
    autList xs = intercalate ", " (init xs) ++ ", and " ++ last xs ++ ": "
    nam = getName

    book = maybe [] (wrap "*") (lookup "booktitle" entries)
    publisher = maybe [] (wrap "") (lookup "publisher" entries)
    volume = maybe [] v $ (,) <$> lookup "volume" entries <*> lookup "series" entries
    v :: (String, String) -> [String]
    v (vol, ser) = ["volume " ++ vol ++ " of " ++ ser]
    pages = maybe [] (wrap "") (lookup "pages" entries)
    year' = maybe [] ((:[]) . show) (getYear year)

    path ext = "/" ++ ext ++ "/" ++ getId entryId ++ "." ++ ext
    links = [ "[bibtex](" ++ path "bib" ++ ")" ] ++ pdf ++ url
    url = maybe [] (:[]) $ fmap (\x -> "[url](http://dx.doi.org/" ++ x ++ ")") (lookup "doi" entries)
                       <|> fmap (\x -> "[url](" ++ x ++ ")") (lookup "url" entries)
    pdf = let p = path "pdf" in if Set.member p pdfs then ["[pdf](pdf/" ++ p ++ ")"] else []

    wrap x xs = [x ++ filter (`notElem` ['{', '}']) xs ++ x]

-- | Create a set of rules based on set of string values and patterns, the
-- hole in pattern will be filled by values from the string list (after 'ident'
-- is applied to them), and each time rule callback will get original value
-- from list as its parameter
createMany :: Foldable f => f a -> (a -> String) -> Pattern -> (a -> Rules ()) -> Rules ()
createMany xs keyToStr pat rules = forM_ (toList xs) $ \x -> create [fromCapture pat (ident (keyToStr x))] (rules x)

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
                                 else show firstyear ++ "–" ++ show year
    firstyear = 2016

timestamp :: LocalTime
timestamp = unsafePerformIO $ do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    return $ utcToLocalTime timezone now

debug :: Show a => a -> a
debug x = unsafePerformIO (print x) `seq` x
