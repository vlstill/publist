--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, LambdaCase #-}

import           Data.Monoid
import           Data.Maybe
import           Data.Ord
import           Data.Char
import qualified Data.Map as M
import           Data.List

import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Time.LocalTime

import           Data.Typeable ( Typeable )
import           Data.Data ( Data )
import           GHC.Generics ( Generic )
import           Data.Binary
import           Data.ByteString.Lazy ( ByteString )

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
import           Data.IxSet


--------------------------------------------------------------------------------
main :: IO ()
main = do
    let bibdir = "bib"
    files <- map (bibdir </>) <$> getRecursiveContents (const $ pure False) bibdir
    bs <- parseBibFiles files
    case bs of
        Right bibs -> runHakyll bibs
        Left emsg  -> do
            hPutStrLn stderr $ unlines
                [ "Bibliography error: "
                , emsg
                , ""
                , "Refusing to re-generated pages, please fix your BIBs"
                ]
            exitFailure

runHakyll :: IxSet BibEntry -> IO ()
runHakyll pubs = hakyll $ do

    match "css/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

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
    tr = M.fromList
        [ (' ', '-'), ('\n', '-'), ('*', '-'), ('\t', '-')
        , ('ě', 'e'), ('š', 's'), ('č', 'c'), ('ř', 'r'), ('ž', 'z')
        , ('ý', 'y'), ('á', 'a'), ('í', 'i'), ('é', 'e'), ('ú', 'u')
        , ('ů', 'u'), ('ó', 'o')
        , (',', '-'), (';', '-')
        ]

down :: (a -> a -> Ordering) -> a -> a -> Ordering
down cmp a b = case cmp a b of
                  EQ -> EQ
                  LT -> GT
                  GT -> LT

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

timestamp :: LocalTime
timestamp = unsafePerformIO $ do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    return $ utcToLocalTime timezone now

debug :: Show a => a -> a
debug x = unsafePerformIO (print x) `seq` x
