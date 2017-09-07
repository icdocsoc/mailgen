module Main where

-- Imports

import System.Environment (getArgs)
import System.IO
import System.Directory
import Control.Monad
import System.Exit

import qualified Data.Set as Set
import Text.Pandoc hiding (pandocExtensions)
import Text.Pandoc.Builder
import Text.Pandoc.Walk
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Pandoc.Options as PanOptions (def)

-- Options

pandocExtensions :: Set.Set Extension
pandocExtensions = Set.fromList [ Ext_yaml_metadata_block,
                                  Ext_raw_html,
                                  Ext_backtick_code_blocks,
                                  Ext_fenced_code_blocks,
                                  Ext_fenced_code_attributes,
                                  Ext_inline_code_attributes,
                                  Ext_strikeout,
                                  Ext_superscript,
                                  Ext_subscript,
                                  Ext_emoji,
                                  Ext_link_attributes,
                                  Ext_markdown_in_html_blocks ]

pandocReaderOptions :: ReaderOptions
pandocReaderOptions =
  PanOptions.def{ readerExtensions = pandocExtensions }

pandocWriterOptions :: WriterOptions
pandocWriterOptions =
  PanOptions.def{ writerExtensions = pandocExtensions,
                  writerTableOfContents = False,
                  writerHtml5 = True,
                  writerHighlight = True }

pandocWriterOptionsWithTemplate :: String -> WriterOptions
pandocWriterOptionsWithTemplate template =
  pandocWriterOptions{ writerTemplate = Just template }

-- Pandoc Filters

-- Create the agenda metadata
populateAgenda :: Pandoc -> Pandoc
populateAgenda pandoc@(Pandoc meta blocks) =
  Pandoc meta' blocks
  where
    collectAgendaItems :: Block -> [[Inline]]
    collectAgendaItems (Header 1 _ contents) = [contents]
    collectAgendaItems _ = []

    metaAgenda = MetaList $ map MetaInlines (query collectAgendaItems pandoc)
    meta' = case lookupMeta "show_agenda" meta of
              Just (MetaBool True) -> setMeta "agenda" metaAgenda meta
              _ -> meta

-- Chain filters here
pandocFilter :: Pandoc -> Pandoc
pandocFilter = populateAgenda

processMarkdown :: String -> String -> Either String Html
processMarkdown markdown template =
  case (readMarkdown pandocReaderOptions markdown) of
    Left error -> Left (extractErrorString error)
    Right pandoc -> Right $ writeHtml template' (pandocFilter pandoc)
  where
    extractErrorString :: PandocError -> String
    extractErrorString (ParseFailure s) = s
    extractErrorString (ParsecError _ _ ) = "Parsec Error"

    template' = pandocWriterOptionsWithTemplate template

-- Util
putStrLnError :: String -> IO ()
putStrLnError = hPutStrLn stderr

printHelpString :: IO ()
printHelpString = putStrLnError "mailgen <markdown file> <template file>"

printFileNotExistString :: String -> IO ()
printFileNotExistString filename = putStrLnError $ "file " ++ filename ++ " does not exist"

-- Validation
validate :: Bool -> IO () -> IO ()
validate condition otherwise =
  if not condition then
    otherwise >> exitFailure
    else
      return ()

-- Main Function

main :: IO ()
main = do
  args <- getArgs

  -- Validate the argument length
  validate (length args >= 2) printHelpString

  -- Validate that the files exist
  let (markdownFile, templateFile) = (args !! 0, args !! 1)

  markdownIsFile <- doesFileExist markdownFile
  validate markdownIsFile (printFileNotExistString markdownFile)
  templateIsFile <- doesFileExist templateFile
  validate templateIsFile (printFileNotExistString templateFile)

  -- Read the files
  markdownContents <- readFile markdownFile
  templateContents <- readFile templateFile

  -- Output the HTML or an error
  let htmlOrError = processMarkdown markdownContents templateContents
  case htmlOrError of
    Left error -> putStrLnError error
    Right html -> putStrLn $ renderHtml html

  return ()
