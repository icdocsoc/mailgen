{-# LANGUAGE Arrows #-}
module Main where

-- Imports

import System.Environment (getArgs)
import System.IO
import System.Process
import Control.Monad
import System.Exit
import Data.Maybe

import Text.XML.HXT.Core

-- The known emailXSDPath
emailXSDPath = "resources/templates/email.xsd"

validateEmailXML :: String -> IO Bool
validateEmailXML = (flip validateXML) emailXSDPath

-- Validate a given XML file with a known XSD schema
validateXML :: String -> String -> IO Bool
validateXML xml xsd = do
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell ("xmllint --noout --schema " ++ xsd ++ " " ++ xml)){ std_out = CreatePipe } ""
  case exitCode of ExitSuccess -> return True
                   (ExitFailure code) -> do
                     putStrLn $ "Validation failed with code " ++ (show code)
                     putStrLn stderr
                     return False

-- Util
putStrLnError :: String -> IO ()
putStrLnError = hPutStrLn stderr

printHelpString :: IO ()
printHelpString = putStrLnError "mailgen <xml file>"

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
  let emailXMLPath = "resources/templates/email.xml"

  validXML <- validateEmailXML emailXMLPath
  validate validXML (putStrLn "Email XML validation failed")

  tree <- runX $ readDocument [ withValidate no,
                                withParseByMimeType no,
                                withCheckNamespaces no,
                                withInputEncoding Text.XML.HXT.Core.utf8 ]
                                emailXMLPath

                 >>> proc x -> do {
                     string <- writeDocumentToString [] -< x;
                     returnA -< (string, x)
                   }
                 >>> proc x -> do {
                     let (str, _) = x;
                     returnA -< str
                   }

                 -- >>> proc x -> do
                 --   strng <- writeDocumentToString [] -< x
                 --   returnA -< (first x &&& (arr strng))

  putStrLn $ concat tree

  return ()
