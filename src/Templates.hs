{-# LANGUAGE TemplateHaskell #-}

module Templates where

import Text.Heterocephalus
import Text.Blaze.Renderer.String (renderMarkup)

templateDigest :: String -> String
templateDigest title =
  renderMarkup $(compileHtmlFile "resources/templates/digest.html")
