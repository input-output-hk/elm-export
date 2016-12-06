{-# LANGUAGE OverloadedStrings #-}
module XPort.Common  where

import           Data.Text  (Text)
import           Formatting (Format, now)

data Options =
  Options {fieldLabelModifier :: Text -> Text}

defaultOptions :: Options
defaultOptions = Options {fieldLabelModifier = id}

cr :: Format r r
cr = now "\n"
