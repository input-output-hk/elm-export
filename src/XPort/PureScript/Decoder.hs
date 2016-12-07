{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module XPort.PureScript.Decoder
  ( toPSDecoderRef
  , toPSDecoderRefWith
  , toPSDecoderSource
  , toPSDecoderSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           Formatting
import           XPort.Common          (Options, cr, defaultOptions)
import           XPort.PureScript.Type (PSDatatype (..), PSType (..))

class HasDecoder a where
  render :: a -> Reader Options Text

class HasDecoderRef a where
  renderRef :: a -> Reader Options Text

class HasGenericRef a where
  renderGenericRef :: a -> Reader Options Text

instance HasDecoder PSDatatype where
    render d@(PSDatatype name _ _) = do
        genericName <- renderGenericRef d
        fnName <- renderRef d
        pure $ sformat
          ("derive instance " % stext % " :: Generic " % stext %
          cr % cr % "instance " % stext % " :: DecodeJson " % stext %
          " where" % cr % "  decodeJson = gDecodeJson")
          genericName
          name
          fnName
          name
    render (PSPrimitive _) = undefined

instance HasDecoderRef PSDatatype where
    renderRef (PSDatatype name _ _) =
        pure $ sformat ("decodeJson" % stext) name

    renderRef (PSPrimitive _) = undefined

instance HasGenericRef PSDatatype where
    renderGenericRef (PSDatatype name _ _) =
        pure $ sformat ("generic" % stext) name

    renderGenericRef (PSPrimitive _) = undefined


toPSDecoderRefWith :: PSType a => Options -> a -> Text
toPSDecoderRefWith options x = runReader (renderRef (toPSType x)) options

toPSDecoderRef :: PSType a => a -> Text
toPSDecoderRef = toPSDecoderRefWith defaultOptions

toPSDecoderSourceWith :: PSType a => Options -> a -> Text
toPSDecoderSourceWith options x = runReader (render (toPSType x)) options

toPSDecoderSource :: PSType a => a -> Text
toPSDecoderSource = toPSDecoderSourceWith defaultOptions
