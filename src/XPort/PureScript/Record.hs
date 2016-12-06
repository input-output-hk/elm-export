{-# LANGUAGE OverloadedStrings #-}
module XPort.PureScript.Record
  ( toPSTypeRef
  , toPSTypeRefWith
  , toPSTypeSource
  , toPSTypeSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           Formatting
import           XPort.Common
import           XPort.PureScript.Type

class HasType a where
  render :: a -> Reader Options Text

class HasTypeRef a where
  renderRef :: a -> Reader Options Text

instance HasType PSDatatype where
    render d@(PSDatatype _ constructor@(PSRecordConstructor _ _)) =
        sformat ("type " % stext % " =" % cr % stext) <$> renderRef d <*> render constructor
    render d@(PSDatatype _ constructor@(PSMultipleConstructors _)) =
        sformat ("type " % stext % cr % "  = " % stext) <$> renderRef d <*> render constructor
    render d@(PSDatatype _ constructor@(PSNamedConstructor _ _)) =
        sformat ("type " % stext % cr % "  = " % stext) <$> renderRef d <*> render constructor
    render (PSPrimitive primitive) = renderRef primitive

instance HasTypeRef PSDatatype where
    renderRef (PSDatatype typeName _) =
        pure typeName

    renderRef (PSPrimitive primitive) =
        renderRef primitive


instance HasType PSConstructor where
    render (PSRecordConstructor _ value) =
        sformat ("  { " % stext % cr % "  }") <$> render value
    render (PSNamedConstructor constructorName value) =
        sformat (stext % stext) constructorName <$> render value
    render (PSMultipleConstructors constructors) =
        fmap (Data.Text.intercalate "\n  | ") . sequence $ render <$> constructors


instance HasType PSValue where
    render (PSRef name) = pure name
    render PSEmpty = pure ""
    render (PSValues x y) =
        sformat (stext % cr % "  , " % stext) <$> render x <*> render y
    render (PSPrimitiveRef primitive) = sformat (" " % stext) <$> renderRef primitive
    render (PSField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat (stext % " ::" % stext) (fieldModifier name) <$> render value


instance HasTypeRef PSPrimitive where
    renderRef (PSList (PSPrimitive PSChar)) = renderRef PSString
    renderRef (PSList datatype) = sformat ("List " % stext) <$> renderRef datatype
    renderRef (PSTuple2 x y) =
        sformat ("(" % stext % ", " % stext % ")") <$> renderRef x <*> renderRef y
    renderRef (PSMaybe datatype) =
        sformat ("Maybe " % stext) <$> renderRef datatype
    renderRef (PSDict k v) =
        sformat ("Dict (" % stext % ") (" % stext % ")") <$> renderRef k <*> renderRef v
    renderRef PSInt = pure "Int"
    renderRef PSDate = pure "Date"
    renderRef PSBool = pure "Bool"
    renderRef PSChar = pure "Char"
    renderRef PSString = pure "String"
    renderRef PSUnit = pure "()"
    renderRef PSFloat = pure "Number"

toPSTypeRefWith :: PSType a => Options -> a -> Text
toPSTypeRefWith options x = runReader (renderRef (toPSType x)) options

toPSTypeRef :: PSType a => a -> Text
toPSTypeRef = toPSTypeRefWith defaultOptions

toPSTypeSourceWith :: PSType a => Options -> a -> Text
toPSTypeSourceWith options x = runReader (render (toPSType x)) options

toPSTypeSource :: PSType a => a -> Text
toPSTypeSource = toPSTypeSourceWith defaultOptions
