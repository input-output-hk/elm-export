{-# LANGUAGE OverloadedStrings #-}
module XPort.Flow.Record
  ( toFlowTypeRef
  , toFlowTypeRefWith
  , toFlowTypeSource
  , toFlowTypeSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           Formatting
import           XPort.Common
import           XPort.Flow.Type

class HasType a where
  render :: a -> Reader Options Text

class HasTypeRef a where
  renderRef :: a -> Reader Options Text

instance HasType FlowDatatype where
    render d@(FlowDatatype _ constructor@(FlowRecordConstructor constructorName _)) =
        sformat ("export default type "  % stext % " =" % cr % stext) <$> renderRef d
          <*> render constructor
    render d@(FlowDatatype _ constructor@(FlowMultipleConstructors _)) =
        sformat ("export default type "  % stext % cr % "  = " % stext) <$> renderRef d <*> render constructor
    render d@(FlowDatatype _ constructor@(FlowNamedConstructor _ _)) =
        sformat ("export default type " % stext % "  = " % stext) <$> renderRef d <*> render constructor
    render (FlowPrimitive primitive) = renderRef primitive

instance HasTypeRef FlowDatatype where
    renderRef (FlowDatatype typeName _) =
        pure typeName

    renderRef (FlowPrimitive primitive) =
        renderRef primitive


instance HasType FlowConstructor where
    render (FlowRecordConstructor _ value) =
        sformat ("  { " % stext % cr % "  }") <$> render value
    render (FlowNamedConstructor constructorName value) =
        sformat ("\"" % stext % "\"" % stext) constructorName <$> render value
    render (FlowMultipleConstructors constructors) =
        fmap (Data.Text.intercalate "\n  | ") . sequence $ render <$> constructors


instance HasType FlowValue where
    render (FlowRef name) = pure name
    render FlowEmpty = pure ""
    render (FlowValues x y) =
        sformat (stext % cr % "  , " % stext) <$> render x <*> render y
    render (FlowPrimitiveRef primitive) = sformat (" " % stext) <$> renderRef primitive
    render (FlowField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat (stext % ":" % stext) (fieldModifier name) <$> render value


instance HasTypeRef FlowPrimitive where
    renderRef (FlowList (FlowPrimitive FlowChar)) = renderRef FlowString
    renderRef (FlowList datatype) = sformat ("Array<" % stext % ">") <$> renderRef datatype
    renderRef (FlowTuple2 x y) =
        sformat ("tuple [" % stext % ", " % stext % "]") <$> renderRef x <*> renderRef y
    renderRef (FlowMaybe datatype) =
        sformat ("?" % stext) <$> renderRef datatype
    renderRef (FlowDict k v) =
        sformat ("{[" % stext % "]: (" % stext % ")}") <$> renderRef k <*> renderRef v
    renderRef FlowDate = pure "Date"
    renderRef FlowBool = pure "boolean"
    renderRef FlowString = pure "string"
    renderRef FlowUnit = pure "void"
    renderRef FlowNumber = pure "number"

toFlowTypeRefWith :: FlowType a => Options -> a -> Text
toFlowTypeRefWith options x = runReader (renderRef (toFlowType x)) options

toFlowTypeRef :: FlowType a => a -> Text
toFlowTypeRef = toFlowTypeRefWith defaultOptions

toFlowTypeSourceWith :: FlowType a => Options -> a -> Text
toFlowTypeSourceWith options x = runReader (render (toFlowType x)) options

toFlowTypeSource :: FlowType a => a -> Text
toFlowTypeSource = toFlowTypeSourceWith defaultOptions
