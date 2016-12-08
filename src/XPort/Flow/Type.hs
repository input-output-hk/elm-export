{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module XPort.Flow.Type where

import           Data.Int     (Int16, Int32, Int64, Int8)
import           Data.Map
import           Data.Proxy
import           Data.Text
import           Data.Time
import           GHC.Generics ((:*:), (:+:), C1, Constructor, D1, Datatype,
                               Generic, Rec0, Rep, S1, Selector, U1,
                               conIsRecord, conName, datatypeName, from,
                               selName, unM1)
import           Prelude

data FlowDatatype
    = FlowDatatype Text FlowConstructor
    | FlowPrimitive FlowPrimitive
     deriving (Show, Eq)

data FlowPrimitive
    = FlowBool
    | FlowChar
    | FlowString
    | FlowDate
    | FlowNumber
    | FlowUnit
    | FlowList FlowDatatype
    | FlowMaybe FlowDatatype
    | FlowTuple2 FlowDatatype FlowDatatype
    | FlowDict FlowPrimitive FlowDatatype
     deriving (Show, Eq)


data FlowConstructor
    = FlowNamedConstructor Text FlowValue
    | FlowRecordConstructor Text FlowValue
    | FlowMultipleConstructors [FlowConstructor]
     deriving (Show, Eq)

data FlowValue
    = FlowRef Text
    | FlowEmpty
    | FlowPrimitiveRef FlowPrimitive
    | FlowValues FlowValue FlowValue
    | FlowField Text FlowValue
     deriving (Show, Eq)

------------------------------------------------------------

class FlowType a where
    toFlowType :: a -> FlowDatatype
    toFlowType = genericToFlowDatatype . from
    default toFlowType :: (Generic a, GenericFlowDatatype (Rep a)) => a -> FlowDatatype

------------------------------------------------------------

class GenericFlowDatatype f  where
    genericToFlowDatatype :: f a -> FlowDatatype

instance (Datatype d, GenericFlowConstructor f) =>
         GenericFlowDatatype (D1 d f) where
    genericToFlowDatatype datatype =
        FlowDatatype
            (pack (datatypeName datatype))
            (genericToFlowConstructor (unM1 datatype))

-- ------------------------------------------------------------
class GenericFlowConstructor f  where
    genericToFlowConstructor :: f a -> FlowConstructor

instance (Constructor c, GenericFlowValue f) =>
         GenericFlowConstructor (C1 c f) where
    genericToFlowConstructor constructor =
        if conIsRecord constructor
            then FlowRecordConstructor name (genericToFlowValue (unM1 constructor))
            else FlowNamedConstructor name (genericToFlowValue (unM1 constructor))
      where
        name = pack $ conName constructor

instance (GenericFlowConstructor f, GenericFlowConstructor g) =>
         GenericFlowConstructor (f :+: g) where
    genericToFlowConstructor _ =
        FlowMultipleConstructors
            [ genericToFlowConstructor (undefined :: f p)
            , genericToFlowConstructor (undefined :: g p)]

------------------------------------------------------------

class GenericFlowValue f  where
    genericToFlowValue :: f a -> FlowValue

instance (Selector s, GenericFlowValue a) =>
         GenericFlowValue (S1 s a) where
    genericToFlowValue selector =
        case selName selector of
            "" -> genericToFlowValue (undefined :: a p)
            name -> FlowField (pack name) (genericToFlowValue (undefined :: a p))

instance (GenericFlowValue f, GenericFlowValue g) =>
         GenericFlowValue (f :*: g) where
    genericToFlowValue _ =
        FlowValues
            (genericToFlowValue (undefined :: f p))
            (genericToFlowValue (undefined :: g p))

instance GenericFlowValue U1 where
    genericToFlowValue _ = FlowEmpty

instance FlowType a =>
         GenericFlowValue (Rec0 a) where
    genericToFlowValue _ =
        case toFlowType (undefined :: a) of
            FlowPrimitive primitive -> FlowPrimitiveRef primitive
            FlowDatatype name _ -> FlowRef name

instance FlowType a => FlowType [a] where
    toFlowType _ = FlowPrimitive (FlowList (toFlowType (undefined :: a)))

instance FlowType a => FlowType (Maybe a) where
    toFlowType _ = FlowPrimitive (FlowMaybe (toFlowType (undefined :: a)))

instance FlowType () where
    toFlowType _ = FlowPrimitive FlowUnit

instance FlowType Text where
    toFlowType _ = FlowPrimitive FlowString

instance FlowType Day where
    toFlowType _ = FlowPrimitive FlowDate

instance FlowType UTCTime where
    toFlowType _ = FlowPrimitive FlowDate

instance FlowType Float where
    toFlowType _ = FlowPrimitive FlowNumber

instance FlowType Double where
    toFlowType _ = FlowPrimitive FlowNumber

instance FlowType Int8 where
    toFlowType _ = FlowPrimitive FlowNumber

instance FlowType Int16 where
    toFlowType _ = FlowPrimitive FlowNumber

instance FlowType Int32 where
    toFlowType _ = FlowPrimitive FlowNumber

instance FlowType Int64 where
    toFlowType _ = FlowPrimitive FlowNumber

instance (FlowType a, FlowType b) =>
         FlowType (a, b) where
    toFlowType _ =
        FlowPrimitive $
        FlowTuple2 (toFlowType (undefined :: a)) (toFlowType (undefined :: b))


instance (FlowType a) =>
         FlowType (Proxy a) where
    toFlowType _ = toFlowType (undefined :: a)

instance (HasFlowComparable k, FlowType v) =>
         FlowType (Map k v) where
    toFlowType _ =
        FlowPrimitive $ FlowDict (toFlowComparable (undefined :: k)) (toFlowType (undefined :: v))

class HasFlowComparable a where
  toFlowComparable :: a -> FlowPrimitive

instance HasFlowComparable String where
  toFlowComparable _ = FlowString

instance FlowType Int where
  toFlowType _ = FlowPrimitive FlowNumber

instance FlowType Char where
  toFlowType _ = FlowPrimitive FlowChar

instance FlowType Bool where
  toFlowType _ = FlowPrimitive FlowBool
