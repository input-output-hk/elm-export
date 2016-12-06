{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module XPort.PureScript.Type where

import           Data.Int     (Int16, Int32, Int64, Int8)
import           Data.Map
import           Data.Proxy
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Prelude

data PSDatatype
    = PSDatatype Text
                  PSConstructor
    | PSPrimitive PSPrimitive
     deriving (Show, Eq)

data PSPrimitive
    = PSInt
    | PSBool
    | PSChar
    | PSDate
    | PSFloat
    | PSString
    | PSUnit
    | PSList PSDatatype
    | PSMaybe PSDatatype
    | PSTuple2 PSDatatype
              PSDatatype
    | PSDict PSPrimitive
            PSDatatype
     deriving (Show, Eq)


data PSConstructor
    = PSNamedConstructor Text PSValue
    | PSRecordConstructor Text PSValue
    | PSMultipleConstructors [PSConstructor]
     deriving (Show, Eq)

data PSValue
    = PSRef Text
    | PSEmpty
    | PSPrimitiveRef PSPrimitive
    | PSValues PSValue PSValue
    | PSField Text PSValue
     deriving (Show, Eq)

------------------------------------------------------------

class PSType a  where
    toPSType :: a -> PSDatatype
    toPSType = genericToPSDatatype . from
    default toPSType :: (Generic a, GenericPSDatatype (Rep a)) => a -> PSDatatype

------------------------------------------------------------

class GenericPSDatatype f  where
    genericToPSDatatype :: f a -> PSDatatype

instance (Datatype d, GenericPSConstructor f) =>
         GenericPSDatatype (D1 d f) where
    genericToPSDatatype datatype =
        PSDatatype
            (pack (datatypeName datatype))
            (genericToPSConstructor (unM1 datatype))

-- ------------------------------------------------------------
class GenericPSConstructor f  where
    genericToPSConstructor :: f a -> PSConstructor

instance (Constructor c, GenericPSValue f) =>
         GenericPSConstructor (C1 c f) where
    genericToPSConstructor constructor =
        if conIsRecord constructor
            then PSRecordConstructor name (genericToPSValue (unM1 constructor))
            else PSNamedConstructor name (genericToPSValue (unM1 constructor))
      where
        name = pack $ conName constructor

instance (GenericPSConstructor f, GenericPSConstructor g) =>
         GenericPSConstructor (f :+: g) where
    genericToPSConstructor _ =
        PSMultipleConstructors
            [ genericToPSConstructor (undefined :: f p)
            , genericToPSConstructor (undefined :: g p)]

------------------------------------------------------------

class GenericPSValue f  where
    genericToPSValue :: f a -> PSValue

instance (Selector s, GenericPSValue a) =>
         GenericPSValue (S1 s a) where
    genericToPSValue selector =
        case selName selector of
            "" -> genericToPSValue (undefined :: a p)
            name -> PSField (pack name) (genericToPSValue (undefined :: a p))

instance (GenericPSValue f, GenericPSValue g) =>
         GenericPSValue (f :*: g) where
    genericToPSValue _ =
        PSValues
            (genericToPSValue (undefined :: f p))
            (genericToPSValue (undefined :: g p))

instance GenericPSValue U1 where
    genericToPSValue _ = PSEmpty

instance PSType a =>
         GenericPSValue (Rec0 a) where
    genericToPSValue _ =
        case toPSType (undefined :: a) of
            PSPrimitive primitive -> PSPrimitiveRef primitive
            PSDatatype name _ -> PSRef name

instance PSType a => PSType [a] where
    toPSType _ = PSPrimitive (PSList (toPSType (undefined :: a)))

instance PSType a => PSType (Maybe a) where
    toPSType _ = PSPrimitive (PSMaybe (toPSType (undefined :: a)))

instance PSType () where
    toPSType _ = PSPrimitive PSUnit

instance PSType Text where
    toPSType _ = PSPrimitive PSString

instance PSType Day where
    toPSType _ = PSPrimitive PSDate

instance PSType UTCTime where
    toPSType _ = PSPrimitive PSDate

instance PSType Float where
    toPSType _ = PSPrimitive PSFloat

instance PSType Double where
    toPSType _ = PSPrimitive PSFloat

instance PSType Int8 where
    toPSType _ = PSPrimitive PSInt

instance PSType Int16 where
    toPSType _ = PSPrimitive PSInt

instance PSType Int32 where
    toPSType _ = PSPrimitive PSInt

instance PSType Int64 where
    toPSType _ = PSPrimitive PSInt

instance (PSType a, PSType b) =>
         PSType (a, b) where
    toPSType _ =
        PSPrimitive $
        PSTuple2 (toPSType (undefined :: a)) (toPSType (undefined :: b))


instance (PSType a) =>
         PSType (Proxy a) where
    toPSType _ = toPSType (undefined :: a)

instance (HasPSComparable k, PSType v) =>
         PSType (Map k v) where
    toPSType _ =
        PSPrimitive $
        PSDict (toPSComparable (undefined :: k)) (toPSType (undefined :: v))

class HasPSComparable a where
  toPSComparable :: a -> PSPrimitive

instance HasPSComparable String where
  toPSComparable _ = PSString

instance PSType Int where
  toPSType _ = PSPrimitive PSInt

instance PSType Char where
  toPSType _ = PSPrimitive PSChar

instance PSType Bool where
  toPSType _ = PSPrimitive PSBool
