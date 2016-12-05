{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module TypesSpec where

import           GHC.Generics
import           Test.Hspec   as Hspec
import           XPort

-- All the types in this file should be Elm-encodable.
data Person = Person
    { personName :: String
    } deriving (Generic, ElmType)

spec :: Hspec.Spec
spec = return ()
