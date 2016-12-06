{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module PureScript.ExportSpec where

import           Data.Char
import           Data.Int
import           Data.Map
import           Data.Monoid
import           Data.Proxy
import           Data.Text    hiding (unlines)
import           Data.Time
import           GHC.Generics
import           Test.Hspec   hiding (Spec)
import           Test.Hspec   as Hspec
import           Text.Printf
import           XPort        (Options, PSType, defaultOptions,
                               fieldLabelModifier, toPSTypeRef,
                               toPSTypeSourceWith)

-- Debugging hint:
-- ghci> import GHC.Generics
-- ghci> :kind! Rep Post
-- ...

data Post = Post
    { id       :: Int
    , name     :: String
    , age      :: Maybe Double
    , comments :: [Comment]
    , promoted :: Maybe Comment
    , author   :: Maybe String
    } deriving (Generic, PSType)

data Comment = Comment
    { postId         :: Int
    , text           :: Text
    , mainCategories :: (String, String)
    , published      :: Bool
    , created        :: UTCTime
    , tags           :: Map String Int
    } deriving (Generic, PSType)

data Position
  = Beginning
  | Middle
  | End
  deriving (Generic, PSType)

data Timing
  = Start
  | Continue Double
  | Stop
  deriving (Generic, PSType)

newtype Useless =
    Useless ()
     deriving (Generic, PSType)

newtype FavoritePlaces =
  FavoritePlaces {positionsByUser :: Map String [Position]}
  deriving (Generic, PSType)

-- | We don't actually use this type, we just need to see that it compiles.
data LotsOfInts = LotsOfInts
    { intA :: Int8
    , intB :: Int16
    , intC :: Int32
    , intD :: Int64
    } deriving (Generic, PSType)

spec :: Hspec.Spec
spec =
  do toPSTypeSpec
    --  toPSDecoderSpec
    --  toPSEncoderSpec

toPSTypeSpec :: Hspec.Spec
toPSTypeSpec =
  describe "Convert to PureScript types." $
  do it "toPSTypeSource Post" $
       shouldMatchTypeSource
         (unlines ["module PostType where"
                  ,""
                  ,"import CommentType"
                  ,""
                  ,"import Data.Maybe"
                  ,""
                  ,"import Data.List"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Post)
         "test/PureScript/PostType.purs"
    --  it "toPSTypeSource Comment" $
    --    shouldMatchTypeSource
    --      (unlines ["module CommentType where"
    --               ,""
    --               ,"import Date exposing (Date)"
    --               ,"import Dict exposing (Dict)"
    --               ,""
    --               ,""
    --               ,"%s"])
    --      defaultOptions
    --      (Proxy :: Proxy Comment)
    --      "test/PureScript/CommentType.purs"
    --  it "toPSTypeSource Position" $
    --    shouldMatchTypeSource
    --      (unlines ["module PositionType where","","","%s"])
    --      defaultOptions
    --      (Proxy :: Proxy Position)
    --      "test/PureScript/PositionType.purs"
    --  it "toPSTypeSource Timing" $
    --    shouldMatchTypeSource
    --      (unlines ["module TimingType where","","","%s"])
    --      defaultOptions
    --      (Proxy :: Proxy Timing)
    --      "test/PureScript/TimingType.purs"
    --  it "toPSTypeSource Useless" $
    --    shouldMatchTypeSource
    --      (unlines ["module UselessType where","","","%s"])
    --      defaultOptions
    --      (Proxy :: Proxy Useless)
    --      "test/PureScript/UselessType.purs"
    --  it "toPSTypeSource FavoritePlaces" $
    --    shouldMatchTypeSource
    --      (unlines ["module FavoritePlacesType where"
    --               ,""
    --               ,"import PositionType where"
    --               ,""
    --               ,""
    --               ,"%s"])
    --      defaultOptions
    --      (Proxy :: Proxy FavoritePlaces)
    --      "test/PureScript/FavoritePlacesType.purs"
    --  it "toPSTypeSourceWithOptions Post" $
    --    shouldMatchTypeSource
    --      (unlines ["module PostTypeWithOptions where"
    --               ,""
    --               ,"import CommentType where"
    --               ,""
    --               ,""
    --               ,"%s"])
    --      (defaultOptions {fieldLabelModifier = withPrefix "post"})
    --      (Proxy :: Proxy Post)
    --      "test/PureScript/PostTypeWithOptions.purs"
    --  it "toPSTypeSourceWithOptions Comment" $
    --    shouldMatchTypeSource
    --      (unlines ["module CommentTypeWithOptions where"
    --               ,""
    --               ,"import Date exposing (Date)"
    --               ,"import Dict exposing (Dict)"
    --               ,""
    --               ,""
    --               ,"%s"])
    --      (defaultOptions {fieldLabelModifier = withPrefix "comment"})
    --      (Proxy :: Proxy Comment)
    --      "test/PureScript/CommentTypeWithOptions.purs"
    --  describe "Convert to PureScript type references." $
    --    do it "toPSTypeRef Post" $
    --         toPSTypeRef (Proxy :: Proxy Post)
    --         `shouldBe` "Post"
    --       it "toPSTypeRef [Comment]" $
    --         toPSTypeRef (Proxy :: Proxy [Comment])
    --         `shouldBe` "List (Comment)"
    --       it "toPSTypeRef String" $
    --         toPSTypeRef (Proxy :: Proxy String)
    --         `shouldBe` "String"
    --       it "toPSTypeRef (Maybe String)" $
    --         toPSTypeRef (Proxy :: Proxy (Maybe String))
    --         `shouldBe` "Maybe (String)"
    --       it "toPSTypeRef [Maybe String]" $
    --         toPSTypeRef (Proxy :: Proxy [Maybe String])
    --         `shouldBe` "List (Maybe (String))"
    --       it "toPSTypeRef (Map String (Maybe String))" $
    --         toPSTypeRef (Proxy :: Proxy (Map String (Maybe String)))
    --         `shouldBe` "Dict (String) (Maybe (String))"

shouldMatchTypeSource
  :: PSType a
  => String -> Options -> a -> FilePath -> IO ()
shouldMatchTypeSource wrapping options x =
  shouldMatchFile . printf wrapping $ toPSTypeSourceWith options x

shouldMatchFile :: String -> FilePath -> IO ()
shouldMatchFile actual fileExpected =
  do source <- readFile fileExpected
     actual `shouldBe` source

initCap :: Text -> Text
initCap t =
    case uncons t of
        Nothing -> t
        Just (c, cs) -> cons (Data.Char.toUpper c) cs

withPrefix :: Text -> Text -> Text
withPrefix prefix s = prefix <> initCap  s
