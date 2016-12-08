{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Flow.ExportSpec where

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
import           XPort        (FlowType, Options, defaultOptions,
                               fieldLabelModifier, toFlowTypeRef,
                               toFlowTypeSourceWith)

data Post = Post
    { id       :: Int
    , name     :: String
    , age      :: Maybe Double
    , comments :: [Comment]
    , promoted :: Maybe Comment
    , author   :: Maybe String
    } deriving (Generic, FlowType)

data Comment = Comment
    { postId         :: Int
    , text           :: Text
    , mainCategories :: (String, String)
    , published      :: Bool
    , created        :: UTCTime
    , tags           :: Map String Int
    } deriving (Generic, FlowType)

data Position
  = Beginning
  | Middle
  | End
  deriving (Generic, FlowType)

data Timing
  = Start
  | Continue Double
  | Stop
  deriving (Generic, FlowType)

spec :: Hspec.Spec
spec = toFlowTypeSpec

toFlowTypeSpec :: Hspec.Spec
toFlowTypeSpec =
  describe "Convert to Flow types." $
  do  it "toFlowTypeSource Post" $
        shouldMatchTypeSource
          (unlines ["/* @flow */"
            , ""
            , "import Comment from 'Comment';"
            , ""
            , "%s"])
         defaultOptions
         (Proxy :: Proxy Post)
         "test/Flow/Post.js"
      it "toFlowTypeSource Comment" $
        shouldMatchTypeSource
          (unlines ["/* @flow */"
            , ""
            , "%s"])
         defaultOptions
         (Proxy :: Proxy Comment)
         "test/Flow/Comment.js"
      it "toFlowTypeSource Position" $
        shouldMatchTypeSource
          (unlines ["/* @flow */"
                   ,""
                   ,"%s"])
          defaultOptions
          (Proxy :: Proxy Position)
          "test/Flow/Position.js"
      it "toFlowTypeSource Timing" $
        shouldMatchTypeSource
          (unlines ["/* @flow */"
                   ,""
                   ,"%s"])
          defaultOptions
          (Proxy :: Proxy Timing)
          "test/Flow/Timing.js"

shouldMatchTypeSource :: FlowType a => String -> Options -> a -> FilePath -> IO ()
shouldMatchTypeSource wrapping options x =
  shouldMatchFile . printf wrapping $ toFlowTypeSourceWith options x

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
