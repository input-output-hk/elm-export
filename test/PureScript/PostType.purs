module PostType where

import CommentType
import Data.Maybe
import Data.List

type Post =
  { id :: Int
  , name :: String
  , age :: Maybe Float
  , comments :: List Comment
  , promoted :: Maybe Comment
  , author :: Maybe String
  }
