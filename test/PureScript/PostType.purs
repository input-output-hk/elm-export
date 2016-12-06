module PostType where

import CommentType (Comment)
import Data.Maybe (Maybe)
import Data.List (List)

type Post =
  { id :: Int
  , name :: String
  , age :: Maybe Number
  , comments :: List Comment
  , promoted :: Maybe Comment
  , author :: Maybe String
  }
