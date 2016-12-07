module PostType where

import CommentType (Comment)
import Data.Maybe (Maybe)
import Data.List (List)

data Post = Post
  { id :: Int
  , name :: String
  , age :: Maybe Number
  , comments :: List (Comment)
  , promoted :: Maybe Comment
  , author :: Maybe String
  }
