module PostTypeWithOptions where

import CommentType (Comment)
import Data.Maybe (Maybe)
import Data.List (List)

data Post = Post
  { postId :: Int
  , postName :: String
  , postAge :: Maybe Number
  , postComments :: List Comment
  , postPromoted :: Maybe Comment
  , postAuthor :: Maybe String
  }
