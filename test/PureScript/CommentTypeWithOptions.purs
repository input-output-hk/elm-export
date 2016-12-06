module CommentTypeWithOptions where

import Data.Date (Date)
import Data.Map (Map)
import Data.Tuple (Tuple)

data Comment = Comment
  { commentPostId :: Int
  , commentText :: String
  , commentMainCategories :: Tuple String String
  , commentPublished :: Boolean
  , commentCreated :: Date
  , commentTags :: Map (String) (Int)
  }
