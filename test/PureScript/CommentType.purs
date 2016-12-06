module CommentType where

import Data.Date (Date)
import Data.Map (Map)
import Data.Tuple (Tuple)

data Comment = Comment
  { postId :: Int
  , text :: String
  , mainCategories :: Tuple String String
  , published :: Boolean
  , created :: Date
  , tags :: Map (String) (Int)
  }
