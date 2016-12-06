module CommentType where

import Data.Date (Date)
import Data.Map (Map)

type Comment = {
  postId :: Int
  , text :: String
  , mainCategories :: (String, String)
  , published :: Bool
  , created :: Date
  , tags :: Map String Int
}
