module FavoritePlacesType where

import Data.Map (Map)
import Data.List (List)
import PositionType (Position)

newtype FavoritePlaces = FavoritePlaces
  { positionsByUser :: Map (String) (List Position)
  }
