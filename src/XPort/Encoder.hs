{-# LANGUAGE OverloadedStrings #-}
module XPort.Encoder
  ( HasEncoder(..)
  , HasEncoderRef(..)
  ) where

import           Control.Monad.Reader
import           Data.Text
import           XPort.Common

class HasEncoder a where
  render :: a -> Reader Options Text

class HasEncoderRef a where
  renderRef :: a -> Reader Options Text
