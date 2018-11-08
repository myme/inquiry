-- | Input fields

module Inquiry.Input
  ( input
  ) where

import qualified Brick.Widgets.Edit as E
import           Data.Text (Text)
import           Data.Text.Zipper (gotoEOL)

input :: Text -> Text -> E.Editor Text Text
input name value = E.applyEdit gotoEOL editor
  where editor = E.editorText name (Just 1) value
