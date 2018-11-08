-- | Input fields

module Inquiry.Input
  ( input
  , getInput
  , setInput
  ) where

import qualified Brick.Widgets.Edit as E
import           Data.Text (Text)
import           Data.Text.Zipper (textZipper, gotoEOL)

input :: Text -> Text -> E.Editor Text Text
input name value = E.applyEdit gotoEOL editor
  where editor = E.editorText name (Just 1) value

getInput :: E.Editor Text Text -> Text
getInput = foldr (<>) mempty . E.getEditContents

setInput :: Text -> E.Editor Text Text -> E.Editor Text Text
setInput text = E.applyEdit $ gotoEOL . const (textZipper [text] (Just 1))
