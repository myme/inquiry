-- | Application commands/event handlers
module Inquiry.Commands
  ( continue,
    quit,
    exMode,
    insertMode,
    normalMode,
    cycleMethod,
    nextHistoryItem,
    nextHistoryItem',
    prevHistoryItem,
    prevHistoryItem',
    request,
    toggleRecents,
  )
where

import qualified Brick.Main as M
import Brick.Types (EventM, Next)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Inquiry.Http (http)
import Inquiry.Input (getInput, setInput)
import Inquiry.Request (Request (..), nextMethod, reqMethod, reqUrl)
import Inquiry.Types (AppState, EditMode (..), currentMethod, defaultUrl, mode, requestHistory, showRecents, urlInput)
import qualified Inquiry.Zipper as Z
import Lens.Micro.Platform (over, set, view, (&), (<&>), (^.), _1)
import Prelude hiding (putStrLn)

-- | Continue execution, updating the state if necessary.
continue :: s -> EventM n (Next s)
continue = M.continue

-- | Quit/halt the application.
quit :: s -> EventM n (Next s)
quit = M.halt

-- | Enter Ex (Command) mode.
exMode :: AppState -> EventM n (Next AppState)
exMode = M.continue . set mode Ex

-- | Enter Insert mode.
insertMode :: AppState -> EventM n (Next AppState)
insertMode = M.continue . set mode Insert

-- | Enter Normal mode.
normalMode :: AppState -> EventM n (Next AppState)
normalMode = M.continue . set mode Normal

-- | Cycle through list of methods.
cycleMethod :: AppState -> EventM n (Next AppState)
cycleMethod state = M.continue $ state & over currentMethod nextMethod

prevHistoryItem' :: AppState -> AppState
prevHistoryItem' state =
  let reqs = Z.prev $ view requestHistory state
      current = Z.peek reqs
   in state
        & over urlInput (setInput $ fromMaybe (state ^. defaultUrl) (current <&> view (_1 . reqUrl)))
          . over currentMethod (\m -> maybe m (view (_1 . reqMethod)) current)
          . set requestHistory reqs

-- | Update the navigator with the previous request history item.
prevHistoryItem :: AppState -> EventM n (Next AppState)
prevHistoryItem = M.continue . prevHistoryItem'

-- | Update the navigator with the next request history item.
nextHistoryItem' :: AppState -> AppState
nextHistoryItem' state =
  let reqs = Z.next $ view requestHistory state
      current = Z.peek reqs
   in state
        & over urlInput (setInput $ fromMaybe (state ^. defaultUrl) (current <&> view (_1 . reqUrl)))
          . over currentMethod (\m -> maybe m (view (_1 . reqMethod)) current)
          . set requestHistory reqs

nextHistoryItem :: AppState -> EventM n (Next AppState)
nextHistoryItem = M.continue . nextHistoryItem'

-- | Invoke a request to the current navigator item.
request :: AppState -> EventM n (Next AppState)
request state = do
  let req = Request (view currentMethod state) (getInput $ view urlInput state)
  response <- liftIO $ http req
  M.continue $
    state
      & set mode Normal
        . over urlInput (setInput $ state ^. defaultUrl)
        . over requestHistory (Z.prev . Z.end . Z.append (req, Just response))

toggleRecents :: AppState -> EventM n (Next AppState)
toggleRecents = M.continue . over showRecents not
