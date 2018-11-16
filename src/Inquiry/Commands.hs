-- | Application commands/event handlers

{-# LANGUAGE OverloadedStrings #-}

module Inquiry.Commands
  ( continue
  , quit
  , exMode
  , insertMode
  , normalMode
  , cycleMethod
  , nextHistoryItem
  , nextHistoryItem'
  , prevHistoryItem
  , prevHistoryItem'
  , request
  , toggleRecents
  ) where

import qualified Brick.Main as M
import           Brick.Types (EventM, Next)
import           Data.Maybe (fromMaybe)
import           Data.Text (pack, Text, unpack)
import           Data.Text.IO (putStrLn, hGetContents)
import           Inquiry.Input (getInput, setInput)
import           Inquiry.Request (Request(..), method, url, nextMethod)
import           Inquiry.Types (showRecents, response, currentMethod, AppState, EditMode(..), requestHistory, urlInput, mode)
import qualified Inquiry.Zipper as Z
import           Lens.Micro.Platform ((^.), (<&>), (&), over, view, set)
import           Prelude hiding (putStrLn)
import           System.Process (StdStream(..), std_out, withCreateProcess, proc)

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
  in state &
    over urlInput (setInput $ fromMaybe "http://" (current <&> view url)) .
    over currentMethod (\m -> maybe m (view method) current) .
    set requestHistory reqs

-- | Update the navigator with the previous request history item.
prevHistoryItem :: AppState -> EventM n (Next AppState)
prevHistoryItem = M.continue . prevHistoryItem'

-- | Update the navigator with the next request history item.
nextHistoryItem' :: AppState -> AppState
nextHistoryItem' state =
  let reqs = Z.next $ view requestHistory state
      current = Z.peek reqs
  in state &
    over urlInput (setInput $ fromMaybe "http://" (current <&> view url)) .
    over currentMethod (\m -> maybe m (view method) current) .
    set requestHistory reqs

nextHistoryItem :: AppState -> EventM n (Next AppState)
nextHistoryItem = M.continue . nextHistoryItem'

curl :: Request -> IO Text
curl req = do
  let name = "curl"
      args = ["-L", unpack $ req ^. url]
      cmd = (proc name args){ std_out = CreatePipe }
  putStrLn $ pack $ concat ["Running command: `", name, unwords args, "`\n"]
  withCreateProcess cmd $ \_ (Just stdout) _ _ -> do
    response' <- hGetContents stdout
    putStrLn response'
    return response'

-- | Invoke a request to the current navigator item.
request :: AppState -> EventM n (Next AppState)
request state = M.suspendAndResume $ do
  let req = Request (view currentMethod state) (getInput $ view urlInput state)
  response' <- curl req
  let state' = state &
        set response response' .
        set mode Normal .
        over urlInput (setInput "http://") .
        over requestHistory (Z.end . Z.append req)
  return state'

toggleRecents :: AppState -> EventM n (Next AppState)
toggleRecents = M.continue . over showRecents not
