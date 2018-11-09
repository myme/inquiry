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
  ) where

import qualified Brick.Main as M
import           Brick.Types (EventM, Next)
import           Data.Maybe (fromMaybe)
import           Data.Text (unpack)
import           Inquiry.Input (getInput, setInput)
import           Inquiry.Types (method, nextMethod, currentMethod, AppState, EditMode(..), Request(..), requestHistory, urlInput, url, mode)
import qualified Inquiry.Zipper as Z
import           Lens.Micro.Platform ((<&>), (&), over, view, set)
import           System.IO (hGetContents)
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

curl :: Request -> IO ()
curl req = do
  let name = "curl"
      args = [unpack $ view url req]
      cmd = (proc name args){ std_out = CreatePipe }
  putStrLn $ "Running command: `" <> unwords (name : args) <> "`\n"
  withCreateProcess cmd $ \_ (Just stdout) _ _ -> do
    output <- hGetContents stdout
    putStrLn output

-- | Invoke a request to the current navigator item.
request :: AppState -> EventM n (Next AppState)
request state = M.suspendAndResume $ do
  let req = Request (view currentMethod state) (getInput $ view urlInput state)
  curl req
  let state' = state &
        set mode Normal .
        over urlInput (setInput "http://") .
        over requestHistory (Z.end . Z.append req)
  putStrLn "Press Return to return..."
  _ <- getLine
  return state'
