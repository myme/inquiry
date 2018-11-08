-- | Application commands/event handlers

{-# LANGUAGE OverloadedStrings #-}

module Inquiry.Commands
  ( continue
  , quit
  , exMode
  , insertMode
  , normalMode
  , nextHistoryItem
  , prevHistoryItem
  , request
  ) where

import qualified Brick.Main as M
import           Brick.Types (EventM, Next)
import qualified Brick.Widgets.Edit as E
import           Data.Maybe (fromMaybe)
import           Data.Text (unpack)
import           Data.Text.Zipper (clearZipper)
import           Inquiry.Input (setInput)
import           Inquiry.Types (requestHistory, Request, urlInput, url, currentRequest, AppState, EditMode(..), mode)
import           Inquiry.Zipper (insertZipper, nextZipper, peekZipper, prevZipper)
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

-- | Update the navigator with the previous request history item.
prevHistoryItem :: AppState -> EventM n (Next AppState)
prevHistoryItem state = M.continue $ do
  let reqs = view requestHistory state
      reqs' = prevZipper reqs
      current = peekZipper reqs' <&> view url
  state &
    over urlInput (setInput $ fromMaybe "http://" current) .
    set requestHistory reqs'

-- | Update the navigator with the next request history item.
nextHistoryItem :: AppState -> EventM n (Next AppState)
nextHistoryItem state = M.continue $ do
  let reqs = view requestHistory state
      reqs' = nextZipper reqs
      current = peekZipper reqs' <&> view url
  state &
    over urlInput (setInput $ fromMaybe "http://" current) .
    set requestHistory reqs'

curl :: Request -> IO ()
curl req = do
  let cmd = (proc "curl" [unpack $ view url req]){ std_out = CreatePipe }
  withCreateProcess cmd $ \_ (Just stdout) _ _ -> do
    output <- hGetContents stdout
    putStrLn output

-- | Invoke a request to the current navigator item.
request :: AppState -> EventM n (Next AppState)
request state = M.suspendAndResume $ do
  let req = view currentRequest state
  putStrLn $ "Running request: " <> show req
  curl req
  putStrLn "Press Return to return..."
  _ <- getLine
  return $
    set (currentRequest . url) "" $
    set mode Normal $
    over urlInput (E.applyEdit clearZipper) $
    over requestHistory (insertZipper req) state
