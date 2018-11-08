{-# LANGUAGE OverloadedStrings #-}

module Inquiry.Commands
  ( continue
  , quit
  , exMode
  , insertMode
  , normalMode
  , request
  ) where

import qualified Brick.Main as M
import           Brick.Types (EventM, Next)
import qualified Brick.Widgets.Edit as E
import           Data.Text (unpack)
import           Data.Text.Zipper (clearZipper)
import           Inquiry.Types (Request, recentReqs, urlInput, url, currentRequest, AppState, EditMode(..), mode)
import           Lens.Micro.Platform (over, view, set)
import           System.IO (hGetContents)
import           System.Process (StdStream(..), std_out, withCreateProcess, proc)

continue :: s -> EventM n (Next s)
continue = M.continue

quit :: s -> EventM n (Next s)
quit = M.halt

exMode :: AppState -> EventM n (Next AppState)
exMode = M.continue . set mode Ex

insertMode :: AppState -> EventM n (Next AppState)
insertMode = M.continue . set mode Insert

normalMode :: AppState -> EventM n (Next AppState)
normalMode = M.continue . set mode Normal

curl :: Request -> IO ()
curl req = do
  let cmd = (proc "curl" [unpack $ view url req]){ std_out = CreatePipe }
  withCreateProcess cmd $ \_ (Just stdout) _ _ -> do
    output <- hGetContents stdout
    putStrLn output

request :: AppState -> EventM n (Next AppState)
request state = M.suspendAndResume $ do
  let req = view currentRequest state
  putStrLn $ "Running request: " <> show req
  curl req
  putStrLn "Press Return to return..."
  _ <- getLine
  return $
    set (currentRequest . url) "" $
    over urlInput (E.applyEdit clearZipper) $
    over recentReqs (<> [req]) state
