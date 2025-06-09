module UI.EditorWindow.EditorEvents where

import UI.EditorWindow.EditorBuilder
import CommandlineHandler.HandleCommands
import Domain.DomainTypes



import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import Control.Monad.IO.Class -- needed for liftIO

import Control.Monad.State.Class (get, gets)
import Data.Tree
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  , padAll
  , fill
  , padBottom
  , Padding (Max)
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

getEditorWindowSize :: T.EventM Name St ()
getEditorWindowSize = do
  mvp <- M.lookupViewport LinesVP
  case mvp of
    Just vp -> do
      let (_, h) = vp ^. T.vpSize
      editorWindowSize .= h
    Nothing -> return ()
 


updateScrollText :: T.EventM Name St ()
updateScrollText = do
    st <- get
    let
        maxheight = st ^. editorWindowSize
        oldstartline = st^.scrolledLine
        curline = length (lines (st^.beforeEdit ++ st^.currentEdit))
        newstartline 
            | oldstartline < curline && curline < (oldstartline + maxheight) =  oldstartline
            | curline >= (oldstartline + maxheight) = oldstartline + (curline-(oldstartline + maxheight-1))
            | otherwise = curline
    scrolledLine .= newstartline
        

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    handleEsc
appEvent (T.VtyEvent (V.EvKey V.KDel [])) =
    handleDel
appEvent (T.VtyEvent (V.EvKey V.KBS [])) =
    handleBackspace
appEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) =
    M.halt
appEvent (T.VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) =
    handleRedo
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
    handleEnter 

-- TODO: Clear the command buffer
-- Example that currently doesn't work:
-- 5 <C-u> j will go down 5 lines after <C-u> instead of just one.
appEvent (T.VtyEvent (V.EvKey (V.KChar 'u') [V.MCtrl])) = do
    handlePageUp
    updateScrollText
appEvent (T.VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = do
    handlePageDown
    updateScrollText

{- appEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [])) =
    --liftIO $ LP.landingPage
appEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
    focusRing %= F.focusNext
appEvent (T.VtyEvent (V.EvKey V.KBackTab [])) =
    focusRing %= F.focusPrev
-}
appEvent ev = do
    zoom commandEdit $ E.handleEditorEvent ev
    handleCommands 
    getEditorWindowSize
    updateScrollText




