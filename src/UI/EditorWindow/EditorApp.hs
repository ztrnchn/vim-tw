module UI.EditorWindow.EditorApp where

import UI.EditorWindow.EditorBuilder
import UI.EditorWindow.EditorEvents

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import Control.Monad.IO.Class -- needed for liftIO

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

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

editorPage :: FilePath -> IO ()
editorPage fp = do
    initial <- initialState fp
    st <- M.defaultMain theApp initial
    return ()
