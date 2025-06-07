{-# LANGUAGE OverloadedStrings #-}
module UI.MainUI where


import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

import qualified Brick.Widgets.Dialog as D


import UI.LandingWindow.LandingBuilder 
import UI.LandingWindow.LandingEvents 
import UI.LandingWindow.LandingApp
import UI.EditorWindow.EditorBuilder
import UI.EditorWindow.EditorEvents
import UI.EditorWindow.EditorApp

initialWindow :: IO ()
initialWindow = do
  -- chosenAction :: (LPName, [Char]])
    chosenAction <- landingPage
    case chosenAction of
      (_, _, True ) -> return ()
      (Just Inputfield, somePath, _ ) -> editorPage somePath
      (Just TutorialButton, somePath, _ ) -> editorPage "Tutorial"
      _ -> putStrLn $ "You chose: " <> show chosenAction
    putStrLn "Good Bye!"
   