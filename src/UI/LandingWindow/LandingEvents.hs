
module UI.LandingWindow.LandingEvents where 

import UI.LandingWindow.LandingBuilder

import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Types as T
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

landingPageEvent :: T.BrickEvent LPName e -> T.EventM LPName LandingState ()
landingPageEvent e@(T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> do
            exit .= True
            M.halt
        V.EvKey (V.KChar 'q') [V.MCtrl] -> do 
            exit .= True
            M.halt
        V.EvKey V.KEnter [] -> M.halt
        V.EvKey (V.KChar '\t') [] -> focusRing %= F.focusNext
        V.EvKey V.KBackTab [] -> focusRing %= F.focusPrev
        _ -> do
            r <- use focusRing
            case F.focusGetCurrent r of
                Just Inputfield -> zoom inputfield $ E.handleEditorEvent e
                -- Just DialogBox -> zoom dialogfield $ D.handleDialogEvent ev
                Just TutorialButton -> return()
                Nothing -> return ()
landingPageEvent _ = return ()

{-
handleEnter :: Maybe LPName -> IO ()
handleEnter (Just EditorButton) = do
    s <- M.defaultMain EP.theApp EP.initialState
    return ()
handleEnter (Just TutorialButton) = return ()
handleEnter (Just HelpButton) = return ()
handleEnter _ = return ()

-}