
module UI.LandingWindow.LandingApp where 

import UI.LandingWindow.LandingBuilder
import UI.LandingWindow.LandingEvents
import FileHandler.FileLoader
import Algebra.HelperFunctions

import qualified Brick.Main as M
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

import Control.Monad.IO.Class -- needed for liftIO

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

landingPageApp :: IO (M.App LandingState e LPName)
landingPageApp = do
    return M.App
        { M.appDraw = drawLandingPageUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = landingPageEvent
        , M.appStartEvent = return ()
        , M.appAttrMap = const landingPageMap
        }

landingPage :: IO (Maybe LPName, [Char], Bool)
landingPage = do
    dir <- getCurrentDir
    let drawUI = drawLandingPageUI
    app <- landingPageApp
    st <- M.defaultMain app (makeLandingState dir)
    let 
        fr = st ^. focusRing
        f = F.focusGetCurrent fr
        [e] = safehead (E.getEditContents (st ^. inputfield))
        exitflag =  st^.exit
    return (f, e, exitflag )