{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module UI.LandingWindow.LandingBuilder where 

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( padAll
  , str
  , (<=>)
  , (<+>)
  , vBox
  , hBox
  , withAttr
  , Padding (..)
  , hLimit
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.Border
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T
import qualified Brick.Focus as F

import Control.Monad.IO.Class -- needed for liftIO
import System.IO

import FileHandler.FileLoader
import UI.UIColors


data LPName =
    TutorialButton
    | Inputfield
    deriving (Show, Eq, Ord)

data LandingState =
    LandingState { _focusRing :: F.FocusRing LPName
        , _inputfield :: E.Editor String LPName
        , _exit :: Bool
       }

makeLenses ''LandingState

getfocusAttr :: Bool -> A.AttrName
getfocusAttr f = 
  if  f 
    then A.attrName "focusedButton"
    else A.attrName "nonfocusedButton"

makeButton :: [Char] -> Bool -> Widget LPName
makeButton n f = padAll 1 $ withAttr (getfocusAttr f ) $ border $ padAll 1 $ str n 

makeLandingState :: FilePath -> LandingState 
makeLandingState fp = 
  LandingState  (F.focusRing [TutorialButton, Inputfield ])
                (E.editor Inputfield (Just 1) (fp ++ "/examplefile.txt"))
                False


drawLandingPageUI :: LandingState -> [Widget LPName]
drawLandingPageUI st = [ui]
  where
    fr = st ^. focusRing
    f = F.focusGetCurrent fr
    ed = st^. inputfield
    content = unlines $ E.getEditContents ed
    width = length content +1
    e = E.renderEditor (str . unlines) (f == Just Inputfield) (st ^. inputfield)
    buttons = makeButton "  Tutorial  " (f == Just TutorialButton)
    ui = 
      C.hCenter (str "  vim with training wheels\no⸂                        ⸃o")
        <=> border (withAttr (A.attrName "Welcome")(
          C.hCenter 
           (padAll 2 (str "        Welcome to\n" 
           <=> 
           str "  vim with training wheels\n"
           <=>  
           str "o⸂                        ⸃o" ))
          )
          <=>
          withAttr (A.attrName "nonfocusedButton")(
          C.hCenter 
           (padAll 2 (str "use tab to choose between the tutorial or a file to open in the editor" )))
          <=>
          C.hCenter  (padAll 2 buttons)
          <=> 
          C.hCenter ( hLimit (width +2) (border (withAttr (getfocusAttr (f == Just Inputfield)) (C.hCenter (str "Start Editor") <=> e))))
          <=> 
          C.hCenter ( padAll 1 (withAttr (A.attrName "helptext") (str "ctrl-q: to quit    tab: change selection   enter: choose selection\nfilepath is editable when it is selected" )))
        )


landingPageMap :: A.AttrMap
landingPageMap = A.attrMap V.defAttr
    [ 
     (E.editAttr,             cBrightGreen `on` cBlack)
    , (E.editFocusedAttr,     cBlack `on` cYellow)
    
    , (A.attrName "nonfocusedButton",     cBrightGreen `on` cBlack)
    , (A.attrName "focusedButton",     cBlack `on` cYellow)
    , (A.attrName "Welcome",     cYellow `on` cBlack)
    , (A.attrName "helptext",    bg cDarkGreen)
    ]



landingAppCursor :: LandingState -> [T.CursorLocation LPName] -> Maybe (T.CursorLocation LPName )
landingAppCursor = F.focusRingCursor (^.focusRing)



