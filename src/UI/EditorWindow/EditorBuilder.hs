{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module UI.EditorWindow.EditorBuilder where

import FileHandler.FileLoader 
import Domain.DomainTypes
import Algebra.HelperFunctions
import UI.UIColors
import UI.EditorWindow.Helptexts

import System.IO
import Numeric.Natural

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import Control.Monad.IO.Class -- needed for liftIO

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Border (border)
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , vBox
  , hLimit
  , vLimit
  , str
  , strWrap
  , padAll
  , padLeft
  , fill
  , padBottom
  , padRight
  , Padding (..)
  , emptyWidget
  , withAttr
  , viewport
  , padLeftRight
  , withVScrollBars
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on, bg, fg)

import Numeric.Natural
import Data.Tree
import Data.Monoid ()         -- enables Monoid instance
import Data.Semigroup ((<>))  -- for Semigroup and (<>)


data Name = Before
          | Current
          | After
          | Editor
          | Command
          | LinesVP
          deriving (Ord, Show, Eq)


data St =
    St {_beforeEdit :: [Char]
       ,_currentEdit :: [Char]
       ,_afterEdit :: [Char]
       , _commandEdit :: E.Editor String Name
       , _editorMode :: EditorMode
       , _filepath :: FilePath
       , _editorClipboard :: [Char]
       , _help :: Bool
       , _editorWindowSize :: Int
       , _scrolledLine :: Int
       , _history :: HistoryTree
       , _historyPos :: Natural
       }

makeLenses ''St

initialHistory :: CurrentBuffer -> HistoryTree
initialHistory cb = Node cb []
-- E.editor:: n -> Maybe Int -> a
-- n = name of editor , Maybe Int = limit number of lines, a = the initial content.

initialState :: FilePath -> IO St
initialState fp = do
    (a,b,c) <- checkFilepath fp
    return $
        St a
            b
            c
            (E.editor Command (Just 4) "")
            NormalMode
            fp
            []
            True
            100
            1
            (initialHistory (a,b,c))
            0


theMap :: A.AttrMap
theMap = A.attrMap (cWhite `on` cDarkRed)
    [ 
     (E.editFocusedAttr,           cBlack `on` V.brightMagenta)
    , (A.attrName "cursorStyle",    cBlack `on` cWhite)
    , (A.attrName "normalMode",    cWhite `on` cBlack)
    , (A.attrName "visualMode",    cWhite `on` cDarkBlue)
    , (A.attrName "insertMode",    cBlack `on` cYellow)
    , (A.attrName "clON",    V.withStyle ( cDarkRed `on` cYellow) V.bold)
    , (A.attrName "clOFF",    cWhite `on` cBlack)
    , (A.attrName "wheelFG",    fg cWhite)
    , (A.attrName "wheelBG",    bg cDarkRed)
    , (A.attrName "helptext",    bg cDarkGreen)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = const . const Nothing

chooseModeAttr :: EditorMode -> A.AttrName
chooseModeAttr mode = case mode of
    NormalMode -> A.attrName "normalMode"
    VisualMode -> A.attrName "visualMode"
    InsertMode -> A.attrName "insertMode"
    CommandMode -> A.attrName "normalMode"
    
chooseModeAttrCl :: EditorMode -> A.AttrName
chooseModeAttrCl mode = case mode of
    NormalMode -> A.attrName "clON"
    VisualMode -> A.attrName "clON"
    InsertMode -> A.attrName "clOFF"
    CommandMode -> A.attrName "clON"

makeModeWidget :: EditorMode -> T.Widget Name
makeModeWidget mode = case mode of
    NormalMode -> str "-- NORMAL --"
    CommandMode -> str "-- COMMAND --"
    VisualMode -> str "-- VISUAL --"
    InsertMode -> str "-- INSERT --"

makeFilepathWidget :: FilePath -> T.Widget Name
makeFilepathWidget  fp = padRight Max $ str fp
        
makeCursorWidgets :: [Char] -> (T.Widget Name, T.Widget Name, T.Widget Name, Natural)
makeCursorWidgets cur = (begin,middle,end,fromIntegral len)
    where 
        widgs = map str (replaceEmptyWithNewline (lines cur))
        len = length widgs
        begin = withAttr (A.attrName "cursorStyle") (vBox (safehead widgs))
        middle = if len <3
            then emptyWidget
            else withAttr (A.attrName "cursorStyle") (vBox (safeinit (safetail widgs)))
        end = if len <2 
            then emptyWidget
            else withAttr (A.attrName "cursorStyle") (vBox (safelast widgs))

chooseHelptext :: EditorMode -> [Char]
chooseHelptext mode = 
    case mode of
        NormalMode -> normalHelpText
        CommandMode -> ""
        VisualMode -> visualHelpText
        InsertMode -> insertHelpText


makeHelpWidget :: Bool -> EditorMode-> T.Widget Name
makeHelpWidget help mode = 
    if help
        then padAll 1 (withAttr (A.attrName "helptext") (padAll 1(str (chooseHelptext mode) )))
        else emptyWidget




makeEditorWidget :: St -> T.Widget Name
makeEditorWidget st = editor
    where
        startline = st^.scrolledLine
        splitbefore = splitBefore (st^.beforeEdit) startline
        (a,b,c,d,e) = splitbuffer (splitbefore, st^.currentEdit, st^.afterEdit )
        -- withAttr (A.attrName "cursorStyle")
        (begin,middle,end,len) = makeCursorWidgets c
        selectedline 
            | len < 2 =
                safestr b  <+>   begin <+> str d 
            | len < 3 =
                (safestr b  <+>   begin)
                <=>
                (end <+> str d )
            | len > 2 =
                (safestr b  <+>   begin)
                <=>
                middle
                <=>
                (end <+> str d )
        text =  safestr a
                    <=>
                    padBottom (Pad 0) selectedline
                    <=>
                    safestr e
        editor = 
            (withAttr (A.attrName "wheelFG") (str "Editor              " )
            <=>
            makeHelpWidget (st^.help) (st^.editorMode)
            )
            <+> 
            renderWithLineNumbers st <+>  withAttr (chooseModeAttr (st^.editorMode))   (padRight Max text)


renderCustomEditor ::  EditorMode -> E.Editor String Name -> T.Widget Name
renderCustomEditor mode ed =
    withAttr (chooseModeAttrCl mode) (str (unlines $ E.getEditContents ed))

renderWithLineNumbers :: St -> T.Widget Name
renderWithLineNumbers st  =
    lineNumbersVp 
    where
        text = st^.beforeEdit ++ st^.currentEdit ++ st^.afterEdit
        startline = st^.scrolledLine
        h = length (lines text)
        numbers = [startline..h] 
        emptylines = replicate 20 "~"
        curLine = length (lines (st^.beforeEdit ++ st^.currentEdit))
        maxNumWidth = length $ show h

        lineNumbersVp = withAttr (chooseModeAttr (st^.editorMode)) $ hLimit (maxNumWidth + 1) $ viewport LinesVP T.Vertical body
        body =  vBox (numWidgets ++ emptyLineWidgets)
        numWidgets = mkNumWidget <$> numbers
        mkNumWidget i = str $ show i
        emptyLineWidgets = mkEmptyLineWidget <$> emptylines
        mkEmptyLineWidget = str



drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        mode = st^.editorMode
        c = renderCustomEditor mode (st^.commandEdit) <+> withAttr (chooseModeAttrCl mode) (padRight Max (vLimit 1(fill ' ')))
        scrollableeditor = withVScrollBars T.OnLeft  c 
        ui = 
            C.center  $
                withAttr (A.attrName "clOFF") (padLeft Max (padRight Max (C.hCenter  (str "  vim with training wheels\no⸂                        ⸃o"))))
                <=> border (
                makeEditorWidget st
                <=>
                withAttr (A.attrName "wheelFG")  (str "Command-line        " <+> scrollableeditor )
                <=>
                 (withAttr (A.attrName "wheelFG") (str "Info                ")<+> withAttr (A.attrName "clOFF") (makeModeWidget (st^.editorMode) <+> str "  " <+> makeFilepathWidget (st^.filepath))
                ) 
                <=> (str "                   " <+> padAll 1 (withAttr (A.attrName "helptext") (str commandHelpText )))
                )
            



