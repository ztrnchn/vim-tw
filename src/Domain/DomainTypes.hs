module Domain.DomainTypes where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Data.Tree
import Numeric.Natural

import qualified Brick.Widgets.Edit as E

data WidgetNames = Name1
                |Name2

type CurrentBuffer = ([Char], [Char], [Char])

data EditorMode = NormalMode
                | VisualMode 
                | InsertMode
                | CommandMode
                deriving (Eq, Show)

type Clipboard = [Char]

type HistoryTree = Tree CurrentBuffer

    

 






