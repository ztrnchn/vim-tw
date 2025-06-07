module CommandlineHandler.HandleCommands where

import UI.EditorWindow.EditorBuilder
import CommandlineHandler.CommandlineParser
import Algebra.HelperFunctions
import Algebra.Movement
import Domain.DomainTypes
import Algebra.Selection
import Algebra.InsertionDeletion
import FileHandler.FileLoader
import Algebra.History

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import Control.Monad.IO.Class -- needed for liftIO

import qualified Brick.Main as M
import qualified Brick.Types as T
import Control.Monad.State.Class (get, gets)
import Control.Monad (unless, when)
import qualified Brick.Widgets.Edit as E


clearCommandline :: T.EventM Name St ()
clearCommandline = commandEdit .= E.editor Command (Just 4) ""

updateHistory :: CurrentBuffer -> T.EventM Name St ()
updateHistory (c,d,e)= do
    st <- get
    let 
        pos = st ^. historyPos
        h = st ^. history
        (newh, newpos) = addNewToTree pos (c,d,e) h
    history .= newh
    historyPos .= newpos


handleEnter :: T.EventM Name St ()
handleEnter  = do 
    st <- get 
    let mode = st^.editorMode
        cl = unlines (E.getEditContents (st ^. commandEdit))
    case mode of 
        CommandMode -> do
            let command = parseCommandline cl
            case command of 
                (VCommandlineCommand c) -> do
                    handleCommandlineCommand c
                    changeMode NormalMode
                _ -> return ()
        InsertMode -> do
            let  
                c = st ^. beforeEdit
                d = st ^. currentEdit
                e = st ^. afterEdit
                cb  = insertOneBeforeSelection '\n' (c,d,e)
            updateBuffer cb
        _ -> return ()
             

handleBackspace ::  T.EventM Name St ()
handleBackspace = do 
    st <- get 
    let mode = st^.editorMode
    case mode of
        InsertMode -> do 
            let 
                c = st ^. beforeEdit
                d = st ^. currentEdit
                e = st ^. afterEdit
                cb  = deleteOneBeforeSelection (c,d,e)
            updateBuffer cb
        _ -> do
            let newcommandline = safeinit (safeinit (unlines (E.getEditContents (st ^. commandEdit))))
            commandEdit .= E.editor Command (Just 4) newcommandline

handleDel ::  T.EventM Name St ()
handleDel = do 
    st <- get 
    let mode = st^.editorMode
    case mode of
        InsertMode -> do
            let 
                c = st ^. beforeEdit
                d = st ^. currentEdit
                e = st ^. afterEdit
                cb  = deleteOneBeforeSelection (moveRight (c,d,e))
            updateBuffer cb
        _ -> return ()

handleNormalModeCommands ::  ValidInput -> T.EventM Name St ()
handleNormalModeCommands command = do 
    case command of
            (VSelectionCommand sc)-> handleSelectionCommand sc
            (VMovement m) ->  handleMovement m
            (VDirectCommand dc) -> handleDirectCommand dc
            (VRepeatedCommand rc)-> handleRepeatedCommand  rc
            _ -> return ()

handleVisualModeCommands :: ValidInput -> T.EventM Name St ()
handleVisualModeCommands command = do 
      
    case command of
            (VSelectionCommand sc) -> handleSelectionCommand sc
            (VMovement m) ->  handleSelection m
            (VDirectCommand dc) -> handleDirectCommand dc
            (VRepeatedCommand rc)-> handleRepeatedCommand  rc
            _ -> return ()

handleInsertModeCommands :: [Char] -> T.EventM Name St ()
handleInsertModeCommands cl = do 
    st <- get 
    let 
        c = st ^. beforeEdit
        d = st ^. currentEdit
        e = st ^. afterEdit
        [input] = safehead cl
        cb  = insertOneBeforeSelection input (c,d,e)
    updateBuffer cb
    clearCommandline

handleCommands :: T.EventM Name St ()
handleCommands = do 
    st <- get 
    let mode = st^.editorMode
        cl = unlines (E.getEditContents (st ^. commandEdit))
    case mode of
        NormalMode -> handleNormalModeCommands (parseCommandline cl)
        VisualMode -> handleVisualModeCommands (parseCommandline cl)
        InsertMode -> handleInsertModeCommands cl
        _ -> return ()

updateBuffer:: CurrentBuffer -> T.EventM Name St ()
updateBuffer (x,y,z) = do
        beforeEdit .= x 
        currentEdit .= y
        afterEdit .= z

handleSelectionCommand :: SelectionCommand -> T.EventM Name St ()
handleSelectionCommand (a,b,(c,d)) = do 
    st <- get
    let e = st ^. beforeEdit
        f = st ^. currentEdit
        g = st ^. afterEdit
    case b of 
            "d"-> do
                let (x,y,z) = deleteSelection (applySelectiontoBuffer (c,d) (e,f,g))
                editorClipboard .= y
                updateBuffer (x,y,z) 
                updateHistory (x,y,z) 
                
            "y"-> do
                let (x,y,z) = applySelectiontoBuffer (c, d) (e,f,g)
                editorClipboard .= y

    clearCommandline



applySelectiontoBuffer :: Movement -> CurrentBuffer-> CurrentBuffer
applySelectiontoBuffer (a,b) (c,d,e) = cb
    where 
        cb = case b of 
            "h"-> 
                repeatTimes a () (discardUnit unselectRight) (c,d,e)
                
            "j"-> 
                repeatTimes a () (discardUnit selectDown) (c,d,e)
                
            "k"-> 
                repeatTimes a () (discardUnit unselectDown) (c,d,e)
                
            "l"-> 
                repeatTimes a () (discardUnit selectRight) (c,d,e)
            
            "b"-> 
                repeatTimes a () (discardUnit selectBeginWord) (c,d,e)
                
            "e"-> 
                repeatTimes a () (discardUnit selectEndWord) (c,d,e)
                
            "$" -> 
                selectRightUntilNewline (c,d,e)
                
            "0" -> 
                selectLeftUntilNewline (c,d,e)

            "G" -> selectUntilEOF (c,d,e)
            -- would need refinement of visual mode
            --"gg" -> selectFromBegin (c,d,e) 
            _ -> (c,d,e) 


handleMovement :: Movement -> T.EventM Name St ()
handleMovement (a,b) = do 
    st <- get
    let c = st ^. beforeEdit
        d = st ^. currentEdit
        e = st ^. afterEdit
        cb = case b of 
            "h"-> 
                repeatTimes a () (discardUnit moveLeft) (c,d,e)
                
            "j"-> 
                repeatTimes a () (discardUnit moveDown) (c,d,e)
                
            "k"-> 
                repeatTimes a () (discardUnit moveUp) (c,d,e)
                
            "l"-> 
                repeatTimes a () (discardUnit moveRight) (c,d,e)

            "b"-> 
                repeatTimes a () (discardUnit moveBeginWord) (c,d,e)
                
            "e"-> 
                repeatTimes a () (discardUnit moveEndWord) (c,d,e)
                
            "$" -> 
                moveLeft (moveRightUntilNewline (c,d,e))
                
            "0" -> 
                moveLeftUntilNewline (c,d,e)

            "G" -> moveUntilEOF (c,d,e)

            "gg" -> moveUntilBegin (c,d,e)
            _ -> (c,d,e) 
    updateBuffer cb 
    clearCommandline

handleSelection :: Movement -> T.EventM Name St ()
handleSelection s = do 
    st <- get
    let c = st ^. beforeEdit
        d = st ^. currentEdit
        e = st ^. afterEdit
        cb = applySelectiontoBuffer s (c,d,e)

    updateBuffer cb 
    clearCommandline

changeMode :: EditorMode -> T.EventM Name St ()
changeMode m = do
    editorMode .= m 
    
    

handleEsc :: T.EventM Name St ()
handleEsc = do 
    st <- get
    let c = st ^. beforeEdit
        d = st ^. currentEdit
        e = st ^. afterEdit
        cb = resetCursor (c,d,e)
    updateBuffer cb
    changeMode NormalMode
    updateHistory cb
    clearCommandline



handleRepeatedCommand :: RepeatedCommand -> T.EventM Name St ()
handleRepeatedCommand (n, dc) = do
    st <- get
    let c = st ^. beforeEdit
        d = st ^. currentEdit
        e = st ^. afterEdit
        str = st ^. editorClipboard
    case dc of
        "x" -> do
            handleSelectionCommand (n, "d", (0,"l"))
            
        "p" -> do 
            let cb = repeatTimes n str insertStringBeforeSelection (c,d,e)
            updateBuffer cb
            updateHistory cb
        _ -> updateBuffer (c,d,e)
    clearCommandline

   

handleDirectCommand :: DirectCommand -> T.EventM Name St ()
handleDirectCommand dc = do
    st <- get
    let preserveCommandline = dc == ":"
    case dc of
        "i" -> do
            updateBuffer (resetCursor (st ^. beforeEdit,st ^. currentEdit,st ^. afterEdit))
            changeMode InsertMode

        "v" -> changeMode VisualMode
        ":" -> do 
            changeMode CommandMode
        "a" -> do
            updateBuffer (resetCursor (st ^. beforeEdit,st ^. currentEdit,st ^. afterEdit))
            handleMovement (1, "l")
            changeMode InsertMode
        "A" ->  do
            updateBuffer (resetCursor (st ^. beforeEdit,st ^. currentEdit,st ^. afterEdit))
            handleMovement (1, "$")
            changeMode InsertMode
        "x" -> do
            handleSelectionCommand (1, "d", (0,"l"))
            

        "p" -> do 
            handleRepeatedCommand (1,  dc)
            


        "dd" -> do
            let c = st ^. beforeEdit
                d = st ^. currentEdit
                e = st ^. afterEdit
                newselect = selectLeftUntilNewline(selectRightUntilNewline (c,d,e))
                newbuf = deleteSelection newselect
            updateBuffer newbuf
            updateHistory newbuf

        "yy" -> do
            let c = st ^. beforeEdit
                d = st ^. currentEdit
                e = st ^. afterEdit
                (x,y,z) = selectLeftUntilNewline(selectRightUntilNewline (c,d,e))
            editorClipboard .= y
        "u" -> handleUndo

        _ -> return ()
    unless preserveCommandline $ do
        clearCommandline
    
    
    


handleCommandlineCommand :: [Char] -> T.EventM Name St ()
handleCommandlineCommand cc = do
    st <- get
    let 
        fp = st ^.filepath
        c = st ^. beforeEdit
        d = st ^. currentEdit
        e = st ^. afterEdit

    case cc of
        ":q" -> M.halt
        ":wq" -> do
            liftIO $ writeToFile fp (c,d,e)
            M.halt
        ":w" -> liftIO $ writeToFile fp (c,d,e)
        ":help" -> help .= not (st ^. help)
        _ -> return()
    clearCommandline
    
handleRedo :: T.EventM Name St ()
handleRedo = do
    st <- get
    let 
        pos = st ^. historyPos
        h = st ^. history
        newpos = if pos < getMaxPosTree h  then pos + 1 else pos
        newbuf = getBufferFromTree newpos h
    updateBuffer newbuf
    historyPos .= newpos

handleUndo :: T.EventM Name St ()
handleUndo = do
    st <- get
    let 
        pos = st ^. historyPos
        newpos = if pos < 1 then 0 else pos - 1
        h = st ^. history
        newbuf = getBufferFromTree newpos h
    updateBuffer newbuf
    historyPos .= newpos

            
 
  

    

        