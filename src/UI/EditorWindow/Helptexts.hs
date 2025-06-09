module UI.EditorWindow.Helptexts where

normalHelpText :: [Char]
normalHelpText =  "movement \n"
        ++ "k up\n"
        ++ "j down\n"
        ++ "h left\n"
        ++ "l right\n"
        ++ "b begin of word\n"
        ++ "e end of word\n"
        ++ "gg begin of file\n"
        ++ "G end of file\n"
        ++ "<C-u> page up\n"
        ++ "<C-d> page down\n\n"
        
        ++ "change mode\n"
        ++ "i insert\n"
        ++ "a = l + i\n"
        ++ "v visual\n\n"

        ++ "edit text\n"
        ++ "x copy & delete\n"
        ++ "d copy & delete\n"
        ++ "y copy\n"
        ++ "p put copied\n\n"

        ++ "u = undo\n"
        ++ "Ctrl r = redo \n\n"



insertHelpText :: [Char]
insertHelpText = "change mode\n"
        ++ "Esc normal\n"
    


commandHelpText :: [Char]
commandHelpText = ":w (save) "
        ++ ":q (quit) "
        ++ ":wq (save & quit) "
        ++ ":help (toggle help)"
    

visualHelpText :: [Char]
visualHelpText = "selection \n"
        ++ "k up\n"
        ++ "j down\n"
        ++ "h left\n"
        ++ "l right\n"
        ++ "b begin of word\n"
        ++ "e end of word\n"
        -- ++ "gg begin of file\n"
        ++ "G end of file\n\n"
        
        ++ "change mode\n"
        ++ "i insert\n"
        ++ "a = l + i\n"
        ++ "v visual\n\n"

        ++ "edit text\n"
        ++ "x copy & delete\n"
        ++ "d copy & delete\n"
        ++ "y copy\n"
        ++ "p put copied\n\n"

        ++ "u = undo\n"
        ++ "Ctrl r = redo \n\n"

