module FileHandler.FileLoader where

import System.IO
import System.Directory 
import UI.Tutorial
import Domain.DomainTypes

import Control.Monad.IO.Class -- needed for liftIO

loadIntoCurrentBuffer :: [Char] -> CurrentBuffer
loadIntoCurrentBuffer (a:as) =  ([], [a], as)
loadIntoCurrentBuffer _ =  ([], [], [])

checkFilepath :: FilePath -> IO CurrentBuffer
checkFilepath fp = case fp of
    "Tutorial" -> return (loadIntoCurrentBuffer tutorialText)
    _ -> do
        fileexists <- doesFileExist fp
        if fileexists 
            then loadFile fp
            else do
                makeNewFile fp
                loadFile fp
            

loadFile :: FilePath -> IO CurrentBuffer
loadFile fp = do
    contents <- readFile fp 
    return(loadIntoCurrentBuffer contents)

makeNewFile :: FilePath -> IO ()
makeNewFile fp = writeFile fp " "

getCurrentDir :: IO FilePath
getCurrentDir = do getCurrentDirectory 

writeToFile :: FilePath -> CurrentBuffer -> IO ()
writeToFile fp (a,b,c) = 
    case fp of
        "Here would be the Filepath if this was not the tutorial" -> return ()
        _ -> do 
            let cb = a ++ b ++ c 
            writeFile fp cb 
    
        