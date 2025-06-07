import Test.Hspec
import Control.Exception (evaluate)

import TestSelectMove
import TestHelperFunctions
import TestInsertionDeletion
import TestCommandLineParser
import TestHistory


main :: IO ()
main = do 
    testTree
    testMovement
    testsplitBefore
    testsplitbuffer
    testgetInlinePos
    testrepeatTimes
    testSelection
    testCheckSelection
    testgetSelectionSize
    testInsertion
    testDeletion
    testParsers
