module TestCommandLineParser where



import Test.Hspec
import Test.QuickCheck

import Text.Parsec

import CommandlineHandler.CommandlineParser

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

testParsers :: IO ()
testParsers =  do 
    testcountParser
    testselectionParser
    testmovementParser
    testrepeatedCommandParser
    testselectionCommandParser
    testcommandlineParser



testcountParser :: IO ()
testcountParser = hspec $ do
    describe "Unittests countParser" $ do   
        it "basic check 123" $ do
            parse countParser "" "123"  `shouldBe` Right 123
        it "basic check abc" $ do
            parse countParser "" "abc"  `shouldBe` Right 1



testselectionParser :: IO ()
testselectionParser = hspec $ do
    describe "Unittests selectionParser " $ do   
        it "basic check j" $ do
            parse selectionParser "" "j"  `shouldBe` Right "j"


testmovementParser :: IO ()
testmovementParser = hspec $ do
    describe "Unittests movementParser " $ do   
        it "basic check" $ do
            parse movementParser  "" "j"  `shouldBe` Right (1,"j")


testdirectCommandParser :: IO ()
testdirectCommandParser = hspec $ do
    describe "Unittests directCommandParser" $ do   
        it "basic check" $ do
            parse directCommandParser "" "x"  `shouldBe` Right "x"


testrepeatedCommandParser:: IO ()
testrepeatedCommandParser = hspec $ do
    describe "Unittests repeatedCommandParser" $ do   
        it "basic check" $ do
            parse repeatedCommandParser "" "3x"  `shouldBe` Right (3,"x")


testselectionCommandParser :: IO ()
testselectionCommandParser = hspec $ do
    describe "Unittests selectionCommandParser " $ do   
        it "basic check" $ do
            parse selectionCommandParser  "" "dk"  `shouldBe` Right (1,"d",(1,"k"))


testcommandlineParser:: IO ()
testcommandlineParser = hspec $ do
    describe "Unittests commandlineParser:" $ do   
        it "basic check" $ do
            parse commandlineParser "" "3d4j"  `shouldBe` Right (VSelectionCommand (3,"d",(4,"j")))






