module TestHelperFunctions where

import Test.Hspec
import Test.QuickCheck

import Domain.DomainTypes
import Algebra.HelperFunctions
import Algebra.Movement
import Algebra.Selection


testCheckSelection :: IO ()
testCheckSelection = hspec $ do
    describe "Unittests checkSelection" $ do   
        it "basic check" $ do
            checkSelection ("ab", "cd", "ef") "cd" `shouldBe` True

    describe "Propertytests checkSelection" $ do  
        it "basic check" $
            property $ \(a,b,c) d ->
                (b /= d) ==>
                    not $ checkSelection (a,b,c) d

testgetSelectionSize :: IO ()
testgetSelectionSize = hspec $ do
    describe "Unittests getSelectionSize" $ do   
        it "basic" $ do
            getSelectionSize ("ab", "cd", "ef")  `shouldBe` 2

    describe "Propertytests getSelectionSize" $ do  
        it "basic" $
            property $ \(a,b,c) ->
                getSelectionSize (a,b,c)  == fromIntegral (length b)

testgetInlinePos :: IO ()
testgetInlinePos = hspec $ do
    describe "Unittests getInlinePos" $ do   
        it "basic" $ do
            getInlinePos ("ab", "cd", "ef")  `shouldBe` 3

        it "multiline" $ do
            getInlinePos ("ab\n", "cd", "ef")  `shouldBe` 1

        it "multiline" $ do
            getInlinePos ("ab\n", "\nft", "ef")  `shouldBe` 0

        it "multiline" $ do
            getInlinePos ("abc\n", "abc", "abc\nabc")  `shouldBe` 1


testrepeatTimes :: IO ()
testrepeatTimes = hspec $ do
    describe "Unittests " $ do   
        it "repeatTimes 0 moveRight" $ do
            repeatTimes 0 () (discardUnit moveRight) ("ab", "cd", "ef") `shouldBe` ("ab", "cd", "ef")

        it "repeatTimes 1 selectRight" $ do
            repeatTimes 1 () (discardUnit selectRight) ("ab", "cd", "ef")  `shouldBe` ("ab", "cde", "f")

        it "repeatTimes 3 selectRight" $ do
            repeatTimes 3 () (discardUnit selectRight) ("ab", "cd", "ef") `shouldBe`  ("ab", "cdef", "")

    describe "Propertytests repeatTimes" $ do  
        it "repeatTimes 0 x f cb = cb --> input 0 -> id function" $
            property $ \cb ->
                repeatTimes 0 () (discardUnit selectRight)cb  == cb


testsplitbuffer :: IO ()
testsplitbuffer = hspec $ do
    describe "Unittests splitbuffer" $ do   
        it "splitbuffer basic" $ do
            splitbuffer ("ab\ncde\nfg", "hijk", "lmn\nop\nqref") `shouldBe` ("ab\ncde\n","fg", "hijk", "lmn" ,"op\nqref")

        it "splitbuffer emptylist" $ do
            splitbuffer (""," ","\n\nhello") `shouldBe` ("",""," ","","\nhello")

        it "splitbuffer on second line" $ do
            splitbuffer ("trt\n","abc","\n\ntuut") `shouldBe` ("trt\n","","abc","","\ntuut")

        it "splitbuffer on newlines" $ do
            lines "\n\n\n\n" `shouldBe` ["", "", "", ""]



testsplitBefore :: IO ()
testsplitBefore = hspec $ do
    describe "Unittests splitBefore" $ do   
        it "splitBefore basic" $ do
            splitBefore "ab\ncde\nfg" 1 `shouldBe` "ab\ncde\nfg"

        it "splitBefore start 2" $ do
            splitBefore "ab\ncde\nfg" 2`shouldBe` "cde\nfg"

        it "splitBefore on second line" $ do
            splitBefore "ab\ncde\nfg" 3 `shouldBe` "fg"

        it "splitBefore on tetslines" $ do
            splitBefore "1\n2\n3\n4\n5" 3 `shouldBe` "3\n4\n5"


