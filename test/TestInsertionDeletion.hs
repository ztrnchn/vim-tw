module TestInsertionDeletion where

import Test.Hspec
import Test.QuickCheck

import Domain.DomainTypes
import Algebra.InsertionDeletion

testInsertion :: IO ()
testInsertion = hspec $ do
    describe "Unittests insertOneBeforeSelection" $ do   
        it "basic check" $ do
            insertOneBeforeSelection 'x' ("ab", "cd", "ef")  `shouldBe` ("abx", "cd", "ef") 

    describe "Propertytests insertOneBeforeSelection" $ do  
        it "testInsertion increases length of Selection by 1" $
            property $ \(a,b,c) x ->
                    let (_,bx,_) = insertOneBeforeSelection x (a,b,c)
                    in length bx == length b


testDeletion :: IO ()
testDeletion = hspec $ do
    describe "Unittests deleteSelection" $ do   
        it "basic check" $ do
            deleteSelection ("ab", "cd", "ef") `shouldBe` ("ab", "e", "f")

    describe "Propertytests deleteSelection" $ do  
        it "deleteSelection preserves length of selection" $
            property $ \(x,y,z) ->
                (length z > 1) ==>
                let (a, b, c) = deleteSelection (x,y,z) 
                in length b == 1  