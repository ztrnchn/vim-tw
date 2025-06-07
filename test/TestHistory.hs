module TestHistory where

import Test.Hspec
import Test.QuickCheck

import Domain.DomainTypes
import Algebra.History
import Numeric.Natural
import Data.Tree


tree1 :: HistoryTree
tree1 = 
    Node ("a","b","cd") 
        [ Node ("a","b","ce")
            [ Node ("a","b","cf") 
                [ Node ("a","b","cg") [], Node ("a","b","ch") [] ]
            ]
        ]

tree2 :: HistoryTree
tree2 = 
    Node ("a","b","cd") 
        [ Node ("a","b","ce")
            [ Node ("1","2","3") [], Node ("a","b","cf") 
                [ Node ("a","b","cg") [] ,
                 Node ("a","b","ch") [] 
                ]
            ]
        ]

tree3 :: HistoryTree
tree3 =
     Node ("a","b","cg") []

tree4 :: HistoryTree
tree4 =
    Node ("a","b","cd") 
        [ Node ("a","b","ce")
            [ Node ("a","b","cf") 
                []
            ]
        ]   

tree5 :: HistoryTree
tree5 = 
    Node ("a","b","cd") 
        [ Node ("a","b","cg") [],
        Node ("a","b","ce")
            [ Node ("a","b","cf") 
                []
            ]
        ]

tree6 :: HistoryTree
tree6 =
     Node ("a","b","cg") [Node ("a","b","cg") []]



testTree :: IO ()
testTree = do
    testaddToTree
    testgetCurrentTree
    testgetParentTree
    testgetjoinTrees
    testgetBufferFromTree
    testaddNewToTree
    testgetMaxPosTree

testaddToTree :: IO ()
testaddToTree =  hspec $ do
    describe "Unittests addToTree" $ do   
        it "basic add (1,2,3)" $ do
            addToTree 0 ("a","b","cg") tree4 `shouldBe` tree5

testgetCurrentTree :: IO ()
testgetCurrentTree =  hspec $ do
    describe "Unittests getCurrentTree" $ do   
        it "basic get 3rd" $ do
            getCurrentTree 3 tree1 `shouldBe` tree3

testgetParentTree:: IO ()
testgetParentTree =  hspec $ do
    describe "Unittests getParentTree" $ do   
        it "basic get 3rd" $ do
            getParentTree 3 tree1 `shouldBe` tree4

testgetjoinTrees:: IO ()
testgetjoinTrees =  hspec $ do
    describe "Unittests joinTrees" $ do   
        it "basic get 3rd" $ do
            joinTrees tree3 tree3 `shouldBe` tree6

testgetBufferFromTree:: IO ()
testgetBufferFromTree =  hspec $ do
    describe "Unittests getBufferFromTree" $ do   
        it "basic get 3rd" $ do
            getBufferFromTree 1 tree3  `shouldBe` ("a","b","cg") 

testaddNewToTree:: IO ()
testaddNewToTree =  hspec $ do
    describe "Unittests addNewToTree" $ do   
        it "basic get tree3" $ do
            addNewToTree 1  ("a","b","cg") tree3 `shouldBe` (tree3, 1)
        
        

testgetMaxPosTree:: IO ()
testgetMaxPosTree =  hspec $ do
    describe "Unittests getMaxPosTree" $ do   
        it "basic get tree3" $ do
            getMaxPosTree tree3 `shouldBe` 0

        it "basic get tree5" $ do
            getMaxPosTree tree5 `shouldBe` 1


