module TestSelectMove where

import Test.Hspec
import Test.QuickCheck

import Domain.DomainTypes
import Algebra.Selection
import Algebra.Movement
import Algebra.HelperFunctions

testSelection :: IO ()
testSelection = hspec $ do
    describe "Unittests Selections" $ do   
        it "select to right basic" $ do
            selectRight ("ab", "cd", "ef") `shouldBe` ("ab", "cde", "f")
        it "select to right" $ do
            selectRight ("","a\n","") `shouldBe` ("", "a\n", "")

        it "selectRightInline basic" $ do
            selectRightInline ("ab", "cd", "ef") `shouldBe` ("ab", "cde", "f")
        it "selectRightInline multiline" $ do
            selectRightInline ("ab", "cd\n", "ef") `shouldBe` ("ab", "cd\n", "ef")

        it "unselect to right basic" $ do
            unselectRight ("ab", "cde", "f") `shouldBe` ("ab", "cd", "ef")
        it "unselect to right" $ do
            unselectRight ("","a","") `shouldBe` ("","a","")

        it "select to left" $ do
            selectLeft ("a","","") `shouldBe` ("","a","")
        it "unselect to left" $ do
            unselectLeft ("a", "bcd", "ef") `shouldBe` ("ab", "cd", "ef")

        it "selectLeftInline basic" $ do
            selectLeftInline ("a","","") `shouldBe` ("","a","")

        it "selectLeftInline multiline" $ do
            selectLeftInline ("a\n","b","") `shouldBe` ("a\n","b","")

        it "selectRightUntilNewline multiline" $ do
            selectRightUntilNewline ("a\n","b","cde\nfghi") `shouldBe` ("a\n","bcde\n","fghi")
        
        it "selectRightUntilNewline multi-new-line" $ do
            selectRightUntilNewline ("a\n","b","cd\n\nfghi") `shouldBe` ("a\n","bcd\n","\nfghi")
            
        it "selectLeftUntilNewline multiline" $ do
            selectLeftUntilNewline ("a\nbcde","fg\n","hijk") `shouldBe` ("a\n","bcdefg\n","hijk")

        it "selectLeftUntilNewline multi-new-line" $ do
            selectLeftUntilNewline ("a\n\nbcde","fg\n","hijk") `shouldBe` ("a\n\n","bcdefg\n","hijk")

        it "selectUp multiline" $ do
            selectUp ("a\na\n","b","cde\nfghi") `shouldBe` ("a\n","a\nb","cde\nfghi")
            
        it "selectDown multiline" $ do
            selectDown ("a\nbcde","fg","hijk\nlmnop") `shouldBe` ("a\nbcde","fghijk\nlmno","p")

        it "unselectRightUntilNewline multiline" $ do
            unselectRightUntilNewline ("a\n","bcde\n","fghi") 
                `shouldBe`            ("a\n","bcde\n","fghi") 

        it "unselectRightUntilNewline multiline" $ do
            unselectRightUntilNewline ("a\n","bcde\nn","fghi") 
                `shouldBe`            ("a\n","bcde\n","nfghi")


        it "unselectDown multiline" $ do
            unselectDown   ("a\nbcde","fgh\nijk\nlmno","p") 
                `shouldBe` ("a\nbcde","fgh\nijk\n","lmnop")

        it "unselectDown multi-new-line" $ do
            unselectDown   ("a\nbcde","fgh\n\n","lmn") 
                `shouldBe` ("a\nbcde","fgh\n","\nlmn")



    describe "Propertytests Selections" $ do  


        it "selectRight(unselectRight) is id" $
            property $ \(a,b,c) ->
                (length b > 1 && c /= [] && safelast b /= "\n") ==>
                    selectRight (unselectRight (a,b,c)) == (a,b,c)
            
        it "unselectRight(selectRight) is id" $
            property $ \(a,b,c) ->
                (length b > 1 && c /= [] && safelast b /= "\n") ==>
                    unselectRight (selectRight (a,b,c)) == (a,b,c)

        it "selectRight increases length of selection by 1" $
            property $ \(a,b,c) ->
                (length b > 1 && c /= [] && safelast b /= "\n") ==>
                    let (_,bx,_) = selectRight (a,b,c)
                    in length bx == length b +1

        it "unselectRight decreases length of selection by 1" $
            property $ \(a,b,c) ->
                (length b > 1) ==>
                    let (_,bx,_) = unselectRight (a,b,c)
                    in length bx == length b -1

        it "selectLeft increases length of selection by 1" $
            property $ \(a,b,c) ->
                (a /= [] && safelast a /= "\n") ==>
                    let (_,bx,_) = selectLeft (a,b,c)
                    in length bx == length b +1

        it "unselectLeft decreases length of selection by 1" $
            property $ \(a,b,c) ->
                (length b > 1 ) ==>
                    let (_,bx,_) = unselectLeft (a,b,c)
                    in length bx == length b -1

        

testMovement :: IO ()
testMovement = hspec $ do
    describe "Unittests Movement" $ do   
        it "move to right" $ do
            moveRight ("ab", "cd", "ef") `shouldBe` ("abc", "de", "f")
        it "move to left" $ do
            moveLeft ("ab", "cd", "ef") `shouldBe` ("a", "bc", "def")


        it "move to right (,,a)" $ do
            moveRight ("","","a") `shouldBe` ("","a","")
        it "move to left (a, ,a)" $ do
            moveLeft ("a","","a") `shouldBe` ("", "a", "a")

        it "move to right until newline basic" $ do
            moveRightUntilNewline ("abc\n","a","bc\nabc") `shouldBe` ("abc\nabc\n","a","bc")
        it "move to left until newline basic" $ do
            moveLeftUntilNewline ("abc\nabc","abc","\nabc") `shouldBe` ("abc\n", "abc", "abc\nabc")
        it "move to left until newline emptyline" $ do
            moveLeftUntilNewline ("abc\n\nabc","abc","\nabc") `shouldBe` ("abc\n\n", "abc", "abc\nabc")

        it "move to right until newline on last line" $ do
            moveRightUntilNewline ("abc","a","bcabc") `shouldBe` ("abcabcab","c","")
        it "move to left until newline on first line" $ do
            moveLeftUntilNewline ("abc","abc","abc") `shouldBe` ("", "abc", "abcabc")

        it "moveUp basic (abc\nabc\n, abc, \nabc) " $ do
            moveUp ("abc\nabc\n","abc","\nabc") `shouldBe` ("abc\n","abc","\nabc\nabc")
        it "moveUp emptyline (abc\nabc\n\n, abc, \nabc) " $ do
            moveUp ("abc\nabc\n\n","a","bc\nabc") `shouldBe` ("abc\nabc\n","\n","abc\nabc")

        it "moveDown basic (abc,\n,\nabcde) " $ do
            moveDown ("abc","\n","\nabcde") `shouldBe` ("abc\n", "\n", "abcde")

        it "moveDown basic (abc\nabc,d,e\nabcdef) " $ do
            moveDown ("abc\nabc","d","e\nabcdef") `shouldBe` ("abc\nabcde\nabc", "d", "ef")

    describe "Propertytests movement" $ do
{-
        it "moveRight(moveLeft) is id" $
            property $ \(a,b,c) ->
                (a /= [] && safelast a /= "\n" ) ==>
                moveRight (moveLeft (a,b,c)) == (a,b,c)

        it "moveLeft(moveRight) is id" $
            property $ \(a,b,c) ->
                (c /= []) ==>
                moveLeft (moveRight (a,b,c)) == (a,b,c)

        it "moveRight preserves length of selection" $
            property $ \(a,b,c) ->
                let (_,bx,_) = moveRight (a,b,c)
                in length bx == length b

        it "moveLeft preserves length of selection" $
            property $ \(a,b,c) ->
                let (_,bx,_) = moveLeft (a,b,c)
                in length bx == length b
-}
        it "moveRightUntilNewline preserves length of selection" $
            property $ \(a,b,c) ->
                b /= "" ==>
                    let (_,bx,_) = moveRightUntilNewline (a,b,c)
                    in length bx == length b

        it "moveLeftUntilNewline preserves length of selection" $
            property $ \(a,b,c) ->
                b /= "" ==>
                    let (_,bx,_) = moveLeftUntilNewline (a,b,c)
                    in length bx == length b




    
