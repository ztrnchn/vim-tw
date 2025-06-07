module Algebra.Movement where

import Domain.DomainTypes
import Algebra.Selection
import Algebra.HelperFunctions

import Numeric.Natural


moveRight :: CurrentBuffer -> CurrentBuffer
moveRight (a,b,[]) = (a,b,[])
moveRight (a,b,c) = unselectLeft $ selectRight (a,b,c)

moveRightInline :: CurrentBuffer -> CurrentBuffer
moveRightInline (a,b,[]) = (a,b,[])
moveRightInline (a,b,c) = unselectLeft $ selectRightInline (a,b,c)

moveLeft :: CurrentBuffer -> CurrentBuffer
moveLeft ([],b,c) = ([],b,c)
moveLeft (a,b,c) = unselectRight $ selectLeft (a,b,c)

moveLeftInline :: CurrentBuffer -> CurrentBuffer
moveLeftInline ([],b,c) = ([],b,c)
moveLeftInline (a,b,c) = unselectRight $ selectLeftInline (a,b,c)

skipToFirstLine :: CurrentBuffer -> CurrentBuffer
skipToFirstLine (before, current, after) =
    let newCurrent = safehead before
        newAfter = safetail before <> current <> after
    in ([], newCurrent, newAfter)

skipToLastLine :: CurrentBuffer -> CurrentBuffer
skipToLastLine (before, current, after) =
    let (remaining, lastLine) = (safeinit after, safelast after)
        newBefore = before <> current <> remaining
    in (newBefore, lastLine, [])

moveRightUntilNewline :: CurrentBuffer -> CurrentBuffer
moveRightUntilNewline (a,b,[]) = (a,b,[])
moveRightUntilNewline (a,b,c) = 
    if safelast d == "\n"
        then (d,e,f)
        else moveRightUntilNewline (d,e,f)
    where (d,e,f) = moveRight(a,b,c)

moveLeftUntilNewline :: CurrentBuffer -> CurrentBuffer
moveLeftUntilNewline ([],b,c) = ([],b,c)
moveLeftUntilNewline (a,b,c) = 
    if safelast a  == "\n"
        then (a,b,c)
        else moveLeftUntilNewline (moveLeft(a,b,c))

moveUp :: CurrentBuffer -> CurrentBuffer
moveUp (a,b,c) = 
     repeatTimes count () (discardUnit moveRightInline) newbuf
        where 
            pos = getInlinePos (a,b,c)
            count = if pos < 1 
                then pos
                else pos - 1
            newbuf = moveLeftUntilNewline $ moveLeft $ moveLeftUntilNewline (a,b,c)

moveDown :: CurrentBuffer -> CurrentBuffer
moveDown (a,b,c) =
    repeatTimes count () (discardUnit moveRightInline) newbuf
        where 
            pos = getInlinePos (a,b,c)
            count = if pos < 1 
                then pos
                else pos - 1
            newbuf = moveRightUntilNewline (a,b,c)

moveUntilEOF :: CurrentBuffer -> CurrentBuffer
moveUntilEOF (a,b,[]) = (a,b,[])
moveUntilEOF (a,b,c) = moveUntilEOF (moveRight (a,b,c))

moveUntilBegin :: CurrentBuffer -> CurrentBuffer
moveUntilBegin ([],b,c) = ([],b,c)
moveUntilBegin (a,b,c) = moveUntilBegin (moveLeft (a,b,c))

moveBeginWord :: CurrentBuffer -> CurrentBuffer
moveBeginWord ([],b,c) = ([],b,c)
moveBeginWord (a,b,c) = let (e,f,g) = moveLeft (a,b,c) 
    in if safelast e == [' '] || safelast e == ['\n']
        then (e,f,g)
        else moveBeginWord (e,f,g)

moveEndWord :: CurrentBuffer -> CurrentBuffer
moveEndWord (a,b,[]) = (a,b,[])
moveEndWord (a,b,c) = let (e,f,g) = moveRight (a,b,c) 
    in if safehead g == [' '] || safehead g == ['\n']
        then (e,f,g)
        else moveEndWord (e,f,g)
