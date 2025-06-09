module Algebra.Selection where

import Domain.DomainTypes
import Algebra.HelperFunctions

import Numeric.Natural



selectRight :: CurrentBuffer -> CurrentBuffer
selectRight (a, b, c:cs) = (a, b ++ [c], cs)
selectRight (a,b,c) = (a,b,c)

selectRightInline :: CurrentBuffer -> CurrentBuffer
selectRightInline (a, b, c) = 
    if safelast b == "\n" 
        then (a, b, c)
        else selectRight (a,b,c)

unselectRight :: CurrentBuffer -> CurrentBuffer
unselectRight (a, b:bs, c) = 
    if length (b:bs) < 2 
        then (a, b:bs, c) 
        else (a, safeinit (b:bs), safelast(b:bs) ++ c)
unselectRight (a,b,c) = (a,b,c)

-- Collapse the selection to the left bound of the selection
unselectRightAll :: CurrentBuffer -> CurrentBuffer
unselectRightAll (a, b:bs, c) = (a, [b], bs ++ c)
unselectRightAll x = x

selectLeft :: CurrentBuffer -> CurrentBuffer
selectLeft (a:as, b, c) = (safeinit(a:as), safelast (a:as) ++ b, c)
selectLeft (a,b,c) = (a,b,c)

selectLeftInline :: CurrentBuffer -> CurrentBuffer
selectLeftInline (a, b, c) = 
    if safelast a == "\n" 
        then (a, b, c)
        else selectLeft (a, b, c)

unselectLeft :: CurrentBuffer -> CurrentBuffer
unselectLeft (a, b:bs, c) = 
    if length (b:bs) < 2 
        then (a, b:bs, c) 
        else (a ++ [b], bs,  c)
unselectLeft (a,b,c) = (a,b,c)


selectRightUntilNewline :: CurrentBuffer -> CurrentBuffer
selectRightUntilNewline (a,b,[]) = (a,b,[])
selectRightUntilNewline (a,b,c) = 
    if safelast e == "\n"
        then (d,e,f)
        else selectRightUntilNewline (d,e,f)
    where (d,e,f) = selectRight(a,b,c)

unselectRightUntilNewline :: CurrentBuffer -> CurrentBuffer
unselectRightUntilNewline (a,b,c) = 
    if safelast b == "\n"|| length b == 1
        then (a,b,c)
        else unselectRightUntilNewline (unselectRight (a,b,c))

selectLeftUntilNewline :: CurrentBuffer -> CurrentBuffer
selectLeftUntilNewline ([],b,c) = ([],b,c)
selectLeftUntilNewline (a,b,c) = 
    if safelast a  == "\n"
        then (a,b,c)
        else selectLeftUntilNewline (selectLeft(a,b,c))


--Index starting at 1
getInlinePos :: CurrentBuffer -> Natural
getInlinePos ([],b,c) = 1
getInlinePos (a,b:bs,c) = 
    if b == '\n'
        then 0
        else 1 + getInlinePos ( selectLeft (a,b:bs,c))
getInlinePos (a,b,c) = 1 + getInlinePos ( selectLeft (a,b,c)) 

--Index starting at 1
getLastInlinePos :: CurrentBuffer -> Natural
getLastInlinePos (a,b,c) = getInlinePos ([],newb, [])
    where 
        bAsLines = lines b
        [newb] = safelast bAsLines
        


selectUp :: CurrentBuffer -> CurrentBuffer
selectUp (a,b,c) = 
     repeatTimes count unselectRight newbuf
        where 
            pos = getInlinePos (a,b,c)
            count = if pos < 1 
                then pos
                else pos - 1
            newbuf = selectLeftUntilNewline $ selectLeft $ selectLeftUntilNewline (a,b,c)

selectDown :: CurrentBuffer -> CurrentBuffer
selectDown (a,b,c) =
    repeatTimes count selectRight newbuf
        where 
            pos = getInlinePos (a,b,c)
            count = if pos < 1 
                then pos
                else pos - 1
            newbuf = selectRightUntilNewline (a,b,c)

unselectDown :: CurrentBuffer -> CurrentBuffer
unselectDown (a,b,c) = if  '\n' `elem` e
    then repeatTimes count selectRightInline (n,e,w)
    else (n,e,w)
        where 
            pos = getLastInlinePos (a,b,c)
            count = if pos < 1 
                then pos
                else pos - 1
            (n,e,w) = unselectRightUntilNewline (unselectRight (a,b,c))


selectUntilEOF :: CurrentBuffer -> CurrentBuffer
selectUntilEOF (a,b,[]) = (a,b,[])
selectUntilEOF (a,b,c) = selectUntilEOF (selectRight (a,b,c))

selectFromBegin :: CurrentBuffer -> CurrentBuffer
selectFromBegin ([],b,c) = ([],b,c) 
selectFromBegin (a,b,c) = selectFromBegin (selectLeft (a,b,c))

selectBeginWord :: CurrentBuffer -> CurrentBuffer
selectBeginWord ([],b,c) = ([],b,c)
selectBeginWord (a,b,c) = let (e,f,g) = selectLeft (a,b,c) 
    in if safelast e == [' ']|| safelast e == ['\n']
        then (e,f,g)
        else selectBeginWord (e,f,g)

selectEndWord :: CurrentBuffer -> CurrentBuffer
selectEndWord (a,b,[]) = (a,b,[])
selectEndWord (a,b,c) = let (e,f,g) = selectRight (a,b,c) 
    in if safehead g == [' '] || safehead g == ['\n']
        then (e,f,g)
        else selectEndWord (e,f,g)


resetCursor :: CurrentBuffer -> CurrentBuffer
resetCursor (a,b,c) = 
    if length b == 1 
        then (a,b,c)
        else resetCursor (unselectRight (a,b,c))
    
