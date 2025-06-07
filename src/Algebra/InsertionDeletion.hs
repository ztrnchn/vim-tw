module Algebra.InsertionDeletion where 

import Domain.DomainTypes
import Algebra.Selection
import Algebra.HelperFunctions

insertOneBeforeSelection :: Char -> CurrentBuffer -> CurrentBuffer
insertOneBeforeSelection x (a,b,c) = (a ++[x], b, c)

insertStringBeforeSelection :: String -> CurrentBuffer -> CurrentBuffer
insertStringBeforeSelection str (a,b,c) = (a ++ str, b, c)

deleteSelection :: CurrentBuffer -> CurrentBuffer
deleteSelection (a,_,c) = 
    if null c 
        then selectLeft (a,[],c)
        else selectRight (a,[],c)

deleteOneBeforeSelection :: CurrentBuffer -> CurrentBuffer
deleteOneBeforeSelection (a,b,c) = (safeinit a,b,c)

