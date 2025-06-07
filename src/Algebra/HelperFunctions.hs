module Algebra.HelperFunctions where

import Numeric.Natural

import Domain.DomainTypes
import qualified Brick.Types as T
import qualified Data.Text as Text
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , vBox
  , hLimit
  , vLimit
  , txtWrap
  , str
  , padAll
  , fill
  , padBottom
  , padRight
  , Padding (..)
  , emptyWidget
  , withAttr
  )

safeinit :: [a] -> [a]
safeinit [] = []
safeinit (x:xs) = init (x:xs)

safelast :: [a] -> [a]
safelast [] = []
safelast (x:xs) = [last (x:xs)]

safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

safehead :: [a] -> [a]
safehead [] = []
safehead (x:xs) = [x]
 
safestr :: String -> T.Widget n
safestr "" = emptyWidget
safestr s  = str  s

safestrWrap :: String -> T.Widget n
safestrWrap "" = emptyWidget
safestrWrap s  = txtWrap (Text.pack s)

checkSelection :: CurrentBuffer -> [Char] -> Bool
checkSelection (a,b,c) d = b==d

getSelectionSize :: CurrentBuffer -> Natural
getSelectionSize (a,b,c) = fromIntegral $ length b

discardUnit::  (CurrentBuffer -> CurrentBuffer) -> a -> CurrentBuffer -> CurrentBuffer
discardUnit f  a = f 

repeatTimes :: Natural -> a -> (a -> CurrentBuffer -> CurrentBuffer) -> (CurrentBuffer -> CurrentBuffer)
repeatTimes 0 _ bm = id
repeatTimes y x bm = bm x . repeatTimes (y-1) x bm

splitbuffer :: CurrentBuffer -> ([Char],[Char],[Char],[Char],[Char])
splitbuffer (a,b,c) = (ax,az,b,cx,cz)
    where
        beforeAsLines = 
            if safelast a == ['\n']
                then lines a ++ [""]
                else lines a
        ax = unlines (safeinit beforeAsLines)
        az:as = if null beforeAsLines 
            then [""]
            else safelast beforeAsLines

        afterAsLines = 
            if safelast b ==['\n'] 
                then "" : lines c
                else lines c
        cx:cs =  if null afterAsLines 
                then [""]
                else safehead afterAsLines
        cz = safeinit ( unlines (safetail afterAsLines))

splitBefore :: [Char] -> Int -> [Char]
splitBefore oldbefore startline = newbefore
    where 
        beforeAsLines = if safelast oldbefore ==['\n'] 
                then lines oldbefore ++ [""]
                else lines oldbefore
        newbefore = safeinit (unlines (drop (startline-1)  beforeAsLines))

replaceEmptyWithNewline :: [String] -> [String]
replaceEmptyWithNewline = map (\s -> if null s then "\n" else s)




