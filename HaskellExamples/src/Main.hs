-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import System.Random
import InfList
import AHeap
import MMM
import Prelude -- hiding ((>>=))
import MMMI
import MMMU
import MEquSol
import ESTest
import Debug.Trace
import MEquUtl


selection key level (a1,a2) i =
    if i < a1 then (LT, i)
    else if i == a1 then (EQ, 0)
    else (GT, i-a1-1)


--randToList gl node =
--    where
--    (s1, s2) = headAug h
--    s = 1+s1+s2
--    (hv, gl') = splitPairs gl
--    s' =
--    choose selection s h


test1 :: ([Int], [(Int, StdGen)])
test1 =
    let gl = generatePairs (randomR (0,5)) (mkStdGen 1)
    in splitPairsAt 30 gl

--test2::(ANode (Int,Int) Int)
--test2 = fromList l
--    where
--    (l, g) =  test1

instance ANodeAugmentation Int (Int,Int) where
    mkAHeap (d, level, l, r) =
        case (l,r) of
        (Node ldata _ (la1,la2) _ _,
              Node rdata _ (ra1,ra2)  _ _ ) ->
            let cl = if ldata == d then 1+la1+la2 else 0
                cr = if rdata == d then 1+ra1+ra2 else 0
            in (cl,cr)
        (Node ldata _ (la1,la2) _ _, Leaf) ->
            let cl = if ldata == d then 1+la1+la2 else 0
            in (cl,0)
        (Leaf, Node rdata _ (ra1,ra2)  _ _ ) ->
            let cr = if rdata == d then 1+ra1+ra2 else 0
            in (0,cr)
        (Leaf, Leaf) ->
             (0,0)



test2::BinaryHeap Int (Int,Int)
test2 = fromList l
    where
    (l, g) =  test1



m1 = mapState (\ x -> x+1)
-- m2 y = applyState (\ x -> show((x*y)))
m2 y = applyState (\ x -> (x*y))



test3 =
    m1 MMM.>>= \ x ->
    m1 MMM.>>= \ y ->
    m1 MMM.>>= \ z ->
    m1

test3' =
    do
--        x <- m1
--        y <- m1
        m1
        m1


--test3 =
--    do
--        x <- m1
--        m2 x

test4 :: RetState String Int ()
test4 =
    runState test3' 0

main =
    do
--        putStrLn (show test2);
--        putStrLn (show (toList test2))
        putStrLn  (show (testEq1))
--        trace "Trace0" (putStrLn (show testEq1))


