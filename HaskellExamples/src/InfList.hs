-----------------------------------------------------------------------------
--
-- Module      :  InfList
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Playing with infinite lists
--
-----------------------------------------------------------------------------

module InfList (
    generatePairs, splitPairs, splitPairsAt
) where

generatePairs :: (t1 -> (t, t1)) -> t1 -> [(t, t1)]
generatePairs f g = h:t
    where
    h@(_, g1) = f g
    t = generatePairs f g1

splitPairs :: [(t, b)] -> (t, [(t, b)])
splitPairs (h:t) = (fst h, t)

splitPairsAt :: Int -> [(b, b1)] -> ([b], [(b, b1)])
splitPairsAt n l = (map fst h, t)
    where
        (h, t) = splitAt n l


