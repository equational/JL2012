-----------------------------------------------------------------------------
--
-- Module      :  CFrac
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Fun continued fractions: given the terms of the continued fraction, produces
-- a list of corresponding fractions.
--
-----------------------------------------------------------------------------

module CFrac (
    cf2q
) where

-- a/b = x + y/R -> (a+b*x)/b = y/R -> b/y*(a+b*x) = R

-- a2/b2 = a1/(b1+


cf2q cfs = cf1 0 1 cfs
    where
    cf1 a b ((x,y):t) = (a,b):(cf1 b (y*(a+b*x)) t)
    cf1 a b [] = []


-- 5/12 = 0 + 1/(2 + 1/4)
--

--        putStrLn (show (cf2q [(5,1),(5,1),(5,1),(5,1),(5,1)]))
