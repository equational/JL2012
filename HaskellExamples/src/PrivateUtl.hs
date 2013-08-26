-----------------------------------------------------------------------------
--
-- Module      :  PrivateUtl
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

module PrivateUtl (
   mtr,
   mapOpt,
   fixEither,
   fixEither2
) where

import Debug.Trace

mtr :: Show a => [Char] -> a -> a
mtr n x =
    trace (n++": "++(show x)) x


mapOpt fOpt l = map' l where
    map' [] = []
    map' (h:t) =
        let t' = map' t in
        case fOpt h of
        Just h' -> (h':t')
        Nothing -> t'

-- | fix point over left value until right value
fixEither:: (a->(Either a b))->a->b
fixEither f x = fixEither' x where
    fixEither' x =
        case f x of
        Left y -> fixEither' y
        Right y -> y

-- | fix point over left value until right value
fixEither2:: (a->(Either a b))->a->(b,[a])
fixEither2 f x = fixEither' [] x where
    fixEither' t x =
        case f x of
        Left y -> fixEither' (y:t) y
        Right y -> (y,t)

