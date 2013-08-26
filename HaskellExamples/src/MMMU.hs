-----------------------------------------------------------------------------
--
-- Module      :  MMMU
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

module MMMU (
    firstSuccess
) where

import MMM hiding ((>>=))
import Prelude

instance
    Monad (StateR err state state) where
    (>>=) = bind
    return = ret


iterRightState f l = iterRightState' l where
    iterRightState' l =
        do
        case l of
            (h:t)  ->
                let x = f h -- value must be forced
                in iterRightState' t
            []    ->
                return ()


firstSuccess
  ::    StateR err stateIn stateOut ret
     -> (t -> StateR err stateIn stateOut ret)
     -> [t]
     -> StateR err stateIn stateOut ret
firstSuccess failure f l = firstSuccess' l where
    firstSuccess' l =
        case l of
            (h:t)  ->
                toOrState
                    (f h)
                    (firstSuccess' t)
            []    ->
                failure


--foldRightState f s l = foldRightState' s l where
--    foldRightState' s l =
--        case l of
--        (h:t)  ->
--            foldRightState' (f s h) t
--        []    ->
--            do return s
--
--
--
--foldLeftState f l s = foldLeftState' l s where
--    foldLeftState' l s =
--            case l of
--            (h:t)  ->
--                let t' = foldLeftState' t s
--                in (f h):(t')
--            []    ->
--                do return s
--
--
--mapRightState f l = mapRightState' l where
--    mapRightState' l =
--            case l of
--            []    ->
--                return []
--            (h:t)  ->
--                let t' = mapRightState' t
--                in return ((f h):t')
--
--
--
--mapFoldRightState f s l = mapFoldRightState' s l where
--    mapFoldRightState' s l =
--            case l of
--            (h:t)  ->
--                let (s', h') = f h
--                    (s2', t') = mapFoldRightState' s' t
--                in return (s2', (h':t'))
--            []    ->
--                return (s, [])
--
