-----------------------------------------------------------------------------
--
-- Module      :  HighOrder
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Playing with function signatures
--
-----------------------------------------------------------------------------

module HighOrder (
    fxx2fxx'
) where

fx2fx' ::
    (x -> ret)
    -> ((y -> x) -> (y -> ret))
fx2fx' f fct y = f (fct y)

--fx'2fx ::
--    ((y -> x) -> (y -> ret))
--    -> y
--    -> (x -> ret)
--fx'2fx fct y x = f (fct y)

fxx2fxx' ::
    (ctx -> x -> ret)
    -> (ctx -> (y -> x) -> (y -> ret))
fxx2fxx' f ctx fct y = f ctx (fct y)


