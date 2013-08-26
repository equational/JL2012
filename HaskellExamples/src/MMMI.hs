-----------------------------------------------------------------------------
--
-- Module      :  MMMI
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

module MMMI (

) where


import MMM
import Prelude -- hiding ((>>=))


adapt'::
        (stateIn2 -> stateIn1)
     -> (stateIn2 -> stateOut1 -> stateOut2)
     -> (Err err1 -> Err err2)
     -> (t -> StateR err1 stateIn1 stateOut1 ret)
     -> (t -> StateR err2 stateIn2 stateOut2 ret)

adapt' get set err m' = \ a -> toState (\ s ->
    case (runState (m' a)) (get s) of
    Success (r,s') -> Success (r, set s s')
    Fail es -> Fail (err es)
    )


adapt::
        (stateIn2 -> stateIn1)
     -> (stateIn2 -> stateOut1 -> stateOut2)
     -> (Err err1 -> Err err2)
     -> StateR err1 stateIn1 stateOut1 ret
     -> StateR err2 stateIn2 stateOut2 ret
adapt get set err m = toState (\ s ->
    case (runState m) (get s) of
    Success (r,s') -> Success (r, set s s')
    Fail es -> Fail (err es)
    )


map1M':: (t -> StateR err stateIn stateOut ret)
     ->  (t -> StateR err (stateIn, t) (stateOut, t) ret)
map1M' m' = adapt' fst (\ (_,s) s' -> (s',s)) id m'

map1M:: StateR err stateIn stateOut ret
     -> StateR err (stateIn, t) (stateOut, t) ret
map1M  m  = adapt  fst (\ (_,s) s' -> (s',s)) id m

map2M'::
        (t -> StateR err2 stateIn1 t2 ret)
     -> (t -> StateR err2 (t1, stateIn1) (t1, t2) ret)
map2M' m' = adapt' snd (\ (s,_) s' -> (s,s')) id m'

map2M:: StateR err stateIn t1 ret
     -> StateR err (t, stateIn) (t, t1) ret
map2M  m  = adapt  snd (\ (s,_) s' -> (s,s')) id m

pairM':: t1
     -> (t -> StateR err2 (t1, t2) (t3, stateOut2) ret)
     -> (t -> StateR err2 t2 stateOut2 ret)
pairM' t m' = adapt' (\ s -> (t, s)) (\ s (t,s') -> s') id m'

pairM:: t
     -> StateR err (t, t1) (t2, stateOut) ret
     -> StateR err t1 stateOut ret
pairM  t m  = adapt  (\ s -> (t, s)) (\ s (t,s') -> s') id m


composeAccessor1M::
    (((a, b) -> a) -> ((t1 -> t2) -> (t1, t3) -> (t2, t3)) -> t)
    -> t
composeAccessor1M compose = compose fst (\m (x, y) -> (m x, y))

composeAccessor2M::
    (((a, b) -> b) -> ((t1 -> t3) -> (t2, t1) -> (t2, t3)) -> t)
    -> t
composeAccessor2M compose = compose snd (\m (x, y) -> (x, m y))


recurse1::
    (StateR err stateIn stateOut ret -> StateR err stateIn stateOut ret)
    -> StateR err stateIn stateOut ret
recurse1 m = m' where
    recurse' s = runState (m m') s
    m' = toState recurse'

recurse2
  :: ((StateR err stateIn1 stateOut ret,
       StateR err1 stateIn stateOut1 ret1)
      -> StateR err stateIn1 stateOut ret,
      (StateR err stateIn1 stateOut ret,
       StateR err1 stateIn stateOut1 ret1)
      -> StateR err1 stateIn stateOut1 ret1)
     -> (StateR err stateIn1 stateOut ret,
         StateR err1 stateIn stateOut1 ret1)
recurse2 ms = ms' where
    (m0,m1) = ms
    recurse1_0 s = runState (m0 ms') s
    recurse1_1 s = runState (m1 ms') s
    ms' = (toState recurse1_0, toState recurse1_1)

recurse3
  :: ((StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn stateOut2 ret2)
      -> StateR err stateIn1 stateOut ret,
      (StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn stateOut2 ret2)
      -> StateR err1 stateIn2 stateOut1 ret1,
      (StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn stateOut2 ret2)
      -> StateR err2 stateIn stateOut2 ret2)
     -> (StateR err stateIn1 stateOut ret,
         StateR err1 stateIn2 stateOut1 ret1,
         StateR err2 stateIn stateOut2 ret2)
recurse3 ms = ms' where
    (m0,m1,m2) = ms
    recurse1_0 s = runState (m0 ms') s
    recurse1_1 s = runState (m1 ms') s
    recurse1_2 s = runState (m2 ms') s
    ms' = (toState recurse1_0, toState recurse1_1, toState recurse1_2)

recurse4
  :: ((StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn3 stateOut2 ret2,
       StateR err3 stateIn stateOut3 ret3)
      -> StateR err stateIn1 stateOut ret,
      (StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn3 stateOut2 ret2,
       StateR err3 stateIn stateOut3 ret3)
      -> StateR err1 stateIn2 stateOut1 ret1,
      (StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn3 stateOut2 ret2,
       StateR err3 stateIn stateOut3 ret3)
      -> StateR err2 stateIn3 stateOut2 ret2,
      (StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn3 stateOut2 ret2,
       StateR err3 stateIn stateOut3 ret3)
      -> StateR err3 stateIn stateOut3 ret3)
     -> (StateR err stateIn1 stateOut ret,
         StateR err1 stateIn2 stateOut1 ret1,
         StateR err2 stateIn3 stateOut2 ret2,
         StateR err3 stateIn stateOut3 ret3)
recurse4 ms = ms' where
    (m0,m1,m2,m3) = ms
    recurse1_0 s = runState (m0 ms') s
    recurse1_1 s = runState (m1 ms') s
    recurse1_2 s = runState (m2 ms') s
    recurse1_3 s = runState (m3 ms') s
    ms' = (toState recurse1_0, toState recurse1_1, toState recurse1_2, toState recurse1_3)

recurse5
  :: ((StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn3 stateOut2 ret2,
       StateR err3 stateIn4 stateOut3 ret3,
       StateR err4 stateIn stateOut4 ret4)
      -> StateR err stateIn1 stateOut ret,
      (StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn3 stateOut2 ret2,
       StateR err3 stateIn4 stateOut3 ret3,
       StateR err4 stateIn stateOut4 ret4)
      -> StateR err1 stateIn2 stateOut1 ret1,
      (StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn3 stateOut2 ret2,
       StateR err3 stateIn4 stateOut3 ret3,
       StateR err4 stateIn stateOut4 ret4)
      -> StateR err2 stateIn3 stateOut2 ret2,
      (StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn3 stateOut2 ret2,
       StateR err3 stateIn4 stateOut3 ret3,
       StateR err4 stateIn stateOut4 ret4)
      -> StateR err3 stateIn4 stateOut3 ret3,
      (StateR err stateIn1 stateOut ret,
       StateR err1 stateIn2 stateOut1 ret1,
       StateR err2 stateIn3 stateOut2 ret2,
       StateR err3 stateIn4 stateOut3 ret3,
       StateR err4 stateIn stateOut4 ret4)
      -> StateR err4 stateIn stateOut4 ret4)
     -> (StateR err stateIn1 stateOut ret,
         StateR err1 stateIn2 stateOut1 ret1,
         StateR err2 stateIn3 stateOut2 ret2,
         StateR err3 stateIn4 stateOut3 ret3,
         StateR err4 stateIn stateOut4 ret4)
recurse5 ms = ms' where
    (m0,m1,m2,m3,m4) = ms
    recurse1_0 s = runState (m0 ms') s
    recurse1_1 s = runState (m1 ms') s
    recurse1_2 s = runState (m2 ms') s
    recurse1_3 s = runState (m3 ms') s
    recurse1_4 s = runState (m4 ms') s
    ms' = (toState recurse1_0, toState recurse1_1, toState recurse1_2, toState recurse1_3, toState recurse1_4)

recurse1'
  :: ((t -> StateR err stateIn stateOut ret)
      -> t
      -> StateR err stateIn stateOut ret)
     -> t
     -> StateR err stateIn stateOut ret
recurse1' ms = ms' where
    recurse1_0 = \ x -> toState (\ s -> runState (ms ms' x) s)
    ms' = recurse1_0

recurse2'::
    ((t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t -> StateR err stateIn stateOut ret)
      -> t1
      -> StateR err1 stateIn1 stateOut1 ret1,
      (t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t -> StateR err stateIn stateOut ret)
      -> t
      -> StateR err stateIn stateOut ret)
     -> (t1 -> StateR err1 stateIn1 stateOut1 ret1,
         t -> StateR err stateIn stateOut ret)
recurse2' ms = ms' where
    (m0,m1) = ms
    recurse1_0 = \ x -> toState (\ s -> runState (m0 ms' x) s)
    recurse1_1 = \ x -> toState (\ s -> runState (m1 ms' x) s)
    ms' = (recurse1_0, recurse1_1)

recurse3'::
     ((t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t -> StateR err stateIn stateOut ret)
      -> t1
      -> StateR err1 stateIn1 stateOut1 ret1,
      (t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t -> StateR err stateIn stateOut ret)
      -> t2
      -> StateR err2 stateIn2 stateOut2 ret2,
      (t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t -> StateR err stateIn stateOut ret)
      -> t
      -> StateR err stateIn stateOut ret)
     -> (t1 -> StateR err1 stateIn1 stateOut1 ret1,
         t2 -> StateR err2 stateIn2 stateOut2 ret2,
         t -> StateR err stateIn stateOut ret)
recurse3' ms = ms' where
    (m0,m1,m2) = ms
    recurse1_0 = \ x -> toState (\ s -> runState (m0 ms' x) s)
    recurse1_1 = \ x -> toState (\ s -> runState (m1 ms' x) s)
    recurse1_2 = \ x -> toState (\ s -> runState (m2 ms' x) s)
    ms' = (recurse1_0, recurse1_1, recurse1_2)

recurse4'
  :: ((t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t3 -> StateR err3 stateIn3 stateOut3 ret3,
       t -> StateR err stateIn stateOut ret)
      -> t1
      -> StateR err1 stateIn1 stateOut1 ret1,
      (t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t3 -> StateR err3 stateIn3 stateOut3 ret3,
       t -> StateR err stateIn stateOut ret)
      -> t2
      -> StateR err2 stateIn2 stateOut2 ret2,
      (t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t3 -> StateR err3 stateIn3 stateOut3 ret3,
       t -> StateR err stateIn stateOut ret)
      -> t3
      -> StateR err3 stateIn3 stateOut3 ret3,
      (t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t3 -> StateR err3 stateIn3 stateOut3 ret3,
       t -> StateR err stateIn stateOut ret)
      -> t
      -> StateR err stateIn stateOut ret)
     -> (t1 -> StateR err1 stateIn1 stateOut1 ret1,
         t2 -> StateR err2 stateIn2 stateOut2 ret2,
         t3 -> StateR err3 stateIn3 stateOut3 ret3,
         t -> StateR err stateIn stateOut ret)
recurse4' ms = ms' where
    (m0,m1,m2,m3) = ms
    recurse1_0 = \ x -> toState (\ s -> runState (m0 ms' x) s)
    recurse1_1 = \ x -> toState (\ s -> runState (m1 ms' x) s)
    recurse1_2 = \ x -> toState (\ s -> runState (m2 ms' x) s)
    recurse1_3 = \ x -> toState (\ s -> runState (m3 ms' x) s)
    ms' = (recurse1_0, recurse1_1, recurse1_2, recurse1_3)

recurse5'
  :: ((t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t3 -> StateR err3 stateIn3 stateOut3 ret3,
       t4 -> StateR err4 stateIn4 stateOut4 ret4,
       t -> StateR err stateIn stateOut ret)
      -> t1
      -> StateR err1 stateIn1 stateOut1 ret1,
      (t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t3 -> StateR err3 stateIn3 stateOut3 ret3,
       t4 -> StateR err4 stateIn4 stateOut4 ret4,
       t -> StateR err stateIn stateOut ret)
      -> t2
      -> StateR err2 stateIn2 stateOut2 ret2,
      (t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t3 -> StateR err3 stateIn3 stateOut3 ret3,
       t4 -> StateR err4 stateIn4 stateOut4 ret4,
       t -> StateR err stateIn stateOut ret)
      -> t3
      -> StateR err3 stateIn3 stateOut3 ret3,
      (t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t3 -> StateR err3 stateIn3 stateOut3 ret3,
       t4 -> StateR err4 stateIn4 stateOut4 ret4,
       t -> StateR err stateIn stateOut ret)
      -> t4
      -> StateR err4 stateIn4 stateOut4 ret4,
      (t1 -> StateR err1 stateIn1 stateOut1 ret1,
       t2 -> StateR err2 stateIn2 stateOut2 ret2,
       t3 -> StateR err3 stateIn3 stateOut3 ret3,
       t4 -> StateR err4 stateIn4 stateOut4 ret4,
       t -> StateR err stateIn stateOut ret)
      -> t
      -> StateR err stateIn stateOut ret)
     -> (t1 -> StateR err1 stateIn1 stateOut1 ret1,
         t2 -> StateR err2 stateIn2 stateOut2 ret2,
         t3 -> StateR err3 stateIn3 stateOut3 ret3,
         t4 -> StateR err4 stateIn4 stateOut4 ret4,
         t -> StateR err stateIn stateOut ret)
recurse5' ms = ms' where
    (m0,m1,m2,m3,m4) = ms
    recurse1_0 = \ x -> toState (\ s -> runState (m0 ms' x) s)
    recurse1_1 = \ x -> toState (\ s -> runState (m1 ms' x) s)
    recurse1_2 = \ x -> toState (\ s -> runState (m2 ms' x) s)
    recurse1_3 = \ x -> toState (\ s -> runState (m3 ms' x) s)
    recurse1_4 = \ x -> toState (\ s -> runState (m4 ms' x) s)
    ms' = (recurse1_0, recurse1_1, recurse1_2, recurse1_3, recurse1_4)

seq2 :: Monad m => m t -> m t1 -> m (t, t1)
seq2 p1 p2 =
    do
        v1 <- p1
        v2 <- p2
        return (v1, v2)


seq3 :: Monad m => m t -> m t1 -> m t2 -> m (t, t1, t2)
seq3 p1 p2 p3 =
    do
        v1 <- p1
        v2 <- p2
        v3 <- p3
        return (v1, v2, v3)

seq4 :: Monad m => m t -> m t1 -> m t2 -> m t3 -> m (t, t1, t2, t3)
seq4 p1 p2 p3 p4 =
    do
        v1 <- p1
        v2 <- p2
        v3 <- p3
        v4 <- p4
        return (v1, v2, v3, v4)



