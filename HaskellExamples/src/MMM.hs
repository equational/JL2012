-----------------------------------------------------------------------------
--
-- Module      :  MMM
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | F# state monad extended to delay logical operators.
-- | Code was translated but nothing else. So it does not use the monad classtype.
-----------------------------------------------------------------------------

module MMM (
    StateR(..), RetState(..), Err(..),
    toState, applyState, mapState,
    runState, bind, ret, (>>=), (<<=),
    toOrState, fail
) where

import Prelude hiding ((>>=),fail)
import HighOrder

(+++) s1 s2 = s1 ++ " " ++ s2

data
    StateR err stateIn stateOut ret =
         StateRM    (stateIn -> (RetState err stateOut ret))
        | StateOr   (StateR err stateIn stateOut ret) (StateR err stateIn stateOut ret)
        | StateAnd  (StateR err stateIn stateOut ret) (StateR err stateIn stateOut ret) ((ret, stateOut)->(ret, stateOut)->(ret, stateOut))
        | StateOpt  (StateR err stateIn stateOut ret) (StateR err stateIn stateOut ret) ((ret, stateOut)->(ret, stateOut)->(ret, stateOut))

instance (Show err, Show stateIn, Show stateOut, Show ret) =>
    Show (StateR err stateIn stateOut ret) where
    show (StateRM _) = "StateRM"
    show (StateOr op1 op2) = (show op1) +++ (show op2)
    show (StateAnd op1 op2 _) = (show op1) +++ (show op2)
    show (StateOpt op1 op2 _) = (show op1) +++ (show op2)


data
    RetState err state ret =
          Success   (ret, state)
        | Fail      (Err err)

instance (Show err, Show state, Show ret) =>
    Show (RetState err state ret) where
    show (Success(ret,state)) = (show ret) +++ (show state)
    show (Fail err) = show err

data
    Err err =
          ErrMsg err
        | ErrOr  (Err err) (Err err)
        | ErrAnd (Err err) (Err err)
        | ErrTag err (Err err)

instance (Show err) =>
    Show (Err err) where
    show (ErrMsg err) = (show err)
    show (ErrOr  e1 e2) = (show e1) +++ (show e2)
    show (ErrAnd e1 e2) = (show e1) +++ (show e2)
    show (ErrTag tag e) = show(tag)+++show(e)

type StateM err state ret = StateR err state state ret

toState::
    (stateIn -> RetState err stateOut ret) -> StateR err stateIn stateOut ret
toState f = StateRM f

toOrState
  :: StateR err stateIn stateOut ret
     -> StateR err stateIn stateOut ret
     -> StateR err stateIn stateOut ret
toOrState f1 f2 = StateOr f1 f2

applyState :: (stateOut -> ret) -> StateR err stateOut stateOut ret
applyState f =  toState (\s -> (Success (f s, s)))

mapState :: (stateIn -> stateOut) -> StateR err stateIn stateOut ()
mapState f =  toState (\ s -> Success ((), f s))

foldState ::
    (stateIn -> RetState err stateOut ret)
    -> StateR err stateIn stateOut ret
foldState f =  toState (\ s -> f s)

runState ::
    StateR err stateIn stateOut ret -> stateIn -> RetState err stateOut ret
runState m s =
    case m of
    StateRM f ->
        f s
    StateOr f1 f2  ->
        let r = runState f1 s
        in case r of
        Success _ ->
            r  -- Success so return this one
        Fail es1 ->
            let r = runState f2 s    -- Failed so skip and try next
            in case r of
            Success _ ->
                r
            Fail es2 ->
                Fail (ErrOr es1 es2)
    StateAnd f1 f2 mergeStates ->
        let r = runState f1 s
        in case r of
        Success (r1,s1) ->
            let r = runState f2 s    -- Failed so skip and try next
            in case r of
            Success (r2,s2) ->
                Success (mergeStates (r1,s1) (r2,s2))
            Fail es2 ->
                Fail es2
        Fail es1 ->
            Fail es1
    StateOpt f1 f2 mergeStates  ->
        let r = runState f1 s
        in case r of
        Success (r1,s1) ->
            let r = runState f2 s    -- Failed so skip and try next
            in case r of
            Success (r2,s2) ->
                Success (mergeStates (r1,s1) (r2,s2))
            Fail es2 ->
                Success (r1,s1)
        Fail es1 ->
            let r = runState f2 s    -- Failed so skip and try next
            in case r of
            Success (r2,s2) ->
                Success (r2,s2)
            Fail es2 ->
                Fail (ErrOr es1 es2)
    -- split and merge
    -- switch
fail::
    err -> StateR err stateIn stateOut ret
fail err = toState (\ s -> Fail (ErrMsg err))

ignoreState m = mapSuccess (\ _ -> ()) m

ret::
    ret -> StateR err state state ret
ret x = toState (\ s-> Success (x, s))

retFx::
    (t -> ret)
    -> (t -> StateR err state state ret)
retFx f = \ x -> toState (\ s-> Success (f x, s))

retFxy::
    (x -> y -> ret)
    -> (x -> y -> StateR err state state ret)
retFxy f = \ x y -> toState (\ s-> Success (f x y, s))

--mx'2mm' f m =
--
--mapSuccess f m = toState (\ s ->
--    case (runState m) s of
--    Success (r,s) -> Success (f r, s)
--    Fail es -> Fail es
--    )
--
--mapMSuccess f m = toState (\ s ->
--    case (runState m) s of
--    Success (r,s) ->
--        case runState (f r) s of
--        vs'@(Success (r', s')) ->
--            vs'
--        Fail es' -> Fail es'
--    Fail es -> Fail es
--    )


bind::
     StateR err stateIn stateOut ret1
     -> (ret1 -> StateR err stateOut stateOut2 ret)
     -> StateR err stateIn stateOut2 ret
bind m f = toState (\ s ->
    case runState m s of
    Success (v,s')->
        let n = f v
        in runState n s'
    Fail err  ->
        Fail err)

(>>=) = bind
(<<=) x y = bind y x

zero() = toState (\ s -> Success ((),s))
combine m1 m2 = bind m1 (\() -> m2)

mapFailure::
    (Err err1 -> Err err)
    -> StateR err1 stateIn stateOut ret
    -> StateR err stateIn stateOut ret
mapFailure f m = toState (\ s ->
    case (runState m) s of
    Success (r,s) -> Success (r,s)
    Fail es -> Fail (f es)
    )

mapFailure'::
    (Err err1 -> Err err)
    -> (t -> StateR err1 stateIn stateOut ret)
    -> (t -> StateR err stateIn stateOut ret)
mapFailure' = fxx2fxx' mapFailure


mapSuccess::
    (ret1 -> ret)
    -> StateR err stateIn stateOut ret1
    -> StateR err stateIn stateOut ret
mapSuccess f m = toState (\ s ->
    case (runState m) s of
    Success (r,s) -> Success (f r, s)
    Fail es -> Fail es
    )

mapSuccess'::
    (ret1 -> ret)
    -> (y -> StateR err stateIn stateOut ret1)
    -> (y -> StateR err stateIn stateOut ret)
mapSuccess' = fxx2fxx' mapSuccess


mapMSuccess::
    (ret1 -> StateR err stateIn1 stateOut ret)
     -> StateR err stateIn stateIn1 ret1
     -> StateR err stateIn stateOut ret
mapMSuccess f m = toState (\ s ->
    case (runState m) s of
    Success (r,s) ->
        case runState (f r) s of
        vs'@(Success (r', s')) ->
            vs'
        Fail es' -> Fail es'
    Fail es -> Fail es
    )

mapMSuccess' = fxx2fxx' mapMSuccess

(|>!) :: StateR err state state ret -> (Err err -> Err err) -> StateR err state state ret
(|>!) m f = mapFailure f m

(|>!^)
  :: StateR err1 stateIn stateOut ret
     -> (Err err1 -> Err err)
     -> StateR err stateIn stateOut ret
(|>!^) m f = mapFailure f m

(|>%)
  :: StateR err stateIn stateOut ret1
     -> (ret1 -> ret)
     -> StateR err stateIn stateOut ret
(|>%) m f = mapSuccess f m

(|>%%)
  :: StateR err stateIn stateIn1 ret1
     -> (ret1 -> StateR err stateIn1 stateOut ret)
     -> StateR err stateIn stateOut ret
(|>%%) m f = mapMSuccess f m

(|>%^)
  :: (y -> StateR err stateIn stateOut ret1)
     -> (ret1 -> ret)
     -> y
     -> StateR err stateIn stateOut ret
(|>%^) m f = mapSuccess' f m

(|>%%^) m f = mapMSuccess' f m
(|>%%^)
  :: (y -> StateR err stateIn stateIn1 ret1)
     -> (ret1 -> StateR err stateIn1 stateOut ret)
     -> (y -> StateR err stateIn stateOut ret)


