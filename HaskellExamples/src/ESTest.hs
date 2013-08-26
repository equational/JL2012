-----------------------------------------------------------------------------
--
-- Module      :  ESTest
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | This is test code for thee equation solver (MEquSol and MEquUlt).
-- It defines  terms and the typeclasses that adapt to the equation solver.
-- There is small test (but you need to hand verify the correctness of the result).
-----------------------------------------------------------------------------

module ESTest (
    testEq1
) where

import MEquSol
import MEquUtl -- (VKey(..),VD,FreeVarEq(..), VarDiff(..))
import PrivateUtl(fixEither,fixEither2)
import Data.List(length, nub, null, map,foldl,concatMap, insertBy,foldr)
import Data.Ord
import Prelude

-- | A simple boolean term structure
data Term =
    Var VKey
  | Not Term
  | Const Bool
  | And [Term]
  | Or [Term]
  deriving (Eq, Ord, Show)

-- | Equations on terms
data Equ =
    Equ Term
  deriving Show

-- | Return possible values of a term: (false, false) no values, (true, false) might be true.
potValue:: Term -> (Bool,Bool)
potValue (Var _) = (True,True)
potValue (Not term) = (fv, tv) where
    (tv, fv) = potValue term
potValue (Const True) = (True, False)
potValue (Const False) = (False, True)
potValue (And terms) = foldl fadd (True, False) terms where
    fadd (t1,f1) term =
        let (t2, f2) = potValue term
        in (t1 && t2, f1 || f2)
potValue (Or terms) = foldl fadd (False, True) terms where
    fadd (t1,f1) term =
        let (t2, f2) = potValue term
        in (t1 || t2, f1 && f2)

-- | Equations that still need resolution
type EquHeap = [Equ]

-- | A "partial" solution is a list of variable assignments to terms
-- and equations that still need resolution.
type Sol = ([VD Term],EquHeap)

-- | The empty solution: no vars, no equations.
emptySol = ([],[])::Sol


instance  Equation Equ where
    -- | An equation is valid when it can still can be true.
    valid (Equ term) =
        case tv of
        True -> True
        False -> False
        where
        (tv, _) = potValue term

instance FreeVarEq Equ where
    -- | Collect, and counts, free variables of an equation
    freeVars (Equ term) = fv term where
        fv (Var vk) = singletonVKeySet vk
        fv (Not t) = fv t
        fv (Const _) = emptyVKeySet
        fv (And terms) = addVKeySets (Data.List.map fv terms)
        fv (Or terms) = addVKeySets (Data.List.map fv terms)
    -- | Simple complexity metric: the number of free variables of the equation.
    complexity eq =
        sizeVKeySet (freeVars eq)

instance SettableVar Equ (VD Term) where
    -- |
    setVar vd (Equ term) = Equ (rewrite' term) where
        rewrite' t@(Var vk) =
            case getVarDiff vd of
            (vk1, Left vk2) | (vk==vk1) -> (Var vk2)
            (vk1, Right vt) | (vk==vk1) -> vt
            _ -> t
        rewrite' (Not x) =
            Not (rewrite' x)
        rewrite' t@(Const _) = t
        rewrite' (And terms) = And (Data.List.map rewrite' terms)
        rewrite' (Or terms) = Or (Data.List.map rewrite' terms)
instance  EqSystem Equ (VD Term) where
    solveOne (Equ (Var fv)) = Just [bindVar (fv, (Const True))]
    solveOne (Equ (Not(Var fv))) = Just [bindVar (fv, (Const False))]
    solveOne eq = fmap simple (minKeyVKeySet (freeVars eq)) where
        simple fv =
            [bindVar (fv, (Const True)), bindVar (fv, (Const False))]

instance Heap Sol Equ where
    insert eq (m,l) = (m, Data.List.insertBy (\eq2 eq1 -> compare (complexity eq1) (complexity eq2)) eq l)
    insertMany eql sol = Data.List.foldr insert sol eql
    deCons (_,[]) = Nothing
    deCons (m,(h:t)) = Just (h,(m,t))

instance SettableVar [Equ] (VD Term) where
    setVar vd l = Data.List.map (setVar vd) l


instance SettableVar Sol (VD Term) where
    setVar vd (m,el) = (setVar vd m, setVar vd el)

type Space = [Sol]

instance Heap Space Sol where
    insert sol l = Data.List.insertBy (\(m2,_) (m1,_) -> compare (length m1) (length m2)) sol l
    insertMany eql sol = Data.List.foldr insert sol eql
    deCons [] = Nothing
    deCons (h:t) = Just (h,t)


dummyEqu = Equ (Const False)
dummyVD = (VKey 0,Left (VKey 0))::(VD Term)


instance Projectable Sol Equ (VD Term) where
    reduce vd sys = setVar vd sys


instance Solver Space Sol Equ (VD Term)

testEq1 =
    let meq =   emptySol
        -- 1 /\ 2
        m1 = insert (Equ (Or([Var (VKey 1),(Var (VKey 2))]))) meq
        -- ~1 /\ ~2
        m2 = insert (Equ (Or([Not(Var (VKey 1)),Not(Var (VKey 2))]))) m1
        -- 2 /\ 3
        m3 = insert (Equ (Or([Var (VKey 2),(Var (VKey 3))]))) m2
        -- 3 /\ ~4
        m4 = insert (Equ (Or([Not(Var (VKey 3)),Not(Var (VKey 4))]))) m3
    in  MEquSol.solve [m4] []

--testEq1 =
--    let meq =   emptySol
--        -- 1
--        m1 = insert (Equ (Var (VKey 1))) meq
--        -- ~1
--        m2 = insert (Equ (Not(Var (VKey 1)))) m1
--        -- 2 /\ 3
--        m3 = insert (Equ (Or([Var (VKey 2),(Var (VKey 3))]))) m2
--        -- 3 /\ ~4
--        m4 = insert (Equ (Or([Not(Var (VKey 3)),Not(Var (VKey 4))]))) m3
--    in  MEquSol.solve [m4] []


--
--
