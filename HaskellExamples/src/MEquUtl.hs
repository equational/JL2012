-----------------------------------------------------------------------------
--
-- Module      :  MEquUtl
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

module MEquUtl (
    VarDiff(..),
    GettableVar(..),
    VKey(..), VKeySet,
    VD(..),
    FreeVarEq(..),
    emptyVKeySet,
    singletonVKeySet,
    incVKeySet,
    decVKeySet,
    addVKeySet,
    subVKeySet,
    addVKeySets,
    sizeVKeySet,
    minKeyVKeySet
) where

import MEquSol
import Data.List (delete, union, map, find, elem, insert,foldr)
import Data.Either
import qualified Data.IntMap as IntMap
import Control.Comonad

-- import MMM
-- import MMMU

-- | Key-value or key-key pair operations to support the variable to value map.
class VarDiff vk vd vt where
    -- | "Opens" a pair and provides a tuplet of key and either value, or other key.
    getVarDiff::vd->(vk, Either vk vt)
    -- |  Bind equivalence between two keys.
    eqVar:: (vk,vk) -> vd
    -- |  Bind key to value.
    bindVar:: (vk,vt) -> vd
    -- | Test if pair refers to one of the provided keys.
    dependent:: vd -> [vk] -> Bool


-- | Check if something (a term) has a variable key, and returns corresponding value pair.
class GettableVar vk x vd where
    getVar::vk->x->Maybe(vd)


-- | Default key (not optimized)
data VKey = VKey Int deriving (Eq, Ord, Show)

-- | Default key map
type VKeySet = IntMap.IntMap Int

-- | Default value/key alternative
type VDValue vt = Either VKey vt
-- | Default key-value/key pair
type VD vt = (VKey, VDValue vt)

-- | Default operation on key pair.
instance VarDiff VKey (VD vt) vt where
    getVarDiff vd = vd
    eqVar (vk1, vk2) = (vk1, Left vk2)
    bindVar (vk, vt) = (vk, Right vt)
    dependent (k, _) kset = Data.List.elem k kset

-- | Default heap is a sorted list
instance (Ord m) => Heap [m] m where
    insert = Data.List.insert
    deCons(h:t) = Just (h,t)
    deCons([]) = Nothing
--    apply1 f (h:t) = f (Just (h,t))
--    apply1 f ([]) = f Nothing
    insertMany l1 l2 = Data.List.foldr MEquSol.insert l2 l1

-- | Internal operations needed for all equations
-- Here with 'Int' based key (should probably be move out an be polymorphic).
class FreeVarEq eq where
    freeVars:: eq -> VKeySet
    complexity:: eq->Int

-- | Default map is list of pairs
-- Adding an element is simply adding it a the head
instance SettableVar [VD vt] (VD vt) where
    setVar vd l = vd:l

-- | Default map is list of pairs
-- Getting an element is searching for it in the list.
instance GettableVar VKey [VD vt] (VD vt) where
    getVar vk l = find (\(vk',_) ->vk'==vk) l



-- This is code I was playing but I think I ran out of time to do anything.

emptyVKeySet::VKeySet
emptyVKeySet = IntMap.empty

singletonVKeySet::VKey -> VKeySet
singletonVKeySet (VKey vt) = IntMap.singleton vt 1

incVKeySet::VKey->VKeySet->VKeySet
incVKeySet (VKey vt) c = IntMap.adjust (\ x->x+1) vt c

decVKeySet::VKey->VKeySet->VKeySet
decVKeySet (VKey vt) c = IntMap.adjust (\ x->x-1) vt c

addVKeySet::VKeySet->VKeySet->VKeySet
addVKeySet c1 c2 =
    IntMap.unionWith (\ i1 i2 -> i1+i2) c1 c2

subVKeySet::VKeySet->VKeySet->VKeySet
subVKeySet c1 c2 =
    IntMap.unionWith (\ i1 i2 -> i1-i2) c1 c2

addVKeySets::[VKeySet]->VKeySet
addVKeySets c1 =
    IntMap.unionsWith (\ i1 i2 -> i1+i2) c1

sizeVKeySet::VKeySet->Int
sizeVKeySet c1 = IntMap.size c1

minKeyVKeySet::VKeySet->Maybe VKey
minKeyVKeySet c1 =
    (fmap (VKey . fst . fst) (IntMap.minViewWithKey c1))


data Pointed e = P Int (IntMap.IntMap e) deriving Show

instance Functor Pointed where
    fmap f (P k a) = P k (fmap f a)

instance Comonad Pointed where
    extract (P k a) = (IntMap.!) a k
    extend f (P k a) =
        let g k e = f (P k a)
        in P k $ IntMap.mapWithKey g a


class (Comonad w) => ComonadDx w a x where
    comoninc::x->(w a)->(w a)

instance ComonadDx Pointed a (Int,a) where
    comoninc (k,e) (P k1 a) = P k1 $ IntMap.insert k e a


emptyMap = ([],[])

applyF fl arg = Data.List.map (\ f -> f arg) fl

