-- Orignal code from:
--
-- Copyright (c) 2009 - 2010 Brendan Hickey - http://bhickey.net
-- New BSD License (see http://www.opensource.org/licenses/bsd-license.php)
--
-- | 'Data.Heap.Binary' provides a binary min-heap. Balance is maintained through descendant counting.

-- | Experimenting augmented heaps, each node has an additional payload which
-- depend only on the node's data and the node data "below it" in the tree.
-- The idea is that search operation can be run on the heap that use this augmentation
-- to accelerate the process.

module AHeap
(BinaryHeap(..), ANodeAugmentation(..), head, headAug, tail, merge, singleton, empty, null, fromList, toList, insert, choose, AHeap.map)
where

import Prelude hiding (head, tail, null)
import PrivateUtl

data BinaryHeap d a =
    Leaf
  | Node d !Int a (BinaryHeap d a) (BinaryHeap d a) -- deriving (Eq, Ord)

class ANodeAugmentation d a where
    mkAHeap :: (d, Int, BinaryHeap d a, BinaryHeap d a) -> a

instance (Ord d, Show d, Show a) => Show (BinaryHeap d a) where
  show Leaf = "Leaf"
  show (Node n _ a h1 h2) = "Node " ++ show n ++ " "  ++ show a ++ " (" ++ show h1 ++ " " ++ show h2 ++ ")"

rank :: (Ord n) => BinaryHeap n a -> Int
rank Leaf = 0
rank (Node _ d _ _ _) = d

-- | /O(1)/. 'empty' produces an empty heap.
empty :: (Ord a) => BinaryHeap a k
empty = Leaf

newNode :: (Ord d,  ANodeAugmentation d a) => d -> Int -> BinaryHeap d a -> BinaryHeap d a -> BinaryHeap d a
newNode d depth left right = Node d depth a left right
    where
        a = mkAHeap (d, depth, left, right)

-- | /O(1)/. 'singleton' consumes an element and constructs a singleton heap.
singleton :: (Ord d,  ANodeAugmentation d a) => d -> BinaryHeap d a
singleton d = newNode d 1 Leaf Leaf

-- | 'merge' consumes two binary heaps and merges them.
merge :: (Ord a,  ANodeAugmentation a k) => BinaryHeap a k -> BinaryHeap a k -> BinaryHeap a k
merge Leaf n = n
merge n Leaf = n
merge h1@(Node n1 d1 _ h1l h1r) h2@(Node n2 d2 _ _ _) =
  if  n1<n2 || (n1==n2 && d1<=d2)
  then if rank h1l < rank h1r
       then newNode n1 (d1 + d2) (merge h1l h2) h1r
       else newNode n1 (d1 + d2) h1l (merge h1r h2)
  else merge h2 h1

-- | /O(lg n)/.
insert :: (Ord d,  ANodeAugmentation d a) => d -> BinaryHeap d a -> BinaryHeap d a
insert a h = merge h (singleton a)

-- | /O(1)/.
null :: (Ord a) => BinaryHeap a k -> Bool
null Leaf = True
null _    = False

-- | /O(n lg n)/.
toList :: (Ord a,  Show a, Show k, ANodeAugmentation a k) => BinaryHeap a k -> [a]
toList Leaf = []
toList h@(Node _ _ _ _ _) =
    case head h of
    Just optH -> optH : toList (tail h)
    Nothing -> []

-- | /O(n)/. 'fromList' constructs a binary heap from an unsorted list.
fromList :: (Ord d,  ANodeAugmentation d a) => [d] -> BinaryHeap d a
fromList [] = Leaf
fromList l = mergeList (Prelude.map singleton l)
              where mergeList [a] = a
                    mergeList x = mergeList (mergePairs x)
                    mergePairs (a:b:c) = merge a b : mergePairs c
                    mergePairs x = x

-- | /O(1)/. 'head' returns the element root of the heap.
head :: (Ord a, Show a) => BinaryHeap a k -> (Maybe a)
head Leaf = mtr "head" (Nothing)
head (Node n _ _ _ _) = mtr "head" (Just n)

headAug :: (Ord d) => BinaryHeap d a -> (Maybe a)
headAug Leaf = Nothing
headAug (Node _ _ a _ _) = Just a


-- | /O(lg n)/. 'tail' discards the root of the heap and merges the subtrees.
tail :: (Ord a,  Show a, Show k, ANodeAugmentation a k) => BinaryHeap a k -> BinaryHeap a k
tail Leaf = error "Data.Heap empty list"
tail (Node _ _ _ h1 h2) = mtr "tail" (merge h1 h2)

choose
  :: (Ord d, ANodeAugmentation d a) =>
     (d -> Int -> a -> t -> (Ordering, t))
     -> t
     -> BinaryHeap d a
     -> (Maybe d, BinaryHeap d a)
choose selection ctx node = search ctx node
    where
    search ctx node =
        case node of
        Leaf -> (Nothing, node)
        Node key depth aug left right ->
            let (res, ctx) = selection key depth aug ctx
            in case res of
            LT ->
                case search ctx left of
               (Just d2, left2) ->
                    (Just d2, newNode key depth left2 right)
               (Nothing, _) -> (Nothing, node)
            GT ->
                case search ctx right of
                (Just d2, right2) ->
                    (Just d2, newNode key depth left right2)
                (Nothing, _) -> (Nothing, node)
            EQ ->
                    (Just key, merge left right)

map f node = map' node where
    map' Leaf = Leaf
    map' (Node d depth aug left right) =
        let left' = map' left
            right' = map' right
        in case f d of
            Just d' -> pivot d' depth left' right'
            Nothing -> merge left' right'

-- how to bring the pivot into a single function with no singletons?

pivot d _ Leaf Leaf = singleton d
pivot d _ Leaf node = merge (singleton d) node
pivot d _ node Leaf = merge node (singleton d)
pivot
    d
    depth
    node1@(Node d1 depth1 _ left1 right1)
    node2@(Node d2 depth2 _ left2 right2) =
    let pivotLeft() =
            newNode d1 depth (pivot d depth1 left1 right1) node2
        pivotRight() =
            newNode d2 depth node1 (pivot d depth2 left2 right2)
    in case ((d >= d1), (d >= d2)) of
        (True, True) -> newNode d depth node1 node2
        (False, True) -> pivotLeft()
        (True, False) -> pivotRight()
        (False, False) ->
            if (d1 == d2) then
                if depth1 > depth2 then
                    pivotLeft()
                else
                    pivotRight()
            else
                if d1 > d2 then
                    pivotLeft()
                else
                    pivotRight()


