module LRBTree
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// Inspired by https://gist.github.com/neoGeneva/1056026/raw/755d8420487872a9627c1700ad51ff399071f7f9/RedBlackTree.fsx

// Red and Black tree to test the joins and monadic constructions.

open L2Std
open LState
    
type Color = Red | Black

type 
    Tree'<'a> = { Color: Color; LTree: Tree<'a>; Value: 'a; RTree: Tree<'a>; }
and
    Tree<'a> = Tree'<'a> option

let update (takeOp:'a->Take1) (op :option<'a> -> ('r*'a))  (tree : Tree<'a>) :('r*Tree<'a>)=
    let makeTreeBlack tree = 
        match tree with
        | None -> None
        | Some t -> Some { t with Color = Color.Black }

    let balance tree : Tree<'a> =
        match tree with
        | Some { Color = Color.Black; LTree = Some { Color = Color.Red; LTree = Some { Color = Color.Red; LTree = a; Value = x; RTree = b }; Value = y; RTree = c }; Value = z; RTree = d }
        | Some { Color = Color.Black; LTree = Some { Color = Color.Red; LTree = a; Value = x; RTree = Some { Color = Color.Red; LTree = b; Value = y; RTree = c } }; Value = z; RTree = d }
        | Some { Color = Color.Black; LTree = a; Value = x; RTree = Some { Color = Color.Red; LTree = Some { Color = Color.Red; LTree = b; Value = y; RTree = c }; Value = z; RTree = d } }
        | Some { Color = Color.Black; LTree = a; Value = x; RTree = Some { Color = Color.Red; LTree = b; Value = y; RTree = Some { Color = Color.Red; LTree = c; Value = z; RTree = d } } } -> 
            Some { Color = Color.Red; LTree = Some { Color = Color.Black; LTree = a; Value = x; RTree = b }; Value = y; RTree = Some { Color = Color.Black; LTree = c; Value = z; RTree  = d; } }
        | _ -> tree

    let rec insert' (tree : Tree<'a>) =
        match tree with
        | None ->
            let (r, value) =  op None
            (r, Some { Color = Color.Red; LTree = None; Value = value; RTree = None })
        | Some t ->
            let take = takeOp t.Value
            if take.take1Left then
                let (r, tree) =  insert' t.LTree
                (r, balance(Some { t with LTree = tree }))
            else if take.take1Right then
                let (r, tree) = insert' t.RTree
                (r, balance(Some { t with RTree = tree }))
            else if take.take1This then
                let (r, value) = op (Some t.Value)
                (r, Some { t with Value = value })
            else
                let (r, tree) = insert' t.LTree
                (r, balance(Some { t with LTree = tree }))
    let (r, tree) = insert' tree
    (r, makeTreeBlack tree)

let insert (takeOp:'a->Take1) (value :'a)  (tree : Tree<'a>) :Tree<'a> =
    let (_, newTree) = update takeOp (fun None -> ((), value)) tree
    newTree

let tryFindElement takeOp (tree : Tree<'a>)  =
    let rec insert' tree =
        match tree with
        | None -> None
        | Some t ->
            let take = takeOp t.Value
            if take.take1Left then 
                insert' t.LTree
            else if take.take1Right then
                insert' t.RTree
            else if take.take1This then
                Some t.Value
            else
                None
    insert' tree

let mmap takeOp mf (tree : Tree<'a>) =
    let rec map' tree =
        match tree with
        | Some t ->
            let take = takeOp t.Value
            mcond1 take.take1Left (map' t.LTree)
                (mcond1 take.take1This (mf t.Value)
                    (mcond1 take.take1Right (map' t.RTree)
                        (ret ())))
        | None ->
            ret ()
    map' tree

let mfold takeOp mf (tree : Tree<'a>) =
    let rec map' tree =
        match tree with
        | None ->
            ret
        | Some t ->
            let take = takeOp t.Value
            mcfold1 take.take1Left (map' t.LTree) 
            ^<| mcfold1 take.take1This (mf t.Value) 
            ^<| mcfold1 take.take1Right (map' t.RTree) 
            ^<| ret
    map' tree

let mfold2 take2Op mf (tree1 : Tree<'a>) (tree2 : Tree<'b>)=
    let rec map' tree1 tree2 =
        match tree1, tree2 with
        | Some t1, Some t2 ->
            let take = take2Op t1.Value t2.Value
            mcfold1     take.take2LeftLeft  (map' t1.LTree  t2.LTree) 
            ^<| mcfold1 take.take2LeftThis  (map' t1.LTree  tree2) 
            ^<| mcfold1 take.take2LeftRight (map' t1.LTree  t2.RTree) 
            ^<| mcfold1 take.take2ThisLeft  (map' tree1     t2.LTree) 
            ^<| mcfold1 take.take2ThisThis  (mf t1.Value    t2.Value) 
            ^<| mcfold1 take.take2ThisRight (map' tree1     t2.RTree)
            ^<| mcfold1 take.take2RightLeft (map' t1.RTree  t2.LTree) 
            ^<| mcfold1 take.take2RightThis (map' t1.RTree  tree2) 
            ^<| mcfold1 take.take2RightRight (map' t1.RTree t2.RTree) 
            ^<| ret
        | None, _ ->
            ret
        | _, None ->
            ret
    map' tree1 tree2

(*
        take2LeftLeft: bool;
        take2LeftThis: bool;
        take2LeftRight: bool;
        take2ThisLeft: bool;
        take2ThisThis: bool;
        take2ThisRight: bool;
        take2RightLeft: bool;
        take2RightThis: bool;
        take2RightRight: bool;

*)