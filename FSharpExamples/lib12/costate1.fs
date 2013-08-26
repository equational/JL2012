module costate1
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Comonad educational material for a meetup meeting

type Tree<'a> =
    |   TreeNode of Tree<'a>*'a*Tree<'a>
    |   TreeNil


type  PathEntry =
    | Left1
    | Right1


type Path = list<PathEntry>


let rec optGetTree1 path tree = 
    match path, tree with
    | [], TreeNode(_, value, _) ->
        Some value
    | Left1::nPath, TreeNode(left, _, _) ->
        optGetTree1 nPath left
    | Right1::nPath, TreeNode(_, _, right) ->
        optGetTree1 nPath right
    | _, _ -> None


let mapTree1 g tree =
    let rec mapTree' path t =
        match t with
        | TreeNode (left, value, right) ->
            let newValue = g tree path value
            let newLeft = mapTree' (Left1::path) left
            let newRight = mapTree' (Right1::path) right
            TreeNode(newLeft, newValue, newRight)
        | TreeNil ->
            TreeNil
    mapTree' [] tree

let optUpRight path = 
    match List.rev(path) with
    | h::t  -> Some(List.rev(Right1::t))
    | []    -> None

let g1 tree path value = 
    match optUpRight path with 
    | Some opath ->
        match optGetTree1 opath tree with
        | Some ovalue ->
            Some(value-ovalue)
        | None ->
            None
    | None ->
        None

let test1 tree =
    mapTree1 g1 tree

let testtree = //TreeNode(TreeNode(TreeNode(TreeNil,1,TreeNil),2,TreeNil),3,TreeNode(TreeNil,4,TreeNode(TreeNil,5,TreeNil)))        
    TreeNode(
        TreeNode(
            TreeNode(
                TreeNil,
            1,
                TreeNil),
        2,
            TreeNil),
    3,
        TreeNode(
            TreeNil,
        4,
            TreeNode(
                TreeNil,
            5,
            TreeNil)))        


//type Tree<'a> =
//    |   TreeNode of Tree<'a>*'a*Tree<'a>
//    |   TreeNil


type  TreeZipper<'a> =
    | LeftHole  of  'a*Tree<'a>
    | RightHole of  'a*Tree<'a>

type TreeFocus<'a> = List<TreeZipper<'a>>*Tree<'a>

let optValue1 (_,tree) =
    match tree with
    | TreeNode(r,v,l) ->
        Some v
    | _ ->
        None

let optLeft1 ((path, tree):TreeFocus<_>) =
    match tree with
    | TreeNode(r,v,l) ->
        Some (LeftHole(v, r)::path, l)
    | _ ->
        None

let optRight1 (path, tree) =
    match tree with
    | TreeNode(r,v,l) ->
        Some (RightHole(v, l)::path, r)
    | _ ->
        None

let optUp1 (path, tree) =
    match path with
    | h::t ->
        match h with
        | LeftHole(v,r) ->
            Some(t, TreeNode(tree,v,r))
        | RightHole(v,l) ->
            Some(t, TreeNode(l,v,tree))
        | _ ->
            None
    | _ ->
        None

let mapTree2 g tree =
    let rec mapTree' focus =
        match focus with
        | zippers, TreeNode (left, value, right) ->
            let newValue = g focus
            let newLeft = mapTree' ((LeftHole(value,right))::zippers, left)
            let newRight = mapTree' ((RightHole(value,left))::zippers, right)
            TreeNode(newLeft, newValue, newRight)
        | _, TreeNil ->
            TreeNil
    mapTree' ([], tree)

let (>>?) a f = Option.bind f a

let g2 (focus:TreeFocus<_>) = 
    let optV = optValue1 focus
    let optOther = (optUp1 focus) >>? optLeft1 >>? optValue1
    optV >>? (fun x -> (optOther >>? (fun y -> Some (x-y))))   

let test2 tree = mapTree2 g2 tree

/// state monad "wrapped up in a type
type SM<'s,'r> = SM of ('s -> 'r*'s)

/// takes function out of type and runs it on given state
let run m state = 
    match m with
    | SM f -> f state

/// stateful maybe monad bind
let bindOpt m g =
    SM(fun state->
    let (rf, state2) = run m state
    match rf with
    | Some rfv ->
        let (rg, state3) = run (g rfv) state2
        match rg with
        | Some _ ->
            (rg, state3)
        | None ->
            (None, state)
    | None ->
        (None, state))

let (>>=) m g = bindOpt m g

let getState' f =  SM (fun s -> (Some (f s), s))
let mapState' f = SM (fun s -> (Some (), f s))
let someValue optX = SM (fun s-> (optX, s))
let fail = SM (fun s -> (None, s))
let ret x = SM (fun s -> (Some x,s))

let seqOr m1 m2 =
    SM(fun state->
    let (r1, state2) = run m1 state
    match r1 with
    | Some rfv ->
        (r1, state2)
    | None ->
        run m2 state)

let someOpt m =
    SM(fun state->
    let (r, state2) = run m state
    match r with
    | Some rfv ->
        (Some r, state2)
    | None ->
        (Some None, state))

let (>>|) m1 m2 = seqOr m1 m2


/// stateful maybe monad
type OptState() =
    member inline b.Bind (m, f) =  bindOpt m f
    member inline b.Combine(m1, m2) = bindOpt m1 (fun _ ->m2)
    member inline b.Return x = ret x
    member inline b.ReturnFrom m = m
//    member inline b.Zero() = zero

/// global instance, useful for silly F# do notation syntax.
let optState = OptState()


let toValue (_,tree) =
    match tree with
    | TreeNode(r,v,l) ->
        ret v
    | _ ->
        fail

let toLeft (path, tree) =
    match tree with
    | TreeNode(r,v,l) ->
        ret (LeftHole(v, r)::path, l)
    | _ ->
        fail

let toRight (path, tree) =
    match tree with
    | TreeNode(r,v,l) ->
        ret (RightHole(v, l)::path, r)
    | _ ->
        fail

let toUp (path, tree) =
    match path with
    | h::t ->
        match h with
        | LeftHole(v,r) ->
            ret(t, TreeNode(tree,v,r))
        | RightHole(v,l) ->
            ret(t, TreeNode(l,v,tree))
        | _ ->
            fail
    | _ ->
        fail

let mapTree3 g tree =
    let rec mapTree' focus =
        optState {
            match focus with
            | zippers, TreeNode (left, value, right) ->
                let! newValue = g focus
                let! newLeft = mapTree' ((LeftHole(value,right))::zippers, left)
                let! newRight = mapTree' ((RightHole(value,left))::zippers, right)
                return TreeNode(newLeft, newValue, newRight)
            | _, TreeNil ->
                return TreeNil
            }
    mapTree' ([],tree) 



let g3 (focus:TreeFocus<_>) = 
    someOpt
        (optState {
            let! value = toValue focus
            let! up = toUp focus
            let! upLeft = toLeft up
            let! upLeftValue = toValue upLeft
            return value-upLeftValue 
            })

let test3 tree = run (mapTree3 g3 tree) ()


let cobind1 mtree f =  mtree >>= (fun tree -> mapTree3 f tree)
let (=>>) mtree f =  cobind1 mtree f


let g4 (focus:TreeFocus<_>) = 
    optState {
        let! value = toValue focus
        return Some value
        }

let g5 (focus:TreeFocus<_>) = 
    someOpt
        (optState {
            let! optValue = toValue focus
            let! value = someValue optValue
            let! up = toUp focus
            let! upLeft = toLeft up
            let! optUpLeftValue = toValue upLeft
            let! upLeftValue = someValue optUpLeftValue
            return value-upLeftValue 
            })


let test4 tree = run (ret tree =>> g4 =>> g5) ()

type Take1 = 
    {
        take1Left: bool;
        take1This: bool;
        take1Right: bool;
    }

let mapTree4 choose g tree =
    let rec mapTree' focus =
        optState {
            match focus with
            | zippers, TreeNode (left, value, right) ->
                let choice = choose value
                let! newValue = if choice.take1This then g focus else ret value 
                let! newLeft = if choice.take1Left then mapTree' ((LeftHole(value,right))::zippers, left) else ret left
                let! newRight = if choice.take1Right then mapTree' ((RightHole(value,left))::zippers, right) else ret right
                return TreeNode(newLeft, newValue, newRight)
            | _, TreeNil ->
                return TreeNil
            }
    mapTree' ([],tree) 

let cobind2  mtree (choose,f) =  mtree >>= (mapTree4 choose f)
let (=>-) mtree (choose,f) = cobind2 mtree (choose,f)

let choose optValue =
    match optValue with
    | Some value ->
        {
            take1Left = true;
            take1This = (value = 2);
            take1Right = true;
        }
    | None ->
        {
            take1Left = true;
            take1This = false;
            take1Right = true;
        }
        
//let test5 tree = run (mapTree4 choose g4 tree) ()
let test5 tree = run (cobind1 (ret tree) g4) ()

let test6 tree = run ((ret tree) =>> g4 =>- (choose,g5)) ()

