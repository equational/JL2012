module XState3
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// Monad educational material for a meetup meeting

open System.Threading.Tasks
open System.Threading


type SM<'s,'r> = SM of ('s -> 'r*'s)

let getState f =  SM (fun s -> (f s, s))
let mapState f = SM (fun s -> ((), f s))
let ret x = SM (fun s -> (x,s))

let run2 m env = 
    match m with
    | SM f -> f env

// reminder
/// A simple state as an example
type Env1 = { 
    keyValues:Map<string,float>;
    counter: int; 
    }

// access functions
let getKeyValues env = env.keyValues
let getCounter env = env.counter
let mapKeyValues f env = {env with keyValues=f (env.keyValues); }
let mapCounter f env = {env with counter = f (env.counter); }


/// An initial test state
let testEnv1 = {
    keyValues=Map.ofList ["a",1.0;"b",2.0]; 
    counter=100;
    }


let nbind1 run m g =
    SM(fun env ->
        let (rf, env2) = run m env
        let (rg, env3) = run (g rf) env2
        (rg, env3))

type State2() =
    member inline b.Bind (m, f) =  nbind1 run2 m f
    member inline b.Combine(m1, m2) = nbind1 run2 m1 (fun _ ->m2)
    member inline b.Return x = ret x

let state2 = State2()

(*
let test12 =
    state2 {
        let! a = get4 "a" 
        let! b = get4 "b" 
        do! set4 ("c",(a+b)) 
        let! counter = increment4
        return counter
        }
*)

let bind runA runB mf g =
    SM(fun env->
        let (rf, env2) = runA mf env
        let (rg, env3) = runB (g rf) env2
        (rg, env3))

type State3() =
    member inline b.Bind (m, f) =  bind run2 run2 m f
    member inline b.Combine(m1, m2) = bind run2 run2 m1 (fun _ ->m2)
    member inline b.Return x = ret x

let state3 = State3()

/// Stateful function: normalized arguments
let get5 key =
    state3 {
        let! kv = getState getKeyValues
        return Map.find key kv
        }

/// Stateful function: normalized arguments
let set5 (key, value) = 
    state3 {
        do! mapState (mapKeyValues (Map.add key value))
        }

/// Stateful function: normalized arguments
let increment6 = 
    state3 {
        let! newCounter = getState (fun env-> env.counter+1)
        do! mapState (fun env -> { env with counter = newCounter; })
        return newCounter
        }



let test13 =
    state3 {
        let! a = get5 "a" 
        let! b = get5 "b" 
        do! set5 ("c",(a+b)) 
        let! counter = increment6
        return counter
        }

let nbind3 run1 run2 m g =
    SM(fun env->
    let (rf, env2) = run1 m env
    match rf with
    | Some rfv ->
        let (rg, env3) = run2 (g rfv) env2
        match rg with
        | Some _ ->
            (rg, env3)
        | None ->
            (None, env)
    | None ->
        (None, env))

type State4() =
    member inline b.Bind (m, f) =  nbind3 run2 run2 m f
    member inline b.Combine(m1, m2) = nbind3 run2 run2 m1 (fun _ ->m2)
    member inline b.Return x = ret (Some x)
    member inline b.ReturnFrom m = m

let state4 = State4()

/// Stateful function: in maybe monad
let get6 key =
    SM(fun env->
        let optValue = Map.tryFind key env.keyValues
        (optValue, env))

/// Stateful function: in maybe monad
let set6 (key, value) = 
    SM(fun env->
        let newEnv = { env with keyValues = Map.add key value env.keyValues; }
        (Some (), newEnv))

/// Stateful function: in maybe monad
let increment7 = 
    SM(fun env->
        let newCounter = env.counter+1
        let newEnv = { env with counter = newCounter; }
        (Some newCounter, newEnv))

let getState' f =  SM (fun s -> (Some (f s), s))
let mapState' f = SM (fun s -> (Some (), f s))
let someValue optX = SM (fun s-> (optX, s))

let get7 key =
    state4 {
        let! kv = getState' getKeyValues
        let! value = someValue (Map.tryFind key kv)
        return value
        }

/// Stateful function: normalized arguments
let set7 (key, value) = 
    state4 {
        do! mapState' (mapKeyValues (Map.add key value))
        }

/// Stateful function: normalized arguments
let increment8 = 
    state4 {
        let! newCounter = getState' (fun env-> env.counter+1)
        do! mapState' (fun env -> { env with counter = newCounter; })
        return newCounter
        }


let test14 =
    state4 {
        let! a = get6 "a" 
        let! b = get6 "b" 
        do! set6 ("c",(a+b)) 
        let! counter = increment7
        return counter
        }

let test15 =
    state4 {
        let! a = get6 "a" 
        let! b = get6 "x" 
        do! set6 ("c",(a+b)) 
        let! counter = increment7
        return counter
        }

///  Example of more compelx state
type Env2 = 
    { 
    kenv:Map<string,Env1>;
    zenv:Env1;
    }

/// An initial test state
let testEnv2 = {kenv=Map.ofList ["x",testEnv1;"y",testEnv1]; zenv=testEnv1;}

let foldZEnv f x =
    let (r, x') = f x.zenv
    (r, { x with zenv=x' })

let adaptM fold run m =
    SM (fun env -> fold (run m) env)

let test16 =
    adaptM foldZEnv run2
        (state3 {
            let! a = get5 "a" 
            let! b = get5 "b" 
            do! set5 ("c",(a+b)) 
            let! counter = increment6
            return counter
            })


let foldKEnv k f x =
    let (r, x') = f (Map.find k x.kenv)
    (r, { x with kenv=(Map.add k x' x.kenv) })

let test17 =
    adaptM (foldKEnv "x") run2
        (state3 {
            let! a = get5 "a" 
            let! b = get5 "b" 
            do! set5 ("c",(a+b)) 
            let! counter = increment6
            return counter
            })

let adaptM' fold run m' = 
    fun x -> adaptM fold run (m' x)

let test18 =
    state3 {
        let Z m = adaptM foldZEnv run2 m
        let KVX m = adaptM (foldKEnv "x") run2 m
        let Z' m' = adaptM' foldZEnv run2 m'
        let KVX' m' = adaptM' (foldKEnv "x") run2 m'
        let! a = Z (get5 "a") 
        let! b = (Z' get5) "b" 
        do! (KVX' set5) ("c",(a+b)) 
        let! counter1 = KVX increment6
        let! _ = KVX increment6
        let! counter2 = Z increment6
        return counter2
        }

let test19 =
    state4 {
        let Z m = adaptM foldZEnv run2 m
        let KVX m = adaptM (foldKEnv "x") run2 m
        let Z' m' = adaptM' foldZEnv run2 m'
        let KVX' m' = adaptM' (foldKEnv "x") run2 m'
        let! a = Z (get6 "a") 
        let! b = (Z' get6) "b" 
        do! (KVX' set6) ("c",(a+b)) 
        let! counter1 = KVX increment7
        let! _ = KVX increment7
        let! counter2 = Z increment7
        return counter2
        }

let test20 =
    state4 {
        let Z m = adaptM foldZEnv run2 m
        let KVX m = adaptM (foldKEnv "x") run2 m
        let Z' m' = adaptM' foldZEnv run2 m'
        let KVX' m' = adaptM' (foldKEnv "x") run2 m'
        let! a = Z (get6 "a") 
        let! b = (Z' get6) "b" 
        do! (KVX' set6) ("c",(a+b)) 
        let! counter1 = KVX increment7
        let! _ = KVX increment7
        let! counter2 = Z increment7
        let! err = (Z' get6) "x" 
        return counter2
        }


type Query<'k> = 
| SingleQ of 'k
| RangeQ of Option<'k>*Option<'k>
| MultiQ of list<Query<'k>>
| NotQ of Query<'k>
| AnyQ

type Op<'k,'v,'dv,'s> = 
| Insert    of 'k*'dv
| Update    of 'k*('v->('v*'dv))
| Remove    of 'k
| Map       of 'k*('v->'v)
| Apply     of 'k*('v->SM<'s,Option<unit>>)
| RemoveQ    of Query<'k>
| MapQ       of Query<'k>*('k*'v->'v)
| ApplyQ     of Query<'k>*('k*'v->SM<'s,Option<unit>>)


type Update<'k,'v> = 'k*Option<'v>*Option<'v>
let update k optOld optNew = (k,optOld,optNew)



type TableOp<'table, 'k,'v> = 'table -> Update<'k,'v> -> 'table

type KVList<'k,'v>=list<'k*'v>
type KTList<'k,'v,'s>=list<'k*(Update<'k,'v>->SM<'s,Option<unit>>)>
type KVTS<'us,'k,'v> = KVTS of KVList<'k,'v>*KTList<'k,'v, KVTS<'us,'k,'v>>*'us

let lookupKey k kvl = List.tryPick (fun (k',v') -> if(k=k') then Some v' else None) kvl

let insertTrigger k t = 
    SM (fun (KVTS (kvl, ktl, suser)) ->
        (Some (), KVTS (kvl, (k,t)::ktl, suser)))

let updateTrigger k t = 
    SM (fun (KVTS (kvl, ktl, suser)) ->
        (Some (), KVTS (kvl, (k,t)::ktl, suser)))

/// remove should be by trigger id key not by data key
let removeTrigger k = 
    SM (fun (KVTS (kvl, ktl, suser)) ->
        let ktl = List.filter (fun (k', _) -> k<>k') ktl
        (Some (), KVTS (kvl, ktl, suser)))


let doOpt optT =
    SM (fun s ->
    match optT with
    | Some t -> run2 t s
    | None -> (Some(), s))

let select k m = 
    SM (fun (KVTS (kvl, ktl, suser)) ->
        let optV = lookupKey k kvl
        match optV with
        | Some (KVTS (kvl', ktl', suser')) -> 
            let (t, (KVTS (kvl'', ktl'', suser''))) = run2 m (KVTS (kvl', ktl', suser))
            let v' = KVTS (kvl'', ktl'', suser')
            (t, KVTS((k,v')::kvl, ktl, suser''))
        | None ->
            (None, (KVTS (kvl, ktl, suser))))

let insertActive k v = 
    SM (fun (KVTS (kvl, ktl, suser)) ->
        let optT = lookupKey k ktl
        (Some (Option.map (fun t -> t (update k None (Some v))) optT), KVTS((k,v)::kvl, ktl, suser)))

let updateActive k v = 
    SM (fun (KVTS (kvl, ktl, suser)) ->
        let optT = lookupKey k ktl
        let optV = lookupKey k kvl
        (Some(Option.map (fun t -> t (update k optV (Some v))) optT), KVTS((k,v)::kvl, ktl, suser)))

let mapActive k m = 
    SM (fun (KVTS (kvl, ktl, suser)) ->
        let optT = lookupKey k ktl
        let optOldV = lookupKey k kvl
        let update = m optOldV
        match update with
        | (_, _, Some newV) ->
            (Some(Option.map (fun t -> t update) optT), KVTS((k,newV)::kvl, ktl, suser))
        | (_, _, None) ->
            let kvl = List.filter (fun (k', _) -> k<>k') kvl
            (Some(Option.map (fun t -> t update) optT), KVTS(kvl, ktl, suser)))

let removeActive k = 
    SM (fun (KVTS (kvl, ktl, suser)) ->
        let optT = lookupKey k ktl
        let optV = lookupKey k kvl
        let kvl = List.filter (fun (k', _) -> k<>k') kvl
        (Some(Option.map (fun t -> t (update k optV None)) optT), KVTS(kvl, ktl, suser)))

let test22: SM<KVTS<unit,string,int>, Option<unit>> = 
    state4 {
        let trigger update =
            state4 {
                printfn "Update: %A" update
                return ()
                }
        do! insertTrigger "a" trigger
        let! optT = insertActive "a" 1
        do! doOpt optT
        let! optT = insertActive "b" 33
        do! doOpt optT
        let! optT = updateActive "a" 2
        do! doOpt optT
        let! optT = removeActive "a"
        do! doOpt optT
        return () 
        }

let test23 () = run2 test22 (KVTS([],[],()))

(*

open System.Threading.Tasks

type
    SM<'s,'r> = SM of ('s->'r*'s)
and
    LSM<'s,'r,'m>  =
    | Return    of 'm
    | Or        of LSM<'s,'r,'m>*LSM<'s,'r,'m>
    | And        of LSM<'s,'r,'m>*LSM<'s,'r,'m>
    | Not        of LSM<'s,'r,'m>*'r
and
    SM'<'s,'r,'i> = 'i->SM<'s,'r>
and 
    RSM<'s,'r> = SM<'s,'r> -> 's -> ('r*'s)



let runSM m sIn  =
    match m with
    | SM f ->
        match f sIn with
        | (r, sOut) as res -> res


let inline runLSM' run =
    let rec runLSM m sIn =
        match m with
        | Return m1 ->
            run m1 sIn
        | Or (m1,m2) ->
            match (runLSM m1 sIn) with
            | (Some r, sOut) as res -> res
            | (None, _) -> 
                runLSM m2 sIn
        | And (m1,m2) ->
            match (runLSM m1 sIn) with
            | (Some r1, s1) ->
                runLSM m2 s1
            | (None, sOut) -> 
                (None, sOut)
        | Not (m1,some) ->
            match (runLSM m1 sIn) with
            | (Some r, _) -> (None, sIn)
            | (None, sOut) -> (some, sOut) 
    runLSM

let runLSM m sIn = (runLSM' runSM) m sIn

let ret x = SM (fun s -> (x,s))
let lret x = SM (fun s -> (Some x, s))
let cret x = SM (fun s -> ([x], s))

let bind run1 run2 m f  = SM (fun s1 ->
    match run1 m s1 with
    | (r, s2) ->
        run2 (f r) s2
    )

let rec mapFold f l s =
    match l with
    | h::t ->
        let (h',s') = f h s
        let (t',s'') =  mapFold f t s'
        (h'::t', s'')
    | [] ->
        ([], s)

let cbind run1 run2 m f  = SM (fun s1 ->
    match run1 m s1 with
    | (r, s2) ->
         let (r'', s'') = mapFold (fun r' s' -> run2 (f r') s') r s2
         (List.concat r'', s'')
    )

let lBind run1 run2 m f = Return (SM (fun s1 ->
    match run1 m s1 with
    | (Some r, s2) ->
        run2 (f (Some r)) s2
    | (None, _) ->
        (None, s1)
    ))

let bind1 m f = cbind runSM runSM m f

let lBind1 m f = lBind runSM runSM m f
let lBind2 m f= lBind runLSM runLSM m f

// now with local state adapter

let runSMLocal adapter m sIn  =
    match m with
    | SM f ->
        match (adapter f) sIn with
        | (r, sOut) as res -> res


type
    Record<'s> = {a:'s;}


let foldRecord f x =
    let (r, x') = f x.a
    (r, { x with a=x' })

let adaptSM fold run m =
    SM (fun s -> fold (run m) s)

let adaptSM' fold run m' =
    fun x -> adaptSM fold run (m' x)

let adaptLSM fold run m =
    Return (adaptSM fold run m) 

let adaptLSM' fold run m' =
    fun x -> adaptLSM fold run (m' x)

let lBind1Local m f = bind runSM runSM (adaptSM foldRecord runSM m) (adaptSM' foldRecord runSM f)
let lBind2Local m f = lBind runLSM runLSM (adaptLSM foldRecord runLSM m) (adaptLSM' foldRecord runLSM f)

// test type class




// substate run and bind

// with dynamic adaptater

let foldMap k f map =
    let (r, x') = f (Map.find k map)
    (r, Map.add k x' map)


let lBind3Local k m f = bind runSM runSM (adaptSM (foldMap k) runSM m) (adaptSM' (foldMap k) runSM f)
let lBind4Local k m f = lBind runLSM runLSM (adaptLSM (foldMap k) runLSM m) (adaptLSM' (foldMap k) runLSM f)


let fold2 k f = foldRecord (foldRecord (foldMap k f))

let lBind5Local k m f = bind runSM runSM (adaptSM (fold2 k) runSM m) (adaptSM' (fold2 k) runSM f)
let lBind6Local k m f = lBind runLSM runLSM (adaptLSM (fold2 k) runLSM m) (adaptLSM' (fold2 k) runLSM f)


*)

// now with asynchronous integration
(*
P: events -> state
I: state -> events
We want to ecapsulate user's events into a series of monadic operations
*)


    


(*
and ret x = Return (fun ls -> [(Now(Success x), ls)])

and bind m f = Return (fun (ls as ss) ->
    let lr1 = runLState m ss
    List.foldBack 
        (fun r1 lRes ->
            (
                match r1 with
                | Now(Success v1), ls1 ->
                    let n = f v1 
                    runLState n ls1
                | Now(Failure tc), ls1 ->
                    [(Now(Failure tc), ls1)]
                | Suspend m', ls1 ->
                    [Suspend (bind m' f), ls1]
            ) @ lRes
        ) lr1 []
    )

and bind' m m' = 
    match m' with
        | Return' f ->
            Return (fun (ls as ss) ->
                let lr1 = runLState m ss
                List.foldBack 
                    (fun r1 lRes ->
                        (
                            match r1 with
                            | Now(Success v1), ls1 ->
                                let n = f v1 
                                runLState n ls1
                            | Now(Failure tc), ls1 ->
                                [((Failure tc), ls1)]
                            | Suspend m2, ls1 ->
                                [Suspend (bind' m2 m'), ls1]
                        ) @ lRes
                    ) lr1 []
                )
        | Or'(m1',m2') ->
            Or(bind' m m1', bind' m m2')
        | And'(m1',m2') ->
            And(bind' m m1', bind' m m2')
        | Not'(m1',ft,r)->
            Not(bind' m m1', ft, r)
        | Or2'(m1',m2')-> 
            Or2(bind' m m1', bind' m m2')



and zero = Return (fun ls -> [(Now(Success ()), ls)]) 
and combine m1 m2 = bind m1 (fun () -> m2)
and delay f = Return (fun s -> runLState (f()) s)

//let setState s =  Return (fun _ -> [((), LogicalTrue, s)])
and getState () =  Return (fun ls -> [(Now(Success ls), ls)])
and mapState f =  Return (fun ls -> [(Now(Success ()), (f ls))])
and mapState' f =  Return (fun ls -> 
    [(Now(Success ()), (f ls))])
and failState t =  Return (fun ls -> [(Now(Failure t), ls)])
and suspend m =  Return (fun ls -> [(Suspend m, ls)])
//let applyState f =  Return (fun (invert, ls) -> [(f ls, LogicalTrue, ls)])
//let mapState ft ff =  Return (fun (invert, ls) -> 
//    match invert with
//    | LogicalTrue -> ft ls
//    | LogicalFalse failureTrace -> ff failureTrace ls
//    )

and simpleStateFailure failureTrace state =
    [(Failure failureTrace, state)]

and mapStateTrue m f = Return (fun ls -> 
    mapCat
        (runLState m ls)
        (function
            | Success v, s -> oneNow (Success v, f s)
            | Failure t, s -> oneNow (Failure t, s)))

and mapStateFalse m f = Return (fun ls -> 
    mapCat
        (runLState m ls)
        (function
            | Success v, s -> oneNow (Success v, s)
            | Failure t, s -> oneNow(Failure t, f s)))

and mapRet m f = Return (fun ls -> 
    mapCat
        (runLState m ls)
        (function
            | Success v, s -> oneNow (Success (f v), s)
            | Failure t, s -> oneNow (Failure t, s)))

and combineFail m f = Return (fun ls -> 
    mapCat
        (runLState m ls)
        (function
            | Success v, s -> oneNow (Success v, s)
            | Failure t, s -> oneNow (Failure (combine t f), s)))
and mapSome m f = Return (fun ls -> 
    mapCat
        (runLState m ls)
        (function
            | Success (Some v), s -> oneNow (Success (Some(f v)), s)
            | Success None, s -> oneNow (Success None, s)
            | Failure t, s -> oneNow (Failure t, s)))


and (>>=) = bind
and (>>/) = combine
and (>>* ) m1 m2 = bind m1 (fun _ -> m2)
and (>>^) m1 m2 =
    m1 
    >>= (fun x1 ->
    m2 
    >>/ ret x1)

and (^>=) = mapState
and (^>+) = mapStateTrue
and (^>!) = mapStateFalse

and (|>=) = mapRet
and (|>!) = combineFail
and (|>?) = mapSome

let mapInnerState (get, map) foldInner =
    get ()
    >>= foldInner
    >>= (fun (r,newCache) ->
        map (fun _ -> ret newCache) 
        >>/ ret r)

let consWithReturn (Now(Success v1), ls1) (Now(Success v2), ls2) =
    (Now(Success (v1::v2)), ls2)

let private star' m1 =
    let rec star'' ls =
        mapCat 
            (runLState m1 ls)
            (function
                | (Success _ as r1), ls1 -> List.map (consWithReturn (r1,ls1)) (star'' ls1)
                | Failure _, ls1 as r1 -> oneNow(Success [], ls1))
    star''

let star m1 = 
    Return(fun ls -> star' m1 ls)

let plus m1 = 
    Return(fun ls ->
        mapCat 
            (runLState m1 ls)
            (function
                | Success _ as r1, ls1 -> List.map (consWithReturn (r1, ls1)) (star' m1 ls1)
                | Failure _, ls1 as r1 -> [(Now(Success []), ls1)])
        )

let mcond1 b m1 m2 =
    if b then combine m1 m2
    else m2 

let mcfold1 b fm1 fm2 acc =
    if b then 
        (fm1 acc) >>= fm2
    else 
        fm2 acc


type LState () =
    member inline b.Return x = ret x
    member inline b.ReturnFrom x = x
    member inline b.Bind (m, f) = bind m f
    member inline b.Zero() = zero
    member b.Combine(m1, m2) = combine m1 m2

let lstate = LState ()
*)