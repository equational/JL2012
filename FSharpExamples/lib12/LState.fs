module LState
open System.Threading.Tasks
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// A state monad which I extended in two directions: 
//    moving the logical operators in to the monad, 
//    adding a coroutine suspended state concept for reactive computation.

// I also extended the failure reporting but this turns out to be limited because of the typeing constraints.

type
    StateFailureTrace<'state> = LState<'state,unit>
and
    LStateValue<'state, 'ret> =
    | Success   of 'ret
    | Failure   of StateFailureTrace<'state>
and
    LStateTiming<'state,'ret> =
    | Now       of LStateValue<'state, 'ret>
    | Suspend   of LState<'state,'ret>
and
    LStateRet<'state,'ret> = LStateTiming<'state, 'ret>*'state
and
    LStateVariants<'state,'ret> = list<LStateRet<'state,'ret>>
and
    LStateFct<'state,'ret> = 'state -> LStateVariants<'state,'ret>
and
    LState<'state,'ret> = 
        | Return    of LStateFct<'state,'ret>
        | Or        of LState<'state,'ret>*LState<'state,'ret>
        | And       of LState<'state,'ret>*LState<'state,'ret>
        | Not       of LState<'state,'ret>*StateFailureTrace<'state>*'ret
        | Or2       of LState<'state,'ret>*LState<'state,'ret>
and
    LState'<'state,'ret,'par> = 
        | Return'   of ('par->LState<'state,'ret>)
        | Or'       of LState'<'state,'ret,'par>*LState'<'state,'ret,'par>
        | And'      of LState'<'state,'ret,'par>*LState'<'state,'ret,'par>
        | Not'      of LState'<'state,'ret,'par>*StateFailureTrace<'state>*'ret
        | Or2'      of LState'<'state,'ret,'par>*LState'<'state,'ret,'par>
//        | Star      of LState<'state,'ret>
//        | Plus      of LState<'state,'ret>

//        | Optional  of LState<'state,'ret>*LState<'state,'ret>
//        | AndPar of LState<'state,'ret>*LState<'state,'ret>*HelpFunctions<'state,'ret>
//        | Not of LState<'state,'ret>*HelpFunctions<'state,'ret>
//        | AndNot of LState<'state,'ret>*LState<'state,'ret>*HelpFunctions<'state,'ret>
//        | Exist of LState<'state,'ret>
//        | Any of LState<'state,'ret>
//        | Optional of LState<'state,'ret>*LState<'state,'ret>*HelpFunctions<'state,'ret>

//and LStateHelpers<'state> = 
//    {
//        tmp:bool;
////        isTrue: 'state -> bool;
////        isInvertedLogic: 'estate -> bool;
////        invertLogic: StateFailureTrace<'state> -> 'estate -> 'estate;
////        resetLogic: 'estate->'estate;
////        efailureTrace: 'estate-> StateFailureTrace<'state>;
//    }

and
    GetSubState<'s,'ss> = unit->LState<'s,'ss>
and
    MapSubState<'s,'ss> = ('ss->(LState<'s,'ss>))->LState<'s,unit>
and
    GMSubState<'s,'ss> = GetSubState<'s,'ss>*MapSubState<'s,'ss>




//let mapResult tf 
//    List.foldBack
//        (fun ((v1,b1,((es1,s1) as ls1)) as r1) tmpRes ->
//            (
//                match hf.isInvertedLogic es1, hf.isTrue s1 with
//                | false, _ ->
//                    r1
//                | true, true ->
//                    (v1, LogicalFalse (hf.efailureTrace es1), (es1, s1))
//                | true, false ->
//                    (v1, LogicalTrue, ls1)
//            )::tmpRes

let map21 f (a,b) = (f a, b)
let oneNow (r,ls) = [Now r, ls]

let rec nbind m f = Return (fun (ls as ss) ->
    let lr1 = runLState m ss
    List.foldBack 
        (fun r1 lRes -> (f r1) @ lRes
        ) lr1 []
    )

and mapCat lres f =
    List.foldBack 
        (fun r1 lRes ->
            (let rec map' r = 
                match r with
                | (Now r', ls') -> f (r', ls')
                | (Suspend m', ls') -> [Suspend (nbind m' map'), ls'] 
             (map' r1) @ lRes)
        ) lres []

and runLState<'state,'ret> 
    (m:LState<'state,'ret>) 
    (ls: 'state) 
    : LStateVariants<'state,'ret> =
    match m with
    | Return f ->
        f ls
    | Or (m1,m2)  ->
        let rec run2 = function 
            | Success _ , _ as r1-> oneNow r1
            | Failure _, _ -> runLState m2 ls
        mapCat
            (runLState m1 ls)
            run2
    | And (m1,m2)  ->
        let rec run2 = function
            | Success _, ls1 -> runLState m2 ls1
            | Failure _, _ as r1 -> oneNow r1
        mapCat
            (runLState m1 ls)
            run2
    | Not (m1,failureTrace1,default') ->
        let rec run2 = function 
            | Success _, _ -> [Now (Failure failureTrace1), ls]
            | Failure ft1, _ -> [Now (Success default'), ls]
        mapCat 
            (runLState m1 ls)
            run2
    | Or2 (m1,m2)  ->
            (runLState m1 ls) @ (runLState m2 ls)

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
                                [(Now (Failure tc), ls1)]
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
and (>>*) m1 m2 = bind m1 (fun _ -> m2)
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
                | (Success _ as r1), ls1 -> List.map (consWithReturn (Now r1,ls1)) (star'' ls1)
                | Failure _, ls1 as r1 -> oneNow(Success [], ls1))
    star''

let star m1 = 
    Return(fun ls -> star' m1 ls)

let plus m1 = 
    Return(fun ls ->
        mapCat 
            (runLState m1 ls)
            (function
                | Success _ as r1, ls1 -> List.map (consWithReturn (Now r1, ls1)) (star' m1 ls1)
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
