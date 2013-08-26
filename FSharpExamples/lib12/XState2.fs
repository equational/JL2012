module XState2
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Monad educational material for a meetup meeting

/// state monad "wrapped up in a type
type SM<'s,'r> = SM of ('s -> 'r*'s)

// again the helpful definitions
let getState f =  SM (fun s -> (f s, s))
let mapState f = SM (fun s -> ((), f s))
let ret x = SM (fun s -> (x,s))

/// takes function out of type and runs it on given state
let run m state = 
    match m with
    | SM f -> f state

// Again...
/// A simple state as an example
type State1 = { 
    keyValues:Map<string,float>;
    counter: int; 
    }

// access functions
let getKeyValues state = state.keyValues
let getCounter state = state.counter
let mapKeyValues f state = {state with keyValues=f (state.keyValues); }
let mapCounter f state = {state with counter = f (state.counter); }


/// An initial test state
let testState1 = {
    keyValues=Map.ofList ["a",1.0;"b",2.0]; 
    counter=100;
    }

let bind mf g =
    SM(fun state->
        let (rf, state2) = run mf state
        let (rg, state3) = run (g rf) state2
        (rg, state3))

type StateM() =
    member inline b.Bind (m, f) =  bind m f
    member inline b.Combine(m1, m2) = bind m1 (fun _ ->m2)
    member inline b.Return x = ret x

let stateM = StateM()

let get5 key =
    stateM {
        let! kv = getState getKeyValues
        return Map.find key kv
        }

let set5 (key, value) = 
    stateM {
        do! mapState (mapKeyValues (Map.add key value))
        }

let increment6 = 
    stateM {
        let! newCounter = getState (fun state-> state.counter+1)
        do! mapState (fun state -> { state with counter = newCounter; })
        return newCounter
        }

let test13 =
    stateM {
        let! a = get5 "a" 
        let! b = get5 "b" 
        do! set5 ("c",(a+b)) 
        let! counter = increment6
        return counter
        }

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

/// stateful maybe monad
type OptState() =
    member inline b.Bind (m, f) =  bindOpt m f
    member inline b.Combine(m1, m2) = bindOpt m1 (fun _ ->m2)
    member inline b.Return x = ret (Some x)
    member inline b.ReturnFrom m = m

/// global instance, useful for silly F# do notation syntax.
let optState = OptState()

/// Stateful function: in maybe monad
let get6 key =
    SM(fun state->
        let optValue = Map.tryFind key state.keyValues
        (optValue, state))

/// Stateful function: in maybe monad
let set6 (key, value) = 
    SM(fun state->
        let newState = { state with keyValues = Map.add key value state.keyValues; }
        (Some (), newState))

/// Stateful function: in maybe monad
let increment7 = 
    SM(fun state->
        let newCounter = state.counter+1
        let newState = { state with counter = newCounter; }
        (Some newCounter, newState))

let getState' f =  SM (fun s -> (Some (f s), s))
let mapState' f = SM (fun s -> (Some (), f s))
let someValue optX = SM (fun s-> (optX, s))

let get7 key =
    optState {
        let! kv = getState' getKeyValues
        let! value = someValue (Map.tryFind key kv)
        return value
        }

let set7 (key, value) = 
    optState {
        do! mapState' (mapKeyValues (Map.add key value))
        }

let increment8 = 
    optState {
        let! newCounter = getState' (fun state-> state.counter+1)
        do! mapState' (fun state -> { state with counter = newCounter; })
        return newCounter
        }


let test14 =
    optState {
        let! a = get6 "a" 
        let! b = get6 "b" 
        do! set6 ("c",(a+b)) 
        let! counter = increment7
        return counter
        }

let test15 =
    optState {
        let! a = get6 "a" 
        let! b = get6 "x" 
        do! set6 ("c",(a+b)) 
        let! counter = increment7
        return counter
        }

///  Example of more compelx state
type State2 = 
    { 
    kstate:Map<string,State1>;
    zstate:State1;
    }

/// An initial test state
let testState2 = {kstate=Map.ofList ["x",testState1;"y",testState1]; zstate=testState1;}

let foldZState f x =
    let (r, x') = f x.zstate
    (r, { x with zstate=x' })

let adaptM fold m =
    SM (fun state -> fold (run m) state)

let test16 =
    adaptM foldZState
        (stateM {
            let! a = get5 "a" 
            let! b = get5 "b" 
            do! set5 ("c",(a+b)) 
            let! counter = increment6
            return counter
            })


let foldKState k f x =
    let (r, x') = f (Map.find k x.kstate)
    (r, { x with kstate=(Map.add k x' x.kstate) })

let test17 =
    adaptM (foldKState "x")
        (stateM {
            let! a = get5 "a" 
            let! b = get5 "b" 
            do! set5 ("c",(a+b)) 
            let! counter = increment6
            return counter
            })

let adaptM' fold m' = 
    fun x -> adaptM fold (m' x)

let test18 =
    stateM {
        let Z m = adaptM foldZState m
        let KVX m = adaptM (foldKState "x") m
        let Z' m' = adaptM' foldZState m'
        let KVX' m' = adaptM' (foldKState "x") m'
        let! a = Z (get5 "a") 
        let! b = (Z' get5) "b" 
        do! (KVX' set5) ("c",(a+b)) 
        let! counter1 = KVX increment6
        let! _ = KVX increment6
        let! counter2 = Z increment6
        return counter2
        }

let test19 =
    optState {
        let Z m = adaptM foldZState m
        let KVX m = adaptM (foldKState "x") m
        let Z' m' = adaptM' foldZState m'
        let KVX' m' = adaptM' (foldKState "x") m'
        let! a = Z (get6 "a") 
        let! b = (Z' get6) "b" 
        do! (KVX' set6) ("c",(a+b)) 
        let! counter1 = KVX increment7
        let! _ = KVX increment7
        let! counter2 = Z increment7
        return counter2
        }

let test20 =
    optState {
        let Z m = adaptM foldZState m
        let KVX m = adaptM (foldKState "x") m
        let Z' m' = adaptM' foldZState m'
        let KVX' m' = adaptM' (foldKState "x") m'
        let! a = Z (get6 "a") 
        let! b = (Z' get6) "b" 
        do! (KVX' set6) ("c",(a+b)) 
        let! counter1 = KVX increment7
        let! _ = KVX increment7
        let! counter2 = Z increment7
        let! err = (Z' get6) "x" 
        return counter2
        }


