module XState1
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Monad educational material for a meetup meeting

/// A simple state as an example
type State1 = { 
    keyValues:Map<string,float>;
    counter: int; 
    }

/// An initial test state
let testState1 = {
    keyValues=Map.ofList ["a",1.0;"b",2.0]; 
    counter=100;
    }

/// Stateful read only function example
let get1 state key =
    let value = Map.find key state.keyValues
    value

/// Stateful function example
let set1 state key value = 
    let newEnv = { state with keyValues = Map.add key value state.keyValues; }
    newEnv

/// Stateful function example with no args
let increment1 state = 
    let newCounter = state.counter+1
    let newEnv = { state with counter = newCounter; }
    (newCounter, newEnv)

/// "Main" stateful function example
let test1 state =
    let a = get1 state "a"
    let b = get1 state "b"
    let stateM = set1 state "c" (a+b)
    let (counter, state2) = increment1 stateM
    (counter, state2)


/// Stateful function: normalized return
let get2 state key =
    let value = Map.find key state.keyValues
    (value, state)

/// Stateful function: normalized return
let set2 state key value = 
    let newEnv = { state with keyValues = Map.add key value state.keyValues; }
    ((), newEnv)


/// Stateful function: normalized return
let test2 state =
    let (a,stateM) = get2 state "a"
    let (b,state2) = get2 stateM "b"
    let (_,state3) = set2 state2 "c" (a+b)
    let (counter, state4) = increment1 state3
    (counter, state4)

/// Stateful function: normalized state argument
let get3 key state =
    let value = Map.find key state.keyValues
    (value, state)

/// Stateful function: normalized state argument
let set3 key value state = 
    let newEnv = { state with keyValues = Map.add key value state.keyValues; }
    ((), newEnv)

/// Stateful function: normalized state argument
let increment3 state = 
    let newCounter = state.counter+1
    let newEnv = { state with counter = newCounter; }
    (newCounter, newEnv)

/// Stateful function: normalized state argument
let test3 state =
    let (a,stateM) = get3 "a" state
    let (b,state2) = get3 "b" stateM
    let (_,state3) = set3 "c" (a+b) state2
    let (counter, state4) = increment3 state3
    (counter, state4)

/// Stateful function: normalized arguments
let get4 key state =
    let value = Map.find key state.keyValues
    (value, state)

/// Stateful function: normalized arguments
let set4 (key, value) state = 
    let newEnv = { state with keyValues = Map.add key value state.keyValues; }
    ((), newEnv)

/// Stateful function: normalized arguments
let increment4 _ state = 
    let newCounter = state.counter+1
    let newEnv = { state with counter = newCounter; }
    (newCounter, newEnv)

/// Stateful function: normalized arguments
let test4 state =
    let (a,stateM) = get4 "a" state
    let (b,state2) = get4 "b" stateM
    let (_,state3) = set4 ("c",(a+b)) state2
    let (counter, state4) = increment4 () state3
    (counter, state4)

/// applying one stateful function to the result of the first one
let bind2 f g arg state =
    let (rf, state2) = f arg state
    let (rg, state3) = g rf state2
    (rg, state3)
     
/// applying three stateful functions to their successive results 
let bind3 f g h arg state =
    let (rf, state2) = f arg state
    let (rg, state3) = g rf state2
    let (rh, state4) = h rf state3
    (rh, state4)
         
/// bind: first function includes its arguments
let bind mf g state =
    let (rf, state2) = mf state
    let (rg, state3) = g rf state2
    (rg, state3)

/// bind3: built from bind     
let bind31 f2 g h =
    bind (bind f2 g) h

/// bind: as an operator
let (>>=) mf g = bind mf g

/// bind3: built from bind operator    
let tbind32 mf g h =
    mf >>= g >>= h


let test5 =
    get4 "a" >>= fun a -> 
    get4 "b" >>= fun b ->
    set4 ("c",(a+b)) >>=
    increment4 

let test6 =
    get4 "a" 
    >>= fun a -> 
        get4 "b" 
        >>= fun b ->
            set4 ("c",(a+b)) 
            >>= increment4

/// bind ignore return operator
let (>>/) mf mg =
    mf >>= (fun _ -> mg)

/// Stateful function: normalized arguments
let increment5 state = 
    let newCounter = state.counter+1
    let newEnv = { state with counter = newCounter; }
    (newCounter, newEnv)


let test7 =
    get4 "a" >>= fun a -> 
    get4 "b" >>= fun b ->
    set4 ("c",(a+b)) >>/ 
    increment5 

let ret x = fun state -> (x,state)

let test8 =
    get4 "a" >>= fun a -> 
    get4 "b" >>= fun b ->
    set4 ("c",(a+b)) >>/ 
    increment5 >>= fun counter ->
    ret counter

type StateM() =
    member inline b.Bind (m, f) =  m >>= f
    member inline b.Combine(m1, m2) = m1 >>/ m2
    member inline b.Return x = ret x

let stateM = StateM()

let test9 =
    stateM {
        let! a = get4 "a" 
        let! b = get4 "b" 
        do! set4 ("c",(a+b)) 
        let! counter = increment5
        return counter
        }

let test10 =
    get4 "a" >>= fun a -> 
    get4 "b" >>= fun b ->
    stateM {
        do! set4 ("c",(a+b)) 
        let! counter = increment5
        return counter
        }


let getState f =  (fun s -> (f s, s))
let mapState f = (fun s -> ((), f s))


let getKeyValues state = state.keyValues
let getCounter state = state.counter
let mapKeyValues f state = {state with keyValues=f (state.keyValues); }
let mapCounter f state = {state with counter = f (state.counter); }


/// Stateful function: normalized arguments
let get5 key =
    stateM {
        let! kv = getState getKeyValues
        return Map.find key kv
        }

/// Stateful function: normalized arguments
let set5 (key, value) = 
    stateM {
        do! mapState (mapKeyValues (Map.add key value))
        }

/// Stateful function: normalized arguments
let increment6 = 
    stateM {
        let! newCounter = getState (fun state-> state.counter+1)
        do! mapState (fun state -> { state with counter = newCounter; })
        return newCounter
        }

let test11 =
    stateM {
        let! a = get5 "a" 
        let! b = get5 "b" 
        do! set5 ("c",(a+b)) 
        let! counter = increment6
        return counter
        }


let run1 m state = m state

let test12 state = run1 test9 state
