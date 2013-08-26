module LSUtl
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// helpers to boot strap co-recursive state monadic functions
// rewrite with ' anotation for argument

open LState

let recurse1 m = 
    let rec recurse' s = runLState (m m') s
    and m' = Return recurse'
    m'

let recurse2 ms =
    let (m0,m1) = ms 
    let rec 
        recurse1_0 s = runLState (m0 ms') s
    and recurse1_1 s = runLState (m1 ms') s
    and ms' = (Return recurse1_0, Return recurse1_1)
    ms'

let recurse3 ms =
    let (m0,m1,m2) = ms 
    let rec 
        recurse1_0 s = runLState (m0 ms') s
    and recurse1_1 s = runLState (m1 ms') s
    and recurse1_2 s = runLState (m2 ms') s
    and ms' = (Return recurse1_0, Return recurse1_1, Return recurse1_2)
    ms'

let recurse4 ms =
    let (m0,m1,m2,m3) = ms 
    let rec 
        recurse1_0 s = runLState (m0 ms') s
    and recurse1_1 s = runLState (m1 ms') s
    and recurse1_2 s = runLState (m2 ms') s
    and recurse1_3 s = runLState (m3 ms') s
    and ms' = (Return recurse1_0, Return recurse1_1, Return recurse1_2, Return recurse1_3)
    ms'

let recurse5 ms =
    let (m0,m1,m2,m3,m4) = ms 
    let rec 
        recurse1_0 s = runLState (m0 ms') s
    and recurse1_1 s = runLState (m1 ms') s
    and recurse1_2 s = runLState (m2 ms') s
    and recurse1_3 s = runLState (m3 ms') s
    and recurse1_4 s = runLState (m4 ms') s
    and ms' = (Return recurse1_0, Return recurse1_1, Return recurse1_2, Return recurse1_3, Return recurse1_4)
    ms'

let recurse1' ms =
    let rec 
        recurse1_0 = fun x -> Return (fun s -> runLState (ms ms' x) s)
    and ms' = recurse1_0
    ms'

let recurse2' ms =
    let (m0,m1) = ms 
    let rec 
        recurse1_0 = fun x -> Return (fun s -> runLState (m0 ms' x) s)
    and recurse1_1 = fun x -> Return (fun s -> runLState (m1 ms' x) s)
    and ms' = (recurse1_0, recurse1_1)
    ms'

let recurse3' ms =
    let (m0,m1,m2) = ms 
    let rec 
        recurse1_0 = fun x -> Return (fun s -> runLState (m0 ms' x) s)
    and recurse1_1 = fun x -> Return (fun s -> runLState (m1 ms' x) s)
    and recurse1_2 = fun x -> Return (fun s -> runLState (m2 ms' x) s)
    and ms' = (recurse1_0, recurse1_1, recurse1_2)
    ms'

let recurse4' ms =
    let (m0,m1,m2,m3) = ms 
    let rec 
        recurse1_0 = fun x -> Return (fun s -> runLState (m0 ms' x) s)
    and recurse1_1 = fun x -> Return (fun s -> runLState (m1 ms' x) s)
    and recurse1_2 = fun x -> Return (fun s -> runLState (m2 ms' x) s)
    and recurse1_3 = fun x -> Return (fun s -> runLState (m3 ms' x) s)
    and ms' = (recurse1_0, recurse1_1, recurse1_2, recurse1_3)
    ms'

let recurse5' ms =
    let (m0,m1,m2,m3,m4) = ms 
    let rec 
        recurse1_0 = fun x -> Return (fun s -> runLState (m0 ms' x) s)
    and recurse1_1 = fun x -> Return (fun s -> runLState (m1 ms' x) s)
    and recurse1_2 = fun x -> Return (fun s -> runLState (m2 ms' x) s)
    and recurse1_3 = fun x -> Return (fun s -> runLState (m3 ms' x) s)
    and recurse1_4 = fun x -> Return (fun s -> runLState (m4 ms' x) s)
    and ms' = (recurse1_0, recurse1_1, recurse1_2, recurse1_3, recurse1_4)
    ms'

let seq2 m1 m2 f =
    m1
    >>= (fun x1 ->
    m2
    >>= (fun x2 ->
    f x1 x2))

let seq3 m1 m2 m3 f =
    m1
    >>= (fun x1 ->
    m2
    >>= (fun x2 ->
    m3
    >>= (fun x3 ->
    f x1 x2 x3)))

