module LSList
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// helper functions for the state monad

open LState

let inline listU pairF nilF l : LState<'state,'ret>=
    let rec iterR' l =
            match l with
            | h::t  ->
                pairF iterR' h t
            | []    -> 
                nilF ()
    iterR' l

let iterR f = 
    listU
        (fun iterR' h t -> combine (f h) (iterR' t))
        (fun () -> ret ())
            
let iterL f = 
    listU
        (fun iterR' h t -> combine (iterR' t) (f h))
        (fun () -> ret ())
            

let mapR f = 
    listU
        (fun iterR' h t -> 
            (f h) >>= (fun h' -> (iterR' t) |>= (fun t' -> (h'::t'))))
        (fun () -> ret [])
            
let mapL f = 
    listU
        (fun iterR' h t -> 
             (iterR' t) >>= (fun t' ->(f h)  |>= (fun h' -> (h'::t'))))
        (fun () -> ret [])
            

let foldR f l s : LState<'state,'ret>=
    let rec foldR' s l =
            match l with
            | h::t  ->
                (f h s) >>= (fun s' -> foldR' s' t)
            | []    -> 
                ret s
    foldR' s l

let foldL f l s : LState<'state,'ret>=
    let rec foldL' s l =
            match l with
            | h::t  ->
                (foldL' s t) >>= (fun s' -> f h s')
            | []    -> 
                ret s
    foldL' s l


let reduceR f l : LState<'state,'ret> = foldR f (List.tail l) (List.head l)
let reduceL f l : LState<'state,'ret> = foldL f (List.tail l) (List.head l)
