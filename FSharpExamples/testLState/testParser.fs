module TestParser

open LState
open LParser
open LSUtl

open System
open System.IO

type I2 =
    | IV of int
    | LL of list<I2>

//let getState = 
let private mapState f = 
    getState () 
    >>= f 
    >>= (fun s' -> 
    LState.mapState (fun _ -> s'))  


let pacc = {
    LParser.getMap = (getState, mapState);
        }

let sat = sat pacc
let literal (s : string) r = literal pacc (s : string) r
let digit = digit pacc
let endOfInput = endOfInput pacc
let autoIndent a b = autoIndent pacc a b
let letter = letter pacc
let tag p = tag pacc (GenErr p)


let test expect p (s : string) = 
    let i = s.ToCharArray() |> List.ofArray
    let ans = runLState p (newParserState i)
//    let ans2 = List.map (function
//        | (Success _,_) as r1 -> r1
//        | (Failure f, s) -> List.head (runLState f s)) ans
    printfn "%b %A" expect ans

let digitVal = lstate { 
    let! c = digit
    return int c - int '0' 
    }

let nat = chainl1 digitVal (ret (fun x y -> ret (10*x + y))) |>! tag "natural number"


let addOp = literal "+" (fun x y -> ret(x + y)) <|> literal "-" (fun x y -> ret(x - y)) |>! tag "add/sub op"
let mulOp = literal "*" (fun x y -> ret(x * y)) <|> literal "/" (fun x y -> ret(x / y)) |>! tag "mul/div op"
let expOp = literal "^" (fun (x:int) (y:int) -> ret(Math.Pow(float x,float y) |> int)) |>! tag "exp op"

let expr =
    let expr2 expr =
        let paren =
            seq3
                (literal "(" 0)
                expr
                (literal ")" 0)
                (fun _ e _ -> ret e)
        let part = nat <|> paren
        let factor = chainr1 part expOp
        let term = chainl1 factor mulOp
        let sum = chainl1 term addOp
        sum
    recurse1 expr2

let fullExpr =  
    expr >>^
    (endOfInput (tag "expected end of input") |>= ignore)

let separator = sat Char.IsSeparator
let control = sat  Char.IsControl
let separators = many separator 
let sepcontrols = many (separator <|> control)


//let exprBlock = 
//    lstate {
//        let! b = autoIndent (IV) (LL) sepcontrols expr
//        let! _ = endOfInput
//        return b
//        }

//let simpleAdd  =
//    let loop loop x = 
//            (
//            addOp >>= (fun f -> 
//            term >>=  f x >>= loop 
//            )) <|> (ret x)
//    term >>= (recurse1' loop)

let testParser() =
    test true expOp "^"
//    test true simpleAdd "2+8!"
//    test true factor "2^8!"
//    test true term "2+8!"
    test true expr "2+8!"
    test true letter "abc"
    test false letter "123"
    test true (letter <|> digit) "123"
    test true (seq2 letter digit (fun _ _ -> ret " ")) "a1"
    test false (letter <|> digit) ";23"
    test true (literal "abc" 0) "abc"
    test false (literal "12" 0) "abc"
//    test ((tryP (literal "abc" 1)) <|> (literal "aba" 2)) "aba"
    test true (((literal "abc" 1)) <|> (literal "aba" 2)) "aba"
    test true (many letter) "hello world"
    test true nat "123!"
    test false nat "hello"
    test true (endOfInput (tag "expected end of input")) ""
    test false (endOfInput (tag "expected end of input")) "z"
    test true expr "2+8!"
    test true expr "(611-2^3^2+1)/10-5*2!"
    test false expr "(611-2^-3^2+1)/10-5*2!"
    test false expr "(611-2^3^2.2+1)/10-5*2!"
    test true fullExpr "(611-2^3^2+1)/10-5*2"
    test false fullExpr "(611-2^3^2+1)/10-5*2!"
//    test exprBlock 
//        "2+2 4+1"
//    test exprBlock  "1\n 2\n 3\n4\n 5\n  6\n  7\n8"
//    test exprBlock  "1\n 2\n 3\n4\n  5\n  6\n 7\n8"

