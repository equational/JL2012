module LParser
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Parser monad based on state monad.
// uses "standard" base of parser combinators.


open LState
open LSUtl
open LSList
open System

exception ParserMEmptyTabStack

type
    ColNb = int
and
    LineNb = int
and 
    PState = 
    {
        pInput:list<char>; 
        colNb:ColNb; 
        lineNb:LineNb; 
        tabStack:list<ColNb*LineNb>;
        errs: list<PError*PState>;
    }  
    member ps.pushTab() =
        { ps with tabStack = (ps.colNb, ps.lineNb)::ps.tabStack }
    member ps.popTab() =
        { ps with tabStack = List.tail ps.tabStack }
    member ps.colGreaterAndLineIsNotInTabStack() =
        let (topCol, _) = List.head ps.tabStack
        (ps.colNb > topCol) && not(List.exists (fun (_,l) -> l = ps.lineNb) ps.tabStack)
    member ps.colInTabStackButNotTop() =
        List.exists (fun (c,_) -> c = ps.colNb) (List.tail ps.tabStack)
    member ps.hasExtraTabs() =
        (List.length ps.tabStack)>1
    member ps.tabsLeftOver() =
        let (topCol, _) = List.head ps.tabStack
        ps.colNb < topCol
    member ps.colIsTopOfTabStack() =
        let (topCol, topLine) = List.head ps.tabStack
        (ps.colNb = topCol)
    member ps.colIsTopOfTabStackOrInSameLine() =
        let (topCol, topLine) = List.head ps.tabStack
        (ps.colNb = topCol) || (ps.lineNb = topLine)

and
    PError =
    | ExpectedChar                  of  char
    | ExpectedEndOfInput
    | ExpectTabsAligned
    | ExpectBlockToStartInSameLineOrWithAlignedTabs
    | ExpectedBeginingOfBlock
    | EndOfBlockExpectedAtColumns   of list<ColNb*LineNb>
    | ExpectedEndOfBlock
    | TagLetter
    | TagDigit
    | TagLetterOrDigit
    | TagNewline
    | UnexpectedFollowing
    | TagExpected                   of string
    | GenErr                        of string




type
    GetPState<'s> = unit->LState<'s,PState>
and
    MapPState<'s> = (PState->(LState<'s,PState>))->LState<'s,unit>
and
    GMPState<'s> = GetPState<'s>*MapPState<'s>

type
    PAccessor<'s> = {
        getMap:     GMPState<'s>;
        }


let newParserState i =
    {
        pInput = i; 
        colNb = 0; 
        lineNb = 1; 
        tabStack = [(0,1)];
        errs = [];
    }  


/// parser is a state monad with error management.
/// State is state of the input stream (upcoming list of char).

/// error is list of string messages.
let tag acc msg = 
    mapInnerState acc.getMap (fun pstate -> 
        ret ((), 
            { pstate with
                errs = (msg,pstate)::pstate.errs}))

/// parser one or the other
let orP p1 p2 = Or (p1, p2)
/// handy or operator
let (<|>) = orP

let optP p = p <|> (ret None)

///// try parsing (don't update state on success) and succeed on failure
//let tryNot acc p label = mapRet ((fun _ -> ()), (fun _ -> ErrMsg (label))) p

let sat acc pred = 
    mapInnerState acc.getMap 
        (fun pstate ->
            match (pstate.pInput) with
            | ch::leftover when (pred ch)->
                ret (ch, 
                        { pstate with 
                            pInput=leftover; 
                            colNb=(if ch<>'\n' then pstate.colNb+1 else 0); 
                            lineNb=(if ch='\n' then pstate.lineNb+1 else pstate.lineNb);
                        })
            | ch::_ -> 
                failState (tag acc (ExpectedChar ch)) 
            | []    -> 
                failState (tag acc ExpectedEndOfInput) 
            )

let letter acc = sat acc Char.IsLetter  |>! tag acc TagLetter
let digit acc = sat acc Char.IsDigit    |>! tag acc TagDigit
let letterOrDigit acc = sat acc Char.IsLetterOrDigit |>! tag acc TagLetterOrDigit
let newLine acc = sat acc ((=) '\n')    |>! tag acc TagNewline

let literal acc (s : string) r =
    let help help' l =
            match l with
            | [] -> ret r
            | c :: cs -> 
                sat acc (fun x -> x = c) 
                >>* help' cs 
    s.ToCharArray() |> List.ofArray |> (recurse1' help) |>! tag acc (TagExpected s)

let many p = star p
//    let many (_,many1)  = many1 <|> (ret [])
//    let many1 (many, _)  =  
//        p >>= (fun x ->
//        many >>= (fun xs ->
//        ret (x :: xs)))
//    let (many, many1) =recurse2 (many, many1)
//    many

let many1Fold p f = plus p >>= reduceL f

let many1FoldBack p f = plus p >>= reduceR f

let chainl1' p op q =
    let loop loop x = 
            (
            op >>= (fun f -> 
            q >>=  f x >>= loop 
            )) <|> (ret x)
    p >>= (recurse1' loop)

let chainl1 p op = chainl1' p op p

let chainr1' p op q =
    let chainr1' chainr1' = 
        Or(
            p >>= (fun x ->
            op >>= (fun f -> 
            chainr1' 
            >>= f x)), 
            q)
    recurse1 chainr1'

let chainr1 p op = chainr1' p op p


/// Successfully parse first one but needs to fail second.
/// State is state if second one.
let notFollowedBy tag p1 p2 = And(p1, Not (p2, tag,' '))

let dummyChar = ' ' // used to have a char return type
/// There is no more input
let endOfInput acc msg = notFollowedBy msg (ret dummyChar) (sat acc (fun ch->true)) |>! tag acc ExpectedEndOfInput

let aligned acc =
    mapInnerState acc.getMap (fun pstate -> 
    if pstate.colIsTopOfTabStack() then
        ret((), pstate)
    else
        failState (tag acc ExpectTabsAligned)
    )
   

let inTopBlock acc =
    mapInnerState acc.getMap (fun pstate -> 
    if pstate.colIsTopOfTabStackOrInSameLine() then
        ret((), pstate)
    else
        failState (tag acc (ExpectBlockToStartInSameLineOrWithAlignedTabs))
    )


let beginBlock acc =
    mapInnerState acc.getMap (fun pstate -> 
    if pstate.colGreaterAndLineIsNotInTabStack() then
        ret ((), pstate.pushTab())
    else
        failState (tag acc (ExpectedBeginingOfBlock))
    )

let endBlock acc = 
    mapInnerState acc.getMap (fun pstate -> 
    if pstate.colInTabStackButNotTop() then
        ret ((), pstate.popTab())
    else
        if (pstate.pInput)=[] && pstate.hasExtraTabs() then
            ret ((), pstate.popTab())
        else
            if pstate.tabsLeftOver() then
                failState (tag acc ((EndOfBlockExpectedAtColumns pstate.tabStack))) 
            else
                failState (tag acc (ExpectedEndOfBlock))
    )

// I do not remember if this one works, needs checking.
let autoIndent acc blockItem item =
    let autoIndent' autoIndenR =
        seq2
            (plus item)
            (star (
                seq3
                    (beginBlock acc)
                    autoIndenR
                    (endBlock acc)
                    (fun _ l2 _ -> ret (blockItem l2))))
            (fun s1 s2 ->
        ret (s1 @ s2)
        )
    recurse1 autoIndent'


let charListToString (cl:list<char>) = new String(List.toArray cl)

