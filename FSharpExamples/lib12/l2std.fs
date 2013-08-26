module L2Std
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Some helper types and functions

type comparisonOp =
    | Smaller
    | Equal
    | Greater
    
let compare a b =
    if a < b then Smaller
    else if a > b then Greater
    else Equal

type Take1 = 
    {
        take1Left: bool;
        take1This: bool;
        take1Right: bool;
    }

let take1Compared x y = {take1Left = x<y; take1This = (x=y); take1Right = x>y}
let take1ComparedNoEqual x y = {take1Left = x<y; take1This = false; take1Right = x>y}

type Take2 =
    {
        take2LeftLeft: bool;
        take2LeftThis: bool;
        take2LeftRight: bool;
        take2ThisLeft: bool;
        take2ThisThis: bool;
        take2ThisRight: bool;
        take2RightLeft: bool;
        take2RightThis: bool;
        take2RightRight: bool;
    }

let inline (^<|) f a = f a

type BitArray32 = int
let setBit offset (mask:BitArray32) : BitArray32 = (1<<<offset)|||mask
let testBit offset (mask:BitArray32) :bool = ((1<<<offset)&&&mask)<>0
let setLowBits size :BitArray32 = (1<<<size)-1

let inc x = x+1
let dec x = x-1

let mapFirst map (a, b) = (map a, b)
let mapSecond map (a, b) = (a, map b)
