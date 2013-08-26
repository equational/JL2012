module IBinaryHeap
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// inspired by someone, sorry didn't keep the link
 
//  _shiftDown(0);
 
let swap (heap:array<_>) i j =
    let tmp = heap.[i];
    heap.[i] <- heap.[j]
    heap.[j] <- tmp

let bubbleDown size (heap:array<_>) node =
    let nodeValue = heap.[node]
    let rec bubbleDown' node =
        let left_child = node*2+1;
        let right_child = node*2+2;
        let replace =
            if (right_child < size) then
                // if right size is ok, so is left
                if heap.[right_child] < heap.[left_child] then
                    if (nodeValue < heap.[left_child]) then
                        left_child
                    else
                        node
                else
                    if nodeValue < heap.[right_child] then
                        right_child
                    else
                        node
            else if left_child < size then
                if nodeValue < heap.[left_child] then
                    left_child
                else
                    node
            else
                node
        if (replace <> node) then
            heap.[node] <- heap.[replace];
            bubbleDown' replace 
        else
            heap.[node] <- nodeValue
    bubbleDown' node


let bubbleUp (heap:array<_>) node = 
    let rec bubbleUp' node =
        if (node <> 0) then 
            let parent = (node-1)/2 
            if heap.[node] >= heap.[parent] then
                swap heap node parent;
                bubbleUp' parent
    bubbleUp' node

let init size (heap:array<_>) = 
    let rec insertNext i =
        if i>=0 then
            bubbleDown size heap i;
            insertNext (i-1)
    insertNext (size/2)

let enqueue size (heap:array<_>) value =
    heap.[size] <- value;
    bubbleUp heap size 

let dequeue size (heap:array<_>) = 
    heap.[0] <- heap.[size-1];
    bubbleDown (size-1) heap 0

let decreaseKey size (heap:array<_>) newHead = 
    heap.[0] <- newHead;
    bubbleDown size heap 0

