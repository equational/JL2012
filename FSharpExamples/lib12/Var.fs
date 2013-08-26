module Var
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Unsucessful extension of a "free var" concept so that "triggers" can be set up on unassigned vars.
// The idea was to use it to support of more complex type inference engine.
// Yet I never pulled it completely through. One problem is the notion of level used in the classical type inference algo.

open L2Std
open LState
open LRBTree
open LSList

type
    VarId = int
and
    Level = int
and
    Var<'t> = 
        | Var   of  (VarId*Level)
        | Value of  't
and
    VarEntry<'t> = VarId*Var<'t>
and
    VarMap<'t> = Tree<VarEntry<'t>>
and
    Trigger<'s,'t> =
        | Trigger   of  EqId*ArgPos
        | Alias     of  VarId
and
    TriggerEntry<'s,'t> = VarId*Trigger<'s,'t>
and
    TriggerMap<'s,'t> = Tree<TriggerEntry<'s,'t>>
and
    EqId = int
and
    ArgPos = int
and
    ArgsSet = BitArray32
and
    EqOp<'s> = ArgsSet-> ArgPos -> LState<'s,unit>
and
    DoEqOp<'s> = unit->LState<'s,unit>
and
    Eq<'s> = {
        argsSet         :ArgsSet;
        eqOp            :EqOp<'s>;
        }
and
    EqEntry<'s> = EqId*Eq<'s>
and
    EqMap<'s> = Tree<EqEntry<'s>>
and
    Cache<'s,'t> = {
        varMap          :VarMap<'t>;
        triggerMap      :TriggerMap<'s,'t>;
        eqMap           :EqMap<'s>;
        nextVarCounter  :VarId;
        nextEqCounter   :EqId;
        level           :Level;
        }
and
    GetVarCache<'s,'t> = unit->LState<'s,Cache<'s,'t>>
and
    MapVarCache<'s,'t> = (Cache<'s,'t>->(LState<'s,Cache<'s,'t>>))->LState<'s,unit>
and
    GMCache<'s,'t> = GetVarCache<'s,'t>*MapVarCache<'s,'t>


let inline getVarMap cache = cache.varMap
let inline getTriggerMap cache = cache.triggerMap
let inline getEqMap cache = cache.eqMap
let inline getNextVarCounter cache = cache.nextVarCounter
let inline getNextEqCounter cache = cache.nextEqCounter
let inline getLevel cache = cache.level

let inline mapVarMap map cache = { cache with varMap = map cache.varMap ;}
let inline mapTriggerMap map cache = { cache with triggerMap = map cache.triggerMap ;}
let inline mapEqMap map cache = { cache with eqMap = map cache.eqMap ;}
let inline mapNextVarCounter map cache = { cache with nextVarCounter = map cache.nextVarCounter ;}
let inline mapNextEqCounter map cache = { cache with nextEqCounter = map cache.nextEqCounter ;}

//let private mapInnerState<'s,'t,'r> ((get, map):GMCache<'s,'t>) (mapRet:Cache<'s,'t>->LState<'s,'r*Cache<'s,'t>>) :LState<'s,'r>=
//    get ()
//    >>= mapRet
//    >>= (fun (r,newCache) ->
//        map (fun _ -> ret newCache) 
//        >>/ ret r)

let newCache = { varMap = None; triggerMap = None; eqMap = None; nextVarCounter=0; nextEqCounter=0; level=0}

let newVar<'s,'t> (gm:GMCache<'s,'t>) =
    mapInnerState gm (fun cache ->
            let newVar = getNextVarCounter cache
            let newCache = mapNextVarCounter inc cache
            ret (newVar, newCache))

let private  setVar<'s,'t> (gm:GMCache<'s,'t>) varId varValue =
    mapInnerState gm (fun cache ->
            let cmp (varId2,_) = take1ComparedNoEqual varId varId2
            let newCache = mapVarMap (insert cmp (varId, varValue)) cache
            ret ((), newCache))

let derefVar<'s,'t> (gm:GMCache<'s,'t>) (var:Var<'t>) =
    mapInnerState gm (fun cache ->
        let rec get ((varId,level) as varIdLevel) =
            let cmp (varId2,_) = take1Compared varId varId2
            match tryFindElement cmp cache.varMap with
            | Some (varId', varEntry) ->
                match varEntry with
                | Var otherVarIdLevel ->
                    get otherVarIdLevel
                | Value _ ->
                    varEntry
            | None ->
                Var varIdLevel
        let retVar = 
            match var with
            | Var varIdLevel ->
                get varIdLevel
            | Value t ->
                var
        ret (retVar, cache)
        )

let private  addAlias<'s,'t> (gm:GMCache<'s,'t>) varId otherVarId =
    mapInnerState gm (fun cache ->
            let cmp (varId2,_) = take1ComparedNoEqual varId varId2
            let newCache = mapTriggerMap (insert cmp (varId, (Alias otherVarId))) cache
            ret ((), newCache))


let private  setTrigger<'s,'t> (gm:GMCache<'s,'t>) varId eqId argPos =
    mapInnerState gm (fun cache ->
            let cmp (varId2,_) = take1ComparedNoEqual varId varId2
            let newCache = mapTriggerMap (insert cmp (varId, (Trigger (eqId,argPos)))) cache
            ret ((), newCache))

let private  newEq'<'s,'t> (gm:GMCache<'s,'t>) (eqOp:EqOp<'s>) =
    mapInnerState gm (fun cache ->
    let newEqId = getNextEqCounter cache
    let eq = {argsSet= 0; eqOp=eqOp; }
    let cmp (eqId2,_) = take1ComparedNoEqual newEqId eqId2
    let newCache = (mapNextEqCounter dec) (mapEqMap (insert cmp (newEqId, eq)) cache)
    ret (newEqId, newCache))

let getEq<'s,'t> (gm:GMCache<'s,'t>) eqId =
    mapInnerState gm (fun cache ->
            let cmp (eqId2,_) = take1Compared eqId eqId2
            match tryFindElement cmp cache.eqMap with
            | Some (eqId', eq) ->
                ret((eq.argsSet, eq.eqOp), cache)
        )


let newEq<'s,'t> (gm:GMCache<'s,'t>) (eqOp:EqOp<'s>) (eqArgs:list<Var<'t>>) =
    let indexing = [(0:ArgPos)..(List.length(eqArgs)-1)]
    let indexed = List.zip indexing eqArgs
    let processBound1 eqId (argPos:ArgPos, var:Var<'t>) =
        derefVar gm var
        >>= (fun var ->
        match var with
        | Value _ ->
            ret ()
        | Var (varId, _) ->
            setTrigger gm varId eqId argPos
            >>/ ret ()
        )
    let processBound2 eqId (argPos:ArgPos, var:Var<'t>) =
        derefVar gm var
        >>= (fun var ->
        match var with
        | Value _ ->
            getEq gm eqId
            >>= (fun (mask, eqOp) ->
            eqOp mask argPos)
        | Var _ ->
            ret ()
        )
    newEq' gm eqOp
    >>= (fun newEqId -> 
        iterR (processBound1 newEqId) indexed
        >>/ iterR (processBound2 newEqId) indexed
        >>/ ret newEqId)
                
            
let private bindEqArg<'s,'t> (gm:GMCache<'s,'t>) (eqId, argPos) =
    mapInnerState gm (fun cache ->
        let cmp (eqId2,_) = take1Compared eqId eqId2
        let updateOp (Some (eqId', eq)) = 
            let newEq = { eq with argsSet = setBit argPos eq.argsSet; }
            ((eq.argsSet, eq.eqOp), (eqId', newEq))
        let ((argSet, eqOp), newEqMap) = update cmp updateOp cache.eqMap
        let newCache = { cache with eqMap = newEqMap; }
        ret ((argSet, eqOp), newCache))
    >>= (fun (argSet, eqOp) -> 
    eqOp argSet argPos)
 
let private triggerOnSet<'s,'t> (gm:GMCache<'s,'t>) varId =
    mapInnerState gm (fun cache ->
        let rec accEqs varId acc = 
            let cmp (varId2,_) = take1Compared varId varId2
            let processTrigger (_, trigger) acc =
                match trigger with
                | Trigger (eqId,argPos) ->
                    ret ((eqId,argPos)::acc)
                | Alias otherVarId ->
                    accEqs otherVarId acc
            mfold cmp processTrigger cache.triggerMap acc
        accEqs varId []
        >>= (fun eqs -> ret (eqs, cache)))
    >>= iterR (bindEqArg gm)


let joinVarValue<'s,'t> (gm:GMCache<'s,'t>) (varId:VarId) (value:'t) =
    setVar gm varId (Value value)
    >>/ triggerOnSet gm varId 

let joinVarVar<'s,'t> (gm:GMCache<'s,'t>) (matchValue:('t -> 't -> LState<'s,unit>)) (var1:Var<'t>) (var2:Var<'t>) =
    derefVar gm var1
    >>= (fun var1 ->
    derefVar gm var2
    >>= (fun var2 ->
    match var1, var2 with
    | (Var (varId1,level1)), (Var (varId2,level2)) ->
        let setVar' vid1 vid2 var2 =
            setVar gm vid1 var2
            >>/ addAlias gm vid2 vid1

        if level1 < level2 then
            setVar' varId2 varId1 var1
        else if level2 < level1 then
            setVar' varId1 varId2 var2
        else if varId1 < varId2 then
            setVar' varId2 varId1 var1
        else if varId2 < varId1 then
            setVar' varId1 varId2 var2
        else
            ret ()
    | (Var (varId1,_)), (Value t2) ->
        joinVarValue gm varId1 t2
    | (Value t1), (Var (varId2,_)) ->
        joinVarValue gm varId2 t1
    | (Value t1), (Value t2) ->
        matchValue t1 t2
    ))

//let copyVar 