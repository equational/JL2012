// Learn more about F# at http://fsharp.net

open LState
open LSList
open LRBTree
open TestParser



type
    Trade = {
        tradeIndex: int;
        tradeQuantity: int;
        tradePrice: double;
        tradeInstrumentIndex: int;
        }
and
    Instrument = {
        instrumentIndex: int;
        instrumentName: string;
        }
and
    TradeTable = Map<int,Trade>
and
    InstrumentTable = Map<int,Instrument>


type
    State = {
        instrumentTable: InstrumentTable;
        tradeTable: TradeTable;
        }

let applyTradeTable f = getState () >>= (fun state -> ret (f state.tradeTable))
let mapTradeTable f = mapState (fun state -> { state with tradeTable = f state.tradeTable })

let mapTrades f tradeTable = Map.map (fun index trade -> f trade) tradeTable
let mapTrade index f tradeTable = Map.add index (f (Map.find index tradeTable)) tradeTable
let updateTrades f = mapTradeTable (mapTrades f)
let updateTrade index f = mapTradeTable (mapTrade index f)
let selectTrade index = applyTradeTable

let data = [1;2;3;4;5]
let insert' x tree = insert (fun y -> {take1Left = x<y; take1This = (x=y); take1Right = x>y}) x tree

let tree = List.foldBack insert' data None
let join() = 
    mfold2 
        (fun x y -> {take2LeftLeft=true;take2LeftThis=true;take2LeftRight=true;take2ThisLeft=true;take2ThisThis=true;take2ThisRight=true;take2RightLeft=true;take2RightThis=true;take2RightRight=true})
        (fun x y l -> ret ((x,y)::l))
        tree
        tree
        []

let testTreeJoin () =
    printfn "data %A" (tree);
    printfn "data %A" (runLState (join()) ());

let main =
    testTreeJoin();
    testParser();
    System.Console.ReadLine()  |> ignore;

