
// Learn more about F# at http://fsharp.net
// F# for WPF project template at http://code.msdn.com/fs
// Template Version 0.1 Preview
open System
open System.Windows
open System.Windows.Controls
open LState
open LSUtl
open UIElementEvent

//let content = new TextBlock(Text = "Welcome to F#", HorizontalAlignment = HorizontalAlignment.Center, VerticalAlignment = VerticalAlignment.Center)
let content = new InkCanvas(HorizontalAlignment = HorizontalAlignment.Stretch, VerticalAlignment = VerticalAlignment.Stretch)
//let content = new Marquee(HorizontalAlignment = HorizontalAlignment.Stretch, VerticalAlignment = VerticalAlignment.Stretch)
let window = new Window(Title = "testMGui", Content = content)

let text = ref "Hello World"
(*
    What events do I get when I touch the screen with my finger and move?
    Can I reduce these mouse messages to a command stream?
*)

let test t1 = 
    getState ()
    >>= fun (_,e) ->
    match e with
    | MouseEvent(mbe, orig, zdir) ->
        text := e.ToString() 
    | MouseTransition(me,bdir) ->
        text := e.ToString() 
    | MouseMove(me) ->
        text := e.ToString() 
    | MouseWheel(mwe) ->
        text := e.ToString() 
    | TouchEvent(te, zdif) ->
        text := e.ToString() 
    | TouchTransition(te, bdir) ->
        text := e.ToString() 
    | TouchMove(te) -> 
        text := e.ToString(); 
    suspend (t1)      

let test2 () = recurse1 test

callbackDrivenState ((fun (s,e)->s), (fun s e -> (s,e))) content () (test2())


[<STAThread>] ignore <| (new Application()).Run window