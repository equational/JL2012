module UIElementEvent
// The MIT License (MIT)Copyright (c) 2013 James Litsios Permission is hereby granted, free of charge, to any person obtaining a copy ofthis software and associated documentation files (the "Software"), to deal inthe Software without restriction, including without limitation the rights touse, copy, modify, merge, publish, distribute, sublicense, and/or sell copies ofthe Software, and to permit persons to whom the Software is furnished to do so,subject to the following conditions:The above copyright notice and this permission notice shall be included in allcopies or substantial portions of the Software.THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ORIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESSFOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS ORCOPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHERIN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR INCONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Reactive GUI adapter

open LState

open System.Windows
open System.Windows.Controls


type uieZDirection =
    | EventUp
    | EventDown

type uieOrigin =
    | DefaultOrigin
    | LeftButton
    | RightButton

type uieBDirection =
    | Enter
    | Leave

type uieEvent =
    | MouseEvent        of Input.MouseButtonEventArgs*uieOrigin*uieZDirection
    | MouseTransition   of Input.MouseEventArgs*uieBDirection
    | MouseMove         of Input.MouseEventArgs
    | MouseWheel        of Input.MouseWheelEventArgs
    | TouchEvent        of Input.TouchEventArgs*uieZDirection
    | TouchTransition   of Input.TouchEventArgs*uieBDirection
    | TouchMove         of Input.TouchEventArgs

let react2uie (uie:UIElement) f =
    uie.MouseDown           |> Event.map(fun mbe -> f (MouseEvent (mbe, DefaultOrigin, EventDown))) |> ignore
    uie.MouseEnter          |> Event.map(fun me -> f (MouseTransition (me, Enter))) |> ignore
    uie.MouseLeave          |> Event.map(fun me -> f (MouseTransition (me, Leave))) |> ignore
    uie.MouseLeftButtonDown |> Event.map(fun mbe -> f (MouseEvent (mbe, LeftButton, EventDown))) |> ignore
    uie.MouseLeftButtonUp   |> Event.map(fun mbe -> f (MouseEvent (mbe, LeftButton, EventUp))) |> ignore
    uie.MouseMove           |> Event.map(fun me -> f (MouseMove me)) |> ignore
    uie.MouseRightButtonDown |> Event.map(fun mbe -> f (MouseEvent (mbe, RightButton, EventDown))) |> ignore
    uie.MouseRightButtonUp  |> Event.map(fun mbe -> f (MouseEvent (mbe, RightButton, EventUp))) |> ignore
    uie.MouseUp             |> Event.map(fun mbe -> f (MouseEvent (mbe, DefaultOrigin, EventUp))) |> ignore
    uie.MouseWheel          |> Event.map(fun mwe -> f (MouseWheel mwe)) |> ignore
    uie.TouchDown           |> Event.map(fun te -> f (TouchEvent (te, EventUp))) |> ignore
    uie.TouchEnter          |> Event.map(fun te -> f (TouchTransition (te, Enter))) |> ignore
    uie.TouchLeave          |> Event.map(fun te -> f (TouchTransition (te, Leave))) |> ignore
    uie.TouchMove           |> Event.map(fun te -> f (TouchMove te)) |> ignore
    uie.TouchUp             |> Event.map(fun te -> f (TouchEvent (te, EventDown))) |> ignore



let makeEventRunner (get, set) s0 m =
    let s' = ref s0
    let m' = ref m
    fun event ->
        let s = set (!s') event
        let lret = runLState (!m') s
        List.iter 
            (function
                | Now (Success _), ns ->
                    s' := get ns
                | Suspend nm, ns ->
                    s' := get ns
                    m' := nm
                | Now (Failure _), ns ->
                    // do nothing
                    ())
            lret

let callbackDrivenState (get,set) uie  s m =
    react2uie uie (makeEventRunner (get,set) s m)

