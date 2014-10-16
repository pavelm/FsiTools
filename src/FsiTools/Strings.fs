module Strings

open System

let join sep (items : string seq) = String.Join(sep,items)

let copyClip (text : string) =
    System.Windows.Forms.Clipboard.SetText(text)

let quote = sprintf "\"%s\"" 
