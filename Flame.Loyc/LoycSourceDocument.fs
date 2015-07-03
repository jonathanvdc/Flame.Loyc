namespace Flame.Loyc

open Loyc
open Loyc.Syntax
open Flame
open Flame.Compiler

/// A source document wrapper for 
/// loyc source files (ISourceFile).
type LoycSourceDocument(file : ISourceFile) =
    static let fixIndex max alternative index =
        if index < 0 || index > max then
            alternative
        else
            index

    member this.SourceFile = 
        file

    member this.Slice startIndex length =
        let slice = file.Text.Slice(startIndex, length)
        let text  = string slice
        text.TrimEnd()

    member this.Length =
        file.Text.Count

    interface ISourceDocument with
        member this.Identifier = 
            file.FileName

        member this.Source =
            this.Slice 0 this.Length

        member this.ToGridPosition index =
            let pos = file.IndexToLine index
            new SourceGridPosition(pos.Line - 1, pos.PosInLine - 1)

        member this.GetLine index = 
            let thisLine  = file.LineToIndex (index + 1) |> fixIndex this.Length 0
            let lineCount = (this :> ISourceDocument).LineCount
            let nextLine  = if index + 2 >= lineCount then
                                this.Length
                            else
                                file.LineToIndex (index + 2) |> fixIndex this.Length this.Length
            this.Slice thisLine (nextLine - thisLine)

        member this.LineCount =
            let srcPos = file.IndexToLine (this.Length - 1)
            if srcPos.Line > 0 && srcPos.PosInLine > 0 then
                srcPos.Line + 1
            else
                0
