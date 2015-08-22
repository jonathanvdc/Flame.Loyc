namespace Flame.Loyc

open Loyc
open Loyc.Syntax
open Flame
open Flame.Compiler
open Pixie

/// A message sink that uses an underlying compiler log.
type CompilerLogMessageSink(log : ICompilerLog) =
    
    /// Gets the message sink's inner compiler log.
    member this.Log = log

    static member GetContextMarkupNode (context : obj) =
        match context with
        | :? SourceRange as ran  -> (NodeHelpers.ToSourceLocation ran).CreateDiagnosticsNode()
        | :? LNode as node       -> (NodeHelpers.ToSourceLocation node.Range).CreateDiagnosticsNode()
        | null                   -> new MarkupNode(NodeConstants.TextNodeType, "") :> IMarkupNode
        | _                      -> new MarkupNode(NodeConstants.RemarksNodeType, context.ToString()) :> IMarkupNode

    static member ToLogEntry (titleSplitter : string -> string * string) (context : obj) (message : string) =
        let name, contents = titleSplitter message
        let loc            = CompilerLogMessageSink.GetContextMarkupNode context
        let msgNode        = new MarkupNode("entry", Seq.ofArray [| new MarkupNode(NodeConstants.TextNodeType, contents) :> IMarkupNode; loc |])
        new LogEntry(name, msgNode)

    static member SplitEmptyTitle (msg : string) = ("", msg)

    /// Splits the message's title from the rest of the message based on its punctuation.
    static member SplitPunctuationTitle (msg : string) = 
        let punc = [| '.'; ';'; ','; '?'; '!'; '('; ')' |]
        let colonIndex = msg.IndexOf ':'
        if colonIndex < msg.IndexOfAny punc then
            msg.Substring(0, colonIndex).TrimEnd(), msg.Substring(colonIndex + 1).TrimStart()
        else
            "", msg

    member this.WriteEntry (level : Severity) (entry : LogEntry) =
        if level >= Severity.Error then
            log.LogError entry
        else if level >= Severity.Warning then
            log.LogWarning entry
        else if level >= Severity.Debug then
            log.LogMessage entry
        else
            log.LogEvent entry

    member this.Write (level : Severity) (context : obj) (message : string) =
        this.WriteEntry level (CompilerLogMessageSink.ToLogEntry CompilerLogMessageSink.SplitPunctuationTitle context message)

    interface IMessageSink with
        member this.IsEnabled level = 
            true

        member this.Write (level, context, format, args) = 
            this.Write level context (Localize.From(format, args))

        member this.Write (level, context, format) = 
            this.Write level context (Localize.From(format))

        member this.Write (level, context, format, arg0, arg1) = 
            this.Write level context (Localize.From(format, arg0, arg1))