namespace Flame.Loyc

open Loyc
open Loyc.Syntax
open Flame
open Flame.Compiler
open Flame.Functional

/// Defines a node converter.
type INodeConverter =
    /// Tries to convert an expression node to an expression.
    /// A statement is treated as an expression that has type void.
    abstract member TryConvertExpression : LNode -> LocalScope -> (IExpression * LocalScope) option

    /// Tries to convert a type node to a type.
    abstract member TryConvertType : LNode -> LocalScope -> IType option

    /// Tries to convert the given type member node to a type member.
    /// Constructs like fields, properties and methods are type members.
    abstract member TryConvertTypeMember : LNode -> GlobalScope -> FunctionalType -> FunctionalType option

    /// Tries to convert a namespace member node to a namespace member.
    /// Things like types, namespaces and modules are namespace members.
    abstract member TryConvertNamespaceMember : LNode -> GlobalScope -> IFunctionalNamespace -> IFunctionalNamespace option

    /// Tries to convert the given attribute node to an attribute.
    abstract member TryConvertAttribute : GlobalScope -> LNode -> IAttribute option

[<AutoOpen>]
module NodeConverterExtensions =

    type INodeConverter with
        member private this.convertMember<'a>  (func : LNode -> GlobalScope -> 'a -> 'a option) (node : LNode) scope decl =
            match func node scope decl with
            | Some result -> result
            | None        ->
                let message = new LogEntry("Unknown node type", 
                                           "The node converter didn't know what to do with '" + node.Print() + "'.",
                                           NodeHelpers.ToSourceLocation node.Range)
                scope.Log.LogError(message) // TODO: log this in a somewhat functional way
                decl

        member this.ConvertExpression (node : LNode) (scope : LocalScope) =
            match this.TryConvertExpression node scope with
            | Some result -> result
            | None        -> 
                let message = if node.IsId then
                                    new LogEntry("Unrecognized identifier", 
                                                "Identifier '" + node.Name.Name + "' was not recognized.",
                                                NodeHelpers.ToSourceLocation node.Range)
                                else
                                    new LogEntry("Unknown node type", 
                                                "The node converter didn't know what to do with '" + node.Print() + "'. " + 
                                                "It was kind of hoping that the '" + node.Target.Print() + "' " + 
                                                "node would turn out to be some kind of known expression node.",
                                                NodeHelpers.ToSourceLocation node.Range)
                ExpressionBuilder.VoidError(message), scope

        member this.ConvertExpressions (nodes : LNode seq) (scope : LocalScope) =
            let foldExpr (exprs : IExpression list, scope : LocalScope) (item : LNode) =
                let convExpr, newScope = this.ConvertExpression item scope
                convExpr::exprs, scope
            let exprs, scope = nodes |> Seq.fold foldExpr ([], scope)
            exprs |> List.rev |> Seq.ofList, scope

        member this.ConvertType node scope = 
            match this.TryConvertType node scope with
            | Some result -> result
            | None        -> null

        /// Converts the given type member node to a type member.
        /// Constructs like fields, properties and methods are type members.
        member this.ConvertTypeMember node scope decl =
            this.convertMember this.TryConvertTypeMember node scope decl

        /// Converts a namespace member node to a namespace member.
        /// Things like types, namespaces and modules are namespace members.
        member this.ConvertNamespaceMember node scope decl =
            this.convertMember this.TryConvertNamespaceMember node scope decl