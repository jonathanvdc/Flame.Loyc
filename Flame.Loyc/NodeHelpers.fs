namespace Flame.Loyc

open Loyc
open Loyc.Syntax
open Flame
open Flame.Compiler
open Flame.Functional
open System.Collections.Generic

module NodeHelpers =
    let IsAssignmentSymbol value =
        value = CodeSymbols.Assign

    type LNode with
        member this.IsAssignmentNode =
            IsAssignmentSymbol(this.Name) && this.ArgCount = 2

    /// Gets the id node that is encapsulated by the given node, if any.
    /// Otherwise, None.
    let rec GetIdNode (node : LNode) =
        if node.IsId then
            Some node
        else if node.IsAssignmentNode then
            GetIdNode node.Args.[0]
        else
            None

    /// Gets the name of the id id node that is encapsulated by the given node, if any.
    /// Otherwise, None.
    let GetIdName (node : LNode) =
        match GetIdNode node with
        | Some node -> Some node.Name.Name
        | None      -> None

    /// Gets the value that is assigned to the leftmost node in the assignment expression, if any.
    /// Otherwise, None.
    let rec GetAssignedValueNode (node : LNode) =
        if node.IsAssignmentNode then
            match GetAssignedValueNode node.Args.[0] with
            | None              -> Some(node.Args.[1])
            | Some assignedNode -> Some(node.WithArgs([| assignedNode; node.Args.[1] |]) :> LNode)
        else
            None

    /// Converts the given Loyc `SourceRange` to a Flame `SourceLocation`.
    let ToSourceLocation (range : Loyc.Syntax.SourceRange) =
        let doc = new LoycSourceDocument(range.Source)
        new SourceLocation(doc, range.StartIndex, range.Length)