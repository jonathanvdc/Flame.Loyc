namespace Flame.Loyc

open Loyc
open Loyc.Syntax
open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Functional

module ExpressionConverters =
    /// Creates a function that returns the given constant.
    let Constant<'a, 'b> (result : 'b) = fun (_ : 'a) -> result

    /// Creates a new node converter from the given predicate and converter function.
    let CreateConverter<'a> predicate convert =
        new SingleNodeConverter<'a>(predicate, convert)

    /// Creates a predicate function that matches nodes with the given number
    /// of arguments.
    let MatchesArgumentCount count = 
        fun (node : LNode) -> node.ArgCount = count

    /// Creates an n-ary node converter with the given converter.
    let CreateNAryConverter n =
        CreateConverter (MatchesArgumentCount n)

    /// Defines a nullary node converter with the given converter.
    let CreateNullaryConverter converter = 
        CreateNAryConverter 0 converter

    /// Defines a unary node converter with the given converter.
    let CreateUnaryConverter converter = 
        CreateNAryConverter 1 converter

    /// Defines a binary node converter with the given converter.
    let CreateBinaryConverter converter = 
        CreateNAryConverter 2 converter
            
    /// Defines a binary node converter with the given converter.
    let CreateTernaryConverter converter = 
        CreateNAryConverter 3 converter

    /// Defines a converter that simply returns a constant.
    let DefineConstantConverter value =
        CreateConverter (Constant true) (value |> Constant |> Constant |> Constant)

    /// Defines a nullary operator with the given converter.
    let DefineScopedNullaryOperator converter = 
        let conv (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            converter scope, scope

        CreateNullaryConverter conv

    /// Defines a unary operator that acts on expressions.
    let DefineScopedUnaryOperator converter = 
        let conv (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let expr, newScope = parent.ConvertExpression node.Args.[0] scope
            converter newScope expr, newScope

        CreateUnaryConverter conv

    /// Defines a binary operator with the given converter.
    let DefineScopedBinaryOperator converter = 
        let conv (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let        newScope = scope
            let expr1, newScope = parent.ConvertExpression node.Args.[0] newScope
            let expr2, newScope = parent.ConvertExpression node.Args.[1] newScope
            converter newScope expr1 expr2, newScope

        CreateBinaryConverter conv

    /// Defines a binary operator with the given converter.
    let DefineScopedTypeBinaryOperator converter = 
        let conv (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let      newScope = scope
            let lhs, newScope = parent.ConvertExpression node.Args.[0] newScope
            let rhs           = parent.ConvertType node.Args.[1] newScope
            converter newScope lhs rhs, newScope

        CreateBinaryConverter conv

    /// Defines a ternary operator with the given converter.
    let DefineScopedTernaryOperator converter = 
        let conv (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let        newScope = scope
            let expr1, newScope = parent.ConvertExpression node.Args.[0] newScope
            let expr2, newScope = parent.ConvertExpression node.Args.[1] newScope
            let expr3, newScope = parent.ConvertExpression node.Args.[2] newScope
            converter newScope expr1 expr2 expr3, newScope

        CreateTernaryConverter conv

    /// Defines a nullary operator with the given converter.
    let DefineNullaryOperator value =
        DefineScopedNullaryOperator (Constant value)

    /// Defines a unary operator that acts on expressions.
    let DefineUnaryOperator converter =
        DefineScopedUnaryOperator (Constant converter)

    /// Defines a binary operator with the given converter.
    let DefineBinaryOperator converter =
        DefineScopedBinaryOperator (Constant converter)

    /// Defines a ternary operator with the given converter.
    let DefineTernaryOperator converter =
        DefineScopedTernaryOperator (Constant converter)

    /// Defines a binary assignment operator based on the given converter.
    /// Said converter need not perform the assignment itself: it should compute
    /// the right-hand side of the assignment instead.
    let DefineBinaryAssignmentOperator (converter : LocalScope -> IExpression -> IExpression -> IExpression) =
        DefineScopedBinaryOperator (fun scope lhs rhs -> ExpressionBuilder.Assign scope lhs (converter scope lhs rhs))

    /// Converts a scoped expression. 
    /// Note that an IExpression is returned, instead of an IExpression * LocalScope,
    /// as this function should not be exposed as a converter directly.
    /// Doing so anyway and registering it will most likely end up in infinite recursion.
    let ConvertScopedExpression (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
        parent.ConvertExpression node scope.ChildScope ||> ExpressionBuilder.Scope

    /// A converter for if-then expressions, which are of type void.
    let IfConverter = 
        let convIf (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let cond, newScope = parent.ConvertExpression node.Args.[0] scope
            let scopedBody     = ConvertScopedExpression parent node.Args.[1] newScope
            ExpressionBuilder.If scope cond scopedBody, newScope
        CreateBinaryConverter convIf

    /// A converter for select/if-then-else expressions.
    let SelectConverter =
        let convIfElse (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let cond, newScope = parent.ConvertExpression node.Args.[0] scope
            let ifBody         = ConvertScopedExpression parent node.Args.[1] newScope
            let elseBody       = ConvertScopedExpression parent node.Args.[2] newScope
            ExpressionBuilder.Select scope cond ifBody elseBody, newScope
        CreateTernaryConverter convIfElse

    /// A converter for while expressions.
    let WhileConverter = 
        let convWhile (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let cond, newScope = parent.ConvertExpression node.Args.[0] scope
            let scopedBody     = ConvertScopedExpression parent node.Args.[1] newScope
            ExpressionBuilder.While cond scopedBody, newScope
        CreateBinaryConverter convWhile

    /// A converter for do-while expressions.
    let DoWhileConverter = 
        let convDoWhile (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let scopedBody     = ConvertScopedExpression parent node.Args.[0] scope
            let cond, newScope = parent.ConvertExpression node.Args.[1] scope
            ExpressionBuilder.DoWhile scopedBody cond, newScope
        CreateBinaryConverter convDoWhile

    /// A converter for `for`-loop expressions.
    let ForConverter = 
        let convFor (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let       newScope = scope.ChildScope
            let init, newScope = parent.ConvertExpression node.Args.[0] newScope
            let cond, newScope = parent.ConvertExpression node.Args.[1] newScope
            let delta          = ConvertScopedExpression parent node.Args.[2] newScope
            let body           = ConvertScopedExpression parent node.Args.[3] newScope
            ExpressionBuilder.Scope (ExpressionBuilder.For init cond delta body) newScope, scope

        CreateNAryConverter 4 convFor

    /// Converts an unscoped sequence of nodes by invoking the given block builder.
    /// The resulting block *does not* manage its own scope.
    /// Instead, it works with whatever scope it's passed.
    let ConvertUnscopedSequence (blockBuilder : seq<IExpression> -> IExpression) (parent : INodeConverter) (nodes : seq<LNode>) (scope : LocalScope) =
        let appendItem (list, scope) elem =
            let convExpr, newScope = parent.ConvertExpression elem scope
            convExpr :: list, newScope

        let foldedItems, bodyScope = nodes |> Seq.fold appendItem ([], scope)
        blockBuilder (List.rev foldedItems), bodyScope

    /// Converts a block node by invoking the given block builder.
    /// The resulting block *does not* manage its own scope.
    /// Instead, it works with whatever scope it's passed.
    let ConvertUnscopedBlock (blockBuilder : seq<IExpression> -> IExpression) (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
        ConvertUnscopedSequence blockBuilder parent node.Args scope

    /// Converts a block node by invoking the given block builder.
    /// The resulting block *does* manage its own scope.
    let ConvertScopedBlock (blockBuilder : seq<IExpression> -> IExpression) (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
        ConvertUnscopedBlock blockBuilder parent node scope.ChildScope ||> ExpressionBuilder.Scope, scope

    /// A block converter for void blocks.
    let VoidBlockConverter = CreateConverter (Constant true) (ConvertScopedBlock ExpressionBuilder.VoidBlock)

    /// A block converter for comma blocks.
    let CommaConverter = CreateConverter (Constant true) (ConvertUnscopedBlock ExpressionBuilder.Comma)

    /// Creates an error expression that represents an error (`#error`) node.
    let ConvertError (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
        let message     = string node.Args.[0].Value
        let sourceRange = NodeHelpers.ToSourceLocation node.Range
        let sourceRange = if sourceRange.Length = 0 then new SourceLocation(sourceRange.Document, sourceRange.Position, 1) else sourceRange
        let entry       = new LogEntry("Invalid syntax", message, sourceRange)
        ExpressionBuilder.VoidError entry, scope

    let ErrorConverter = CreateUnaryConverter ConvertError

    let private convertQuickbind (valueGetter : LNode -> LNode) (nameGetter : LNode -> LNode) (parent : INodeConverter) (node : LNode) (scope : LocalScope) = 
        let valueNode      = valueGetter node
        let name           = (nameGetter node).Name.Name
        let expr, newScope = parent.ConvertExpression valueNode scope
        ExpressionBuilder.Quickbind newScope expr name

    /// A converter for quickbind expressions.
    let QuickbindConverter = 
        CreateBinaryConverter (convertQuickbind (fun node -> node.Args.[0]) (fun node -> node.Args.[1]))

    /// A converter for quickbind-set expressions.
    let QuickbindSetConverter = 
        CreateBinaryConverter (convertQuickbind (fun node -> node.Args.[1]) (fun node -> node.Args.[0]))

    /// Converts a single variable definition, with the given type inference function.
    let ConvertVariableDefinition (inferType : IExpression -> IType) (parent : INodeConverter) (node : LNode) (scope : LocalScope) = 
        match NodeHelpers.GetIdNode node with
        | None -> 
            let message = new LogEntry("Invalid variable declaration",
                                       "A variable declaration must reference an identifier node, " +
                                       "which was not found in '" + node.Print() + "'.")
            ExpressionBuilder.VoidError message, scope
        | Some identifier ->
            match (inferType null, NodeHelpers.GetAssignedValueNode node) with
            | (null, None) -> 
                let message = new LogEntry("Invalid variable declaration",
                                           "A variable declaration that infers its type must be assigned a value, " +
                                           "which was not found in '" + node.Print() + "'.")
                ExpressionBuilder.VoidError message, scope

            | (null, Some assignedVal) ->
                let expr, newScope  = parent.ConvertExpression assignedVal scope
                let local, newScope = (inferType expr, identifier.Name.Name) ||> ExpressionBuilder.DeclareLocal newScope
                ExpressionBuilder.Assign newScope local expr, newScope

            | (varType, None) ->
                ExpressionBuilder.DeclareLocal scope varType identifier.Name.Name

            | (varType, Some assignedVal) ->
                let local, newScope = ExpressionBuilder.DeclareLocal scope varType identifier.Name.Name
                let expr, newScope  = parent.ConvertExpression assignedVal newScope
                ExpressionBuilder.Assign newScope local expr, newScope
    
    /// A node converter for variable declarations.
    let VariableDeclarationConverter =
        let convDeclVar (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let typeNode = node.Args.[0]

            let inferType = if typeNode.Name = CodeSymbols.Missing then 
                                fun (arg : IExpression) -> arg.get_TypeOrNull()
                            else 
                                parent.ConvertType typeNode scope |> Constant

            let foldDef (init, currentScope) item =
                let result, currentScope = ConvertVariableDefinition inferType parent item currentScope
                ExpressionBuilder.Initialize init result, currentScope

            node.Args.Slice(1) |> Seq.fold foldDef (ExpressionBuilder.Void, scope)

        CreateConverter (Constant true) convDeclVar

    /// A type converter that tries to retrieve the root type.
    let RootTypeConverter =
        let getRootType (scope : LocalScope) = scope.Global.Environment.RootType
        CreateConverter (Constant true) (getRootType |> Constant |> Constant)

    /// Converts an invocation expression.
    let ConvertInvocation (target : IExpression) (parent : INodeConverter) (node : LNode) (scope : LocalScope) : IExpression * LocalScope =
        let args, scope = parent.ConvertExpressions node.Args scope
        ExpressionBuilder.Invoke scope target args, scope
