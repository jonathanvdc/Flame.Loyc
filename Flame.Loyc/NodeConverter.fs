namespace Flame.Loyc

open Loyc
open Loyc.Syntax
open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Functional
open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Linq

type NodeConverter(callConverters       : IReadOnlyDictionary<Symbol, seq<CallConverter>>, 
                   literalConverters    : IReadOnlyDictionary<Type, LiteralConverter>, 
                   typeConverters       : IReadOnlyDictionary<Symbol, seq<TypeConverter>>,
                   typeMemberConverters : IReadOnlyDictionary<Symbol, seq<TypeMemberConverter>>,
                   nsMemberConverters   : IReadOnlyDictionary<Symbol, seq<NamespaceMemberConverter>>,
                   attrConverters       : IReadOnlyDictionary<Symbol, seq<AttributeConverter>>,
                   identifierConverters : IdentifierConverter seq) =

    let withSource (node : LNode) (arg : (IExpression * LocalScope) option)  =
        match arg with
        | None -> 
            None
        | Some (expr, scope) ->
            Some (ExpressionBuilder.Source (NodeHelpers.ToSourceLocation node.Range) expr, scope)

    member private this.tryConvertMember<'a> (dict : IReadOnlyDictionary<Symbol, seq<ScopedNodeConverter<'a * GlobalScope, 'a * GlobalScope>>>) (node : LNode) scope decl =
        let name = node.Name
        if not(dict.ContainsKey name) then
            None 
        else
            let candidates = dict.Item name
            let result = candidates |> Seq.tryFind (fun item -> item.Matches node)

            match result with
            | Some converter -> Some (converter.Convert (this :> INodeConverter) node (decl, scope))
            | None           -> None

    member private this.tryConvertNode<'Scope, 'Result> (dict : IReadOnlyDictionary<Symbol, seq<ScopedNodeConverter<'Result, 'Scope>>>) (node : LNode) scope =
        let name = node.Name
        if not(dict.ContainsKey name) then
            None
        else
            let candidates = dict.Item name
            let result = candidates |> Seq.tryFind (fun item -> item.Matches node)

            match result with
            | Some converter -> converter.Convert (this :> INodeConverter) node scope |> Some
            | None           -> None

    member private this.tryConvertNullableNode<'Scope, 'Result when 'Result : null> (dict : IReadOnlyDictionary<Symbol, seq<ScopedNodeConverter<'Result, 'Scope>>>) (node : LNode) scope =
        match this.tryConvertNode dict node scope with
        | Some null -> None
        | x         -> x

    /// Converts the given node to an expression, given
    /// a global scope, and cleans up its scope afterward.
    member this.ConvertScopedExpression node (globalScope : GlobalScope) =
        this.ConvertExpression node (new LocalScope(globalScope)) ||> ExpressionBuilder.Scope

    /// Converts the given node sequence to an expression, based
    /// on the given global scope.
    member this.ConvertScopedExpressionSequence nodes (globalScope : GlobalScope) =
        ExpressionConverters.ConvertUnscopedSequence ExpressionBuilder.ExpressionBlock (this :> INodeConverter) nodes (new LocalScope(globalScope))
            ||> ExpressionBuilder.Scope

    /// Converts a list of nodes into a top-level namespace.
    member this.ConvertCompilationUnit (scope : GlobalScope) (declAsm : IAssembly) (nodes : seq<LNode>) =
        let ns = new FunctionalNamespace(new FunctionalMemberHeader(""), declAsm) :> IFunctionalNamespace
        nodes |> Seq.fold (fun (ns, newScope) item -> this.ConvertNamespaceMember item newScope ns) (ns, scope)
              |> fst
         
    member private this.TryConvertIdNode (node : LNode) (scope : LocalScope) =
        identifierConverters |> Seq.map (fun f -> f (this :> INodeConverter) node scope)
                             |> Seq.filter (fun x -> x.IsSome)
                             |> Seq.map (fun x -> x.Value)
                             |> Seq.tryFind (ExpressionConverters.Constant true)
                             |> (fun x -> match x with None -> None | Some y -> Some (y, scope))

    member private this.TryConvertLiteralNode (node : LNode) (scope : LocalScope) =
        if node.Value = null then
            Some (ExpressionBuilder.Null, scope)
        else
            let t = node.Value.GetType()
            if literalConverters.ContainsKey t then
                Some ((literalConverters.Item t) node.Value, scope)
            else
                None

    member private this.TryConvertCallNode (node : LNode) (scope : LocalScope) : (IExpression * LocalScope) option =
        match this.tryConvertNode callConverters node scope with
        | None ->
            match this.TryConvertExpression node.Target scope with
            | Some(target, scope) ->
                Some (ExpressionConverters.ConvertInvocation target (this :> INodeConverter) node scope)
            | None -> None
        | result -> result

    member this.TryConvertExpression (node : LNode) (scope : LocalScope) : (IExpression * LocalScope) option =
        if node.IsCall then
            this.TryConvertCallNode node scope |> withSource node
        else if node.IsId then
            this.TryConvertIdNode node scope |> withSource node
        else
            this.TryConvertLiteralNode node scope |> withSource node

    member this.TryConvertType (node : LNode) (scope : LocalScope) =
        match this.tryConvertNullableNode typeConverters node scope with
        | None   -> 
            if node.IsId then
                match scope.Global.Binder.Bind node.Name.Name with
                | null   -> None
                | result -> Some result
            else
                None
        | result -> result

    /// Tries to convert an attribute node.
    member this.TryConvertAttribute (scope : GlobalScope) (node : LNode) =
        this.tryConvertNullableNode attrConverters node scope

    /// Tries to convert the given type member node to a type member.
    /// Constructs like fields, properties and methods are type members.
    member this.TryConvertTypeMember node scope declType =
        this.tryConvertMember typeMemberConverters node scope declType

    /// Tries to convert a namespace member node to a namespace member.
    /// Things like types, namespaces and modules are namespace members.
    member this.TryConvertNamespaceMember node scope declNs =
        this.tryConvertMember nsMemberConverters node scope declNs 

    interface INodeConverter with
        member this.TryConvertExpression node scope = this.TryConvertExpression node scope

        member this.TryConvertType node scope = this.TryConvertType node scope

        /// Converts the given type member node to a type member.
        /// Constructs like fields, properties and methods are type members.
        member this.TryConvertTypeMember node scope declType = this.TryConvertTypeMember node scope declType

        /// Converts a namespace member node to a namespace member.
        /// Things like types, namespaces and modules are namespace members.
        member this.TryConvertNamespaceMember node scope declNs = this.TryConvertNamespaceMember node scope declNs

        /// Tries to convert an attribute node.
        member this.TryConvertAttribute scope node = this.TryConvertAttribute scope node

    static member private MakePair key value = key, value

    static member private ToLiteralConverterPair<'a> (f : 'a -> IExpression) =
        NodeConverter.MakePair typeof<'a> (fun (x : obj) -> unbox x |> f)

    static member private ToDictionary<'a, 'b when 'a : equality> (sequence : seq<'a * 'b>) =
        let getKey (key, _) = key
        let getValue (_, value) = value

        new ReadOnlyDictionary<'a, 'b>(sequence.ToDictionary(getKey, getValue)) :> IReadOnlyDictionary<'a, 'b>

    static member private ToMultiDictionary<'a, 'b when 'a : equality> (sequence : seq<'a * 'b>)  =
        let getKey (key, _) = key
        let getValue (_, value) = value
        let grouped = sequence |> Seq.groupBy getKey
                               |> Seq.map (fun (key, items) -> key, items |> Seq.map getValue)

        NodeConverter.ToDictionary grouped

    static member DefaultNodeConverter = 
        let makePair = NodeConverter.MakePair
        let makeLit = NodeConverter.ToLiteralConverterPair
        let voidConst = ExpressionConverters.Constant ExpressionBuilder.Void
        let literalConverters = Seq.ofArray [|
                                                makeLit ExpressionBuilder.ConstantInt8;
                                                makeLit ExpressionBuilder.ConstantInt16;
                                                makeLit ExpressionBuilder.ConstantInt32;
                                                makeLit ExpressionBuilder.ConstantInt64;

                                                makeLit ExpressionBuilder.ConstantUInt8;
                                                makeLit ExpressionBuilder.ConstantUInt16;
                                                makeLit ExpressionBuilder.ConstantUInt32;
                                                makeLit ExpressionBuilder.ConstantUInt64;

                                                makeLit ExpressionBuilder.ConstantFloat32;
                                                makeLit ExpressionBuilder.ConstantFloat64;

                                                makeLit ExpressionBuilder.ConstantBoolean;
                                                makeLit ExpressionBuilder.ConstantChar;
                                                makeLit ExpressionBuilder.ConstantString;

                                                makePair typeof<``void``> voidConst;
                                                makePair typeof<NoValue>  voidConst
                                            |] 
                                |> NodeConverter.ToDictionary

        let makeBinOp loycOp op = makePair loycOp (ExpressionBuilder.Binary op |> ExpressionConverters.DefineScopedBinaryOperator)
        let makeBinAssignmentOp loycOp op = makePair loycOp (ExpressionBuilder.Binary op |> ExpressionConverters.DefineBinaryAssignmentOperator)

        let callConverters = Seq.ofArray [|
                                             makeBinOp CodeSymbols.Add Operator.Add;
                                             makeBinAssignmentOp CodeSymbols.AddSet Operator.Add;
                                             makeBinOp CodeSymbols.Sub Operator.Subtract;
                                             makeBinAssignmentOp CodeSymbols.SubSet Operator.Subtract;
                                             makeBinOp CodeSymbols.Mul Operator.Multiply;
                                             makeBinAssignmentOp CodeSymbols.MulSet Operator.Multiply;
                                             makeBinOp CodeSymbols.Div Operator.Divide;
                                             makeBinAssignmentOp CodeSymbols.DivSet Operator.Divide;
                                             makeBinOp CodeSymbols.Mod Operator.Remainder;
                                             makeBinAssignmentOp CodeSymbols.ModSet Operator.Remainder;

                                             makeBinOp CodeSymbols._Concat Operator.Concat;
                                             makeBinAssignmentOp CodeSymbols.ConcatSet Operator.Concat;

                                             makeBinOp CodeSymbols.And Operator.LogicalAnd;
                                             makeBinOp CodeSymbols.Or Operator.LogicalOr;

                                             makeBinOp CodeSymbols.AndBits Operator.And;
                                             makeBinAssignmentOp CodeSymbols.AndBitsSet Operator.And;
                                             makeBinOp CodeSymbols.OrBits Operator.Or;
                                             makeBinAssignmentOp CodeSymbols.OrBitsSet Operator.Or;
                                             makeBinOp CodeSymbols.XorBits Operator.Xor;
                                             makeBinAssignmentOp CodeSymbols.XorBitsSet Operator.Xor;

                                             makeBinOp CodeSymbols.Shl Operator.LeftShift;
                                             makeBinAssignmentOp CodeSymbols.ShlSet Operator.LeftShift;
                                             makeBinOp CodeSymbols.Shr Operator.RightShift;
                                             makeBinAssignmentOp CodeSymbols.ShrSet Operator.RightShift;

                                             makeBinOp CodeSymbols.Eq Operator.CheckEquality;
                                             makeBinOp CodeSymbols.Neq Operator.CheckInequality;
                                             makeBinOp CodeSymbols.GE Operator.CheckGreaterThanOrEqual;
                                             makeBinOp CodeSymbols.LE Operator.CheckLessThanOrEqual;
                                             makeBinOp CodeSymbols.GT Operator.CheckGreaterThan;
                                             makeBinOp CodeSymbols.LT Operator.CheckLessThan;

                                             makePair CodeSymbols.NullCoalesce (ExpressionBuilder.CoalesceNull |> ExpressionConverters.DefineBinaryOperator);
                                             makePair CodeSymbols.NullCoalesceSet (ExpressionBuilder.CoalesceNull |> ExpressionConverters.Constant |> ExpressionConverters.DefineBinaryAssignmentOperator);

                                             makePair CodeSymbols._UnaryPlus (ExpressionConverters.DefineUnaryOperator id);
                                             makePair CodeSymbols.Not (ExpressionConverters.DefineUnaryOperator ExpressionBuilder.Not);
                                             makePair CodeSymbols.NotBits (ExpressionConverters.DefineUnaryOperator ExpressionBuilder.Not);
                                             makePair CodeSymbols._Negate (ExpressionConverters.DefineUnaryOperator ExpressionBuilder.Negate);

                                             makePair CodeSymbols.PreInc (ExpressionConverters.DefineScopedUnaryOperator ExpressionBuilder.PrefixIncrement);
                                             makePair CodeSymbols.PreDec (ExpressionConverters.DefineScopedUnaryOperator ExpressionBuilder.PrefixDecrement);
                                             makePair CodeSymbols.PostInc (ExpressionConverters.DefineScopedUnaryOperator ExpressionBuilder.PostfixIncrement);
                                             makePair CodeSymbols.PostDec (ExpressionConverters.DefineScopedUnaryOperator ExpressionBuilder.PostfixDecrement);

                                             makePair CodeSymbols.Cast (ExpressionConverters.DefineScopedTypeBinaryOperator ExpressionBuilder.Cast);

                                             makePair CodeSymbols.QuestionMark ExpressionConverters.SelectConverter;
                                             makePair CodeSymbols.If ExpressionConverters.SelectConverter; 
                                             makePair CodeSymbols.If ExpressionConverters.IfConverter;
                                             makePair CodeSymbols.While ExpressionConverters.WhileConverter;
                                             makePair CodeSymbols.DoWhile ExpressionConverters.DoWhileConverter;
                                             makePair CodeSymbols.For ExpressionConverters.ForConverter;

                                             makePair CodeSymbols.Braces ExpressionConverters.VoidBlockConverter;
                                             makePair CodeSymbols.Comma ExpressionConverters.CommaConverter;
                                             makePair CodeSymbols.Semicolon (ExpressionConverters.DefineUnaryOperator ExpressionBuilder.Pop);

                                             makePair CodeSymbols.Break (ExpressionConverters.DefineScopedNullaryOperator ExpressionBuilder.Break);
                                             makePair CodeSymbols.Continue (ExpressionConverters.DefineScopedNullaryOperator ExpressionBuilder.Continue);

                                             makePair CodeSymbols.Return (ExpressionConverters.DefineNullaryOperator ExpressionBuilder.ReturnVoid); 
                                             makePair CodeSymbols.Return (ExpressionConverters.DefineUnaryOperator ExpressionBuilder.Return);

                                             makePair CodeSymbols.Result (ExpressionConverters.DefineUnaryOperator ExpressionBuilder.Result);

                                             makePair CodeSymbols.Throw (ExpressionConverters.DefineUnaryOperator ExpressionBuilder.Throw);

                                             makePair CodeSymbols.Assign (ExpressionConverters.DefineScopedBinaryOperator ExpressionBuilder.Assign);
                                             makePair CodeSymbols.QuickBind ExpressionConverters.QuickbindConverter;
                                             makePair CodeSymbols.QuickBindSet ExpressionConverters.QuickbindSetConverter;

                                             makePair CodeSymbols.Var ExpressionConverters.VariableDeclarationConverter;

                                             makePair CodeSymbols.Default ExpressionConverters.DefaultExpressionConverter;
                                             
                                             makePair CodeSymbols.Dot ExpressionConverters.MemberAccessConverter;

                                             makePair CodeSymbols.Error ExpressionConverters.ErrorConverter
                                             makePair CodeSymbols.Warning ExpressionConverters.WarningConverter
                                         |]
                                |> NodeConverter.ToMultiDictionary

        let makeType loycOp flameType = makePair loycOp (ExpressionConverters.DefineConstantConverter flameType)
        let typeConverters = [|
                                 makeType CodeSymbols.Int8 PrimitiveTypes.Int8;
                                 makeType CodeSymbols.Int16 PrimitiveTypes.Int16;
                                 makeType CodeSymbols.Int32 PrimitiveTypes.Int32;
                                 makeType CodeSymbols.Int64 PrimitiveTypes.Int64;

                                 makeType CodeSymbols.UInt8 PrimitiveTypes.UInt8;
                                 makeType CodeSymbols.UInt16 PrimitiveTypes.UInt16;
                                 makeType CodeSymbols.UInt32 PrimitiveTypes.UInt32;
                                 makeType CodeSymbols.UInt64 PrimitiveTypes.UInt64;

                                 makeType CodeSymbols.Single PrimitiveTypes.Float32;
                                 makeType CodeSymbols.Double PrimitiveTypes.Float64;

                                 makeType CodeSymbols.Bool PrimitiveTypes.Boolean;
                                 makeType CodeSymbols.Char PrimitiveTypes.Char;
                                 makeType CodeSymbols.String PrimitiveTypes.String;
                                 makeType CodeSymbols.Void PrimitiveTypes.Void;

                                 makePair CodeSymbols.Object ExpressionConverters.RootTypeConverter;

                                 makePair CodeSymbols.Dot MemberConverters.ScopeOperatorConverter;
                                 makePair CodeSymbols.ColonColon MemberConverters.ScopeOperatorConverter;
                                 makePair CodeSymbols.Of MemberConverters.ArrayTypeConverter;
                             |] |> Seq.ofArray 
                                |> NodeConverter.ToMultiDictionary

        let typeMemberConverters =  [|
                                        CodeSymbols.Var,      MemberConverters.FieldDeclarationConverter
                                        CodeSymbols.Braces,   MemberConverters.TypeMemberBlockConverter
                                        CodeSymbols.Fn,       MemberConverters.MethodDeclarationConverter MemberConverters.ConvertMethodDeclaration
                                        CodeSymbols.Cons,     MemberConverters.MethodDeclarationConverter MemberConverters.ConvertConstructorDeclaration
                                        CodeSymbols.Property, MemberConverters.PropertyDeclarationConverter
                                        CodeSymbols.Import,   MemberConverters.ImportConverter
                                    |] |> Seq.ofArray
                                       |> NodeConverter.ToMultiDictionary

        let nsMemberConverters   =  [|
                                        CodeSymbols.Class,     Seq.empty |> MemberConverters.TypeDeclarationConverter 
                                        CodeSymbols.Struct,    Seq.singleton PrimitiveAttributes.Instance.ValueTypeAttribute |> MemberConverters.TypeDeclarationConverter
                                        CodeSymbols.Interface, Seq.singleton PrimitiveAttributes.Instance.InterfaceAttribute |> MemberConverters.TypeDeclarationConverter
                                        CodeSymbols.Namespace, MemberConverters.NamespaceDeclarationConverter
                                        CodeSymbols.Braces,    MemberConverters.NamespaceMemberBlockConverter
                                        CodeSymbols.Import,    MemberConverters.ImportConverter
                                    |] |> Seq.ofArray
                                       |> NodeConverter.ToMultiDictionary

        let attrConverters       =  [|
                                        CodeSymbols.Private,     AttributeConverters.AccessAttributeConverter AccessModifier.Private
                                        CodeSymbols.Protected,   AttributeConverters.AccessAttributeConverter AccessModifier.Protected
                                        CodeSymbols.ProtectedIn, AttributeConverters.AccessAttributeConverter AccessModifier.ProtectedAndAssembly
                                        CodeSymbols.Internal,    AttributeConverters.AccessAttributeConverter AccessModifier.Assembly
                                        CodeSymbols.Public,      AttributeConverters.AccessAttributeConverter AccessModifier.Public
                                        CodeSymbols.Const,       AttributeConverters.ConstantAttributeConverter PrimitiveAttributes.Instance.ConstantAttribute
                                        CodeSymbols.Abstract,    AttributeConverters.ConstantAttributeConverter PrimitiveAttributes.Instance.AbstractAttribute
                                        CodeSymbols.Virtual,     AttributeConverters.ConstantAttributeConverter PrimitiveAttributes.Instance.VirtualAttribute
                                    |] |> Seq.ofArray
                                       |> NodeConverter.ToMultiDictionary

        let identConverters      =  [|
                                        ExpressionConverters.ConvertVoidIdentifier
                                        ExpressionConverters.ConvertLocalIdentifier
                                        ExpressionConverters.ConvertInstanceIdentifier
                                        ExpressionConverters.ConvertStaticIdentifier
                                    |] |> Seq.ofArray

        NodeConverter(callConverters, 
                      literalConverters,
                      typeConverters,
                      typeMemberConverters,
                      nsMemberConverters,
                      attrConverters,
                      identConverters)