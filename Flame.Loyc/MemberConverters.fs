namespace Flame.Loyc

open Loyc
open Loyc.Syntax
open Flame
open Flame.Build
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Functional
open ExpressionConverters
open LazyHelpers

module MemberConverters =
    let ReadAttributes (parent : INodeConverter) (node : LNode) (scope : GlobalScope) =
        let attrs    = node.Attrs |> Seq.map (parent.TryConvertAttribute scope)
                                  |> Seq.filter (fun x -> x.IsSome)
                                  |> Seq.map (fun x -> x.Value)

        let isStatic = node.Attrs |> Seq.exists (fun x -> x.Name = CodeSymbols.Static)
        isStatic, attrs

    let ConvertGenericParameter (parent : INodeConverter) (scope : GlobalScope) (node : LNode) (declMember : IGenericMember) =
        let name = node.Name.Name
        let descParam = new DescribedGenericParameter(name, declMember)
        descParam :> IGenericParameter

    let rec ReadName (parent : INodeConverter) (node : LNode) (scope : GlobalScope) =
        if node.Name = CodeSymbols.Of then
            let tParams            = node.Args.Slice(1) |> Seq.map (ConvertGenericParameter parent scope)
            let name, innerTParams = ReadName parent node.Args.[0] scope
            
            name, Seq.append innerTParams tParams
        else if node.Name = CodeSymbols.Dot then
            let lName, lParams     = ReadName parent node.Args.[0] scope
            let rName, rParams     = ReadName parent node.Args.[1] scope
            MemberExtensions.CombineNames(lName, rName), Seq.append lParams rParams
        else
            node.Name.Name, Seq.empty

    let private ConvertSingleField (inferType : IExpression -> IType) (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        match NodeHelpers.GetIdNode node with
        | None -> 
            let message = new LogEntry("Invalid variable declaration",
                                       "A variable declaration must reference an identifier node, " +
                                       "which was not found in '" + node.Print() + "'.")
            None, None, lazy (ExpressionBuilder.VoidError message)
        | Some identifier ->
            let name = identifier.Name.Name
            match (inferType null, NodeHelpers.GetAssignedValueNode node) with
            | (null, None) -> 
                let message = new LogEntry("Invalid variable declaration",
                                           "A variable declaration that infers its type must be assigned a value, " +
                                           "which was not found in '" + node.Print() + "'.")
                Some name, None, lazy (ExpressionBuilder.VoidError message)

            | (varType, None) ->
                Some name, Some (lazy varType), lazy null

            | (_, Some assignedVal) ->
                let name         = identifier.Name.Name
                let expr         = lazy (parent.ConvertExpression assignedVal (new LocalScope(scope)) |> fst)
                let inferredType = expr |~ inferType
                Some name, Some inferredType, expr

    let ConvertFieldDeclaration (parent : INodeConverter) (node : LNode) (declType : FunctionalType, scope : GlobalScope) =
        let isStatic, attrs = ReadAttributes parent node scope
        let typeNode = node.Args.[0]

        let inferType = if typeNode.Name = CodeSymbols.Missing then 
                            fun (arg : IExpression) -> arg.get_TypeOrNull()
                        else 
                            parent.ConvertType typeNode (new LocalScope(scope)) |> Constant

        let defineField name fieldType isStatic (attrs : seq<IAttribute>) initValue (parentType : IType) =
            let header = new FunctionalMemberHeader(name, attrs)
            new FunctionalField(header, parentType, isStatic, fieldType, initValue) :> IField

        let foldDef (parentType : FunctionalType) item =
            match ConvertSingleField inferType parent item scope parentType with
            | (None, _, expr) 
            | (_, None, expr) -> 
                // Something went horribly wrong. Abort here.
                // TODO: log this!
                parentType
            | (Some name, Some fieldType, expr) ->
                defineField name fieldType isStatic attrs expr |> parentType.WithField

        node.Args.Slice(1) |> Seq.fold foldDef declType

    let ConvertParameterDeclaration (parent : INodeConverter) (scope : LocalScope) (node : LNode) =
        let varType = parent.ConvertType node.Args.[0] scope
        let name, _ = ReadName parent node.Args.[1] scope.Global
        new DescribedParameter(name, varType) :> IParameter

    let private ConvertCommonMethodDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        let isStatic, attrs = ReadAttributes parent node scope
        let name, tParams   = ReadName parent node.Args.[1] scope
        let fMethod         = new FunctionalMethod(new FunctionalMemberHeader(name, attrs), declType, isStatic)
        let fMethod         = tParams |> Seq.fold (fun (state : FunctionalMethod) item -> state.WithGenericParameter item) fMethod
        let tempLocalScope  = new LocalScope(scope)
        let fMethod         = node.Args.[2].Args |> Seq.fold (fun (state : FunctionalMethod) item -> state.WithParameter (lazy (ConvertParameterDeclaration parent tempLocalScope item))) fMethod

        let getBody declMethod =
            let localScope      = new LocalScope(new FunctionScope(scope, declMethod))
            let body            = parent.ConvertExpression node.Args.[3] localScope ||> ExpressionBuilder.Scope 
                                                                                     |> ExpressionBuilder.ToStatement
            body

        fMethod.WithBody getBody

    let AutoReturn (retType : IType) (body : IStatement) =
        if retType = null || retType.Equals(PrimitiveTypes.Void) then
            new BlockStatement([| body; new ReturnStatement() :> IStatement |]) :> IStatement
        else
            body

    let ConvertMethodDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        let fMethod = ConvertCommonMethodDeclaration parent node scope declType
        let retType = lazy (parent.ConvertType node.Args.[0] (new LocalScope(scope)))
        let fMethod = fMethod.WithReturnType retType
        (fun _ -> AutoReturn retType.Value fMethod.Body) |> fMethod.WithBody :> IMethod

    let ConvertConstructorDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        let fMethod = ConvertCommonMethodDeclaration parent node scope declType
        let fMethod = fMethod.AsConstructor 
        (fun _ -> AutoReturn PrimitiveTypes.Void fMethod.Body) |> fMethod.WithBody :> IMethod

    let MethodDeclarationConverter conv =
        let convertMethod (parent : INodeConverter) (node : LNode) (declType : FunctionalType, scope : GlobalScope) =
            let func = conv parent node scope
            declType.WithMethod func
        let recognizeMethod (node : LNode) = node.ArgCount = 4
        new TypeMemberConverter(recognizeMethod, convertMethod)

    /// A type member converter for field declarations.
    let FieldDeclarationConverter =
        let recognizeField (node : LNode) = node.ArgCount > 1
        new TypeMemberConverter(recognizeField, ConvertFieldDeclaration)

    let ConvertTypeDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declNs : INamespace) =
        let isStatic, attrs = ReadAttributes parent node scope
        let newAttrs = if isStatic then Seq.singleton PrimitiveAttributes.Instance.StaticTypeAttribute |> Seq.append attrs else attrs
        let name, tParams = ReadName parent node.Args.[0] scope
        let fType = new FunctionalType(new FunctionalMemberHeader(name, newAttrs), declNs)
        let fType = tParams |> Seq.fold (fun (state : FunctionalType) item -> state.WithGenericParameter item) fType
        let fType = node.Args.Slice(2) |> Seq.fold (fun (state : FunctionalType) item -> parent.ConvertTypeMember item scope state) fType
        fType :> IType
    
    /// A namespace member converter for type declarations.
    let TypeDeclarationConverter = 
        let recognizeType (node : LNode) = node.ArgCount = 3
        let convDecl parent node (declNs : IFunctionalNamespace, scope) =
            let t = ConvertTypeDeclaration parent node scope
            declNs.WithType t

        new NamespaceMemberConverter(recognizeType, convDecl)

    /// Converts a namespace declaration.
    let ConvertNamespaceDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declNs : INamespaceBranch) =
        let _, attrs = ReadAttributes parent node scope
        let name, _ = ReadName parent node.Args.[0] scope
        let fNs = new FunctionalNamespace(new FunctionalMemberHeader(MemberExtensions.CombineNames(declNs.FullName, name), attrs), declNs.DeclaringAssembly)
        let fType = node.Args.Slice(2) |> Seq.fold (fun (state : IFunctionalNamespace) item -> parent.ConvertNamespaceMember item scope state) (fNs :> IFunctionalNamespace)
        fType :> INamespaceBranch
    
    /// A namespace member converter for namespace declarations.
    let NamespaceDeclarationConverter = 
        let recognizeNs (node : LNode) = node.ArgCount = 3
        let convDecl parent node (declNs : IFunctionalNamespace, scope) =
            let ns = ConvertNamespaceDeclaration parent node scope
            declNs.WithNamespace ns

        new NamespaceMemberConverter(recognizeNs, convDecl)

    let ConvertMemberBlock<'a> (selector : INodeConverter -> LNode -> GlobalScope -> 'a -> 'a) (parent : INodeConverter) (node : LNode) (decl : 'a, scope : GlobalScope) =
        let conv = selector parent
        node.Args |> Seq.fold (fun state item -> conv item scope state) decl

    let NamespaceMemberBlockConverter =
        new NamespaceMemberConverter(Constant true, ConvertMemberBlock (fun (parent : INodeConverter) -> parent.ConvertNamespaceMember))

    let TypeMemberBlockConverter =
        new TypeMemberConverter(Constant true, ConvertMemberBlock (fun (parent : INodeConverter) -> parent.ConvertTypeMember))
    