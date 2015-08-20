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
open MemberHelpers
open System

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
            let message = new LogEntry("Invalid field declaration",
                                       "A field declaration must reference an identifier node, " +
                                       "which was not found in '" + node.Print() + "'.")
            None, None, lazy (ExpressionBuilder.VoidError message)
        | Some identifier ->
            let name = identifier.Name.Name
            match (inferType null, NodeHelpers.GetAssignedValueNode node) with
            | (null, None) -> 
                let message = new LogEntry("Invalid field declaration",
                                           "A field declaration that infers its type must be assigned a value, " +
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

        let defineField name fieldType isStatic (attrs : seq<IAttribute>) initValue srcLoc (parentType : IType) =
            let header = new FunctionalMemberHeader(name, attrs, srcLoc)
            new FunctionalField(header, parentType, isStatic, fieldType, initValue) :> IField

        let foldDef (parentType : FunctionalType) item =
            match ConvertSingleField inferType parent item scope parentType with
            | (None, _, expr) 
            | (_, None, expr) -> 
                // Something went horribly wrong. Abort here.
                // TODO: log this!
                parentType
            | (Some name, Some fieldType, expr) ->
                defineField name fieldType isStatic attrs expr (NodeHelpers.ToSourceLocation item.Range) |> parentType.WithField

        node.Args.Slice(1) |> Seq.fold foldDef declType, scope

    let ConvertParameterDeclaration (parent : INodeConverter) (scope : LocalScope) (node : LNode) =
        let varType = parent.ConvertType node.Args.[0] scope
        let name, _ = ReadName parent node.Args.[1] scope.Global
        new DescribedParameter(name, varType) :> IParameter

    let private ConvertCommonMethodDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        let isStatic, attrs = ReadAttributes parent node scope
        let name, tParams   = ReadName parent node.Args.[1] scope
        let fMethod         = new FunctionalMethod(new FunctionalMemberHeader(name, attrs, NodeHelpers.ToSourceLocation node.Args.[1].Range), declType, isStatic)
        let fMethod         = tParams |> Seq.fold (fun (state : FunctionalMethod) item -> state.WithGenericParameter item) fMethod
        let tempLocalScope  = new LocalScope(scope)
        let fMethod         = node.Args.[2].Args |> Seq.fold (fun (state : FunctionalMethod) item -> state.WithParameter (lazy (ConvertParameterDeclaration parent tempLocalScope item))) fMethod

        let getBody declMethod =
            if node.ArgCount > 3 then
                let localScope      = new LocalScope(new FunctionScope(scope, declMethod))
                let body            = parent.ConvertExpression node.Args.[3] localScope ||> ExpressionBuilder.Scope 
                                                                                         |> ExpressionBuilder.ToStatement
                body
            else
                EmptyStatement.Instance :> IStatement

        fMethod.WithBody getBody

    /// Appends a `return(void);` statement to the given statement, provided the given
    /// type is either `null` or `void`.
    let AutoReturn (retType : IType) (body : IStatement) =
        if retType = null || retType.Equals(PrimitiveTypes.Void) then
            new BlockStatement([| body; new ReturnStatement() :> IStatement |]) :> IStatement
        else
            body

    let ConvertMethodDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        let fMethod = ConvertCommonMethodDeclaration parent node scope declType
        let retType = lazy (parent.ConvertType node.Args.[0] (new LocalScope(scope)))
        let fMethod = fMethod.WithReturnType retType
        let fMethod = fMethod.WithBaseMethods (InferBaseMethods (scope.GetAllMembers >> OfType))
        (fun (x : IMethod) -> AutoReturn x.ReturnType (fMethod.CreateBody x)) |> fMethod.WithBody :> IMethod

    let ConvertConstructorDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        let fMethod = ConvertCommonMethodDeclaration parent node scope declType
        let fMethod = fMethod.AsConstructor 
        (fun (x : IMethod) -> AutoReturn PrimitiveTypes.Void (fMethod.CreateBody x)) |> fMethod.WithBody :> IMethod

    /// Creates a type member converter based on the given method definition converter.
    let MethodDeclarationConverter conv =
        let convertMethod (parent : INodeConverter) (node : LNode) (declType : FunctionalType, scope : GlobalScope) =
            let func = conv parent node scope
            declType.WithMethod func, scope
        let recognizeMethod (node : LNode) = node.ArgCount = 3 || node.ArgCount = 4
        new TypeMemberConverter(recognizeMethod, convertMethod)

    /// A type member converter for field declarations.
    let FieldDeclarationConverter =
        let recognizeField (node : LNode) = node.ArgCount > 1
        new TypeMemberConverter(recognizeField, ConvertFieldDeclaration)

    /// Parses the given symbol into an accessor type.
    let GetAccessorType (symbol : Symbol) =
        if symbol = CodeSymbols.get then
            Some AccessorType.GetAccessor
        else if symbol = CodeSymbols.set then
            Some AccessorType.SetAccessor
        else if symbol = CodeSymbols.add then
            Some AccessorType.AddAccessor
        else if symbol = CodeSymbols.remove then
            Some AccessorType.RemoveAccessor
        else
            None

    /// Gets an accessor's return type and parameters, based on its accessor type and the enclosing property's return type and parameters.
    let GetAccessorSignature (accType : AccessorType) (propType : Lazy<IType>) (indexerParams : Lazy<IParameter seq>) : Lazy<IType> * Lazy<IParameter seq> =
        if accType = AccessorType.GetAccessor then
            propType, indexerParams
        else
            lazy PrimitiveTypes.Void, lazy Seq.append (evalLazy indexerParams) ([| new DescribedParameter("value", evalLazy propType) |])

    /// Converts an accessor declaration.
    let ConvertAccessorDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (accType : AccessorType) (declProp : IProperty) : IAccessor =
        let isStatic, attrs = ReadAttributes parent node scope
        if isStatic then
            let staticNode = node.Attrs |> Seq.find (fun x -> x.Name = CodeSymbols.Static)
            scope.Log.LogError(new LogEntry("Invalid 'static' attribute", 
                                            "'static' attributes cannot be applied to accessors directly. " + 
                                            "Apply them to the enclosing property instead.", 
                                            NodeHelpers.ToSourceLocation staticNode.Range))
        let acc = new FunctionalAccessor(new FunctionalMemberHeader(accType.ToString().ToLower() + "_" + declProp.Name, attrs, NodeHelpers.ToSourceLocation node.Range), declProp, accType)
        let retType, parameters = GetAccessorSignature accType (lazy declProp.PropertyType) (lazy (declProp.GetIndexerParameters() |> Seq.ofArray))
        let acc = acc.WithReturnType retType
        let acc = evalLazy parameters |> Seq.fold (fun (x : FunctionalAccessor) y -> x.WithParameter (lazy y)) acc
        let acc = acc.WithBaseMethods ((InferBaseAccessors (scope.GetAllMembers >> OfType<ITypeMember, IProperty>)) >> Seq.cast)

        let getBody declMethod =
            let localScope      = new LocalScope(new FunctionScope(scope, declMethod))
            let body            = parent.ConvertExpression node.Args.[0] localScope ||> ExpressionBuilder.Scope 
                                                                                     |> ExpressionBuilder.ToStatement
            AutoReturn declMethod.ReturnType body

        acc.WithBody getBody :> IAccessor


    /// Converts a property "member", i.e. an accessor or a block that contains accessors.
    let rec ConvertPropertyMember (parent : INodeConverter) (scope : GlobalScope) (prop : FunctionalProperty) (node : LNode) : FunctionalProperty =
        if node.Name = CodeSymbols.Braces then
            node.Args |> Seq.fold (ConvertPropertyMember parent scope) prop
        else
            match GetAccessorType node.Name with
            | None         ->
                scope.Log.LogError(new LogEntry("Unrecognized accessor type", 
                                                "'" + node.Name.Name + "' was not recognized as a type of accessor. ", 
                                                NodeHelpers.ToSourceLocation node.Target.Range))
                prop
            | Some accType ->
                let createAcc = ConvertAccessorDeclaration parent node scope accType
                prop.WithAccessor createAcc

    /// Converts a single property declaration.
    let ConvertPropertyDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (name : string) (declType : IType) =
        let isStatic, attrs = ReadAttributes parent node scope
        let propType = lazy match parent.TryConvertType node.Args.[0] (new LocalScope(scope)) with
                            | None    ->
                                let message = new LogEntry("Unresolved type",
                                                           "Could not resolve the declared type of property '" + name + 
                                                           "' of '" + scope.TypeNamer declType + "'.",
                                                           NodeHelpers.ToSourceLocation node.Args.[0].Range)
                                scope.Log.LogError(message)
                                null
                            | Some ty -> ty

        let prop = new FunctionalProperty(new FunctionalMemberHeader(name, attrs, NodeHelpers.ToSourceLocation node.Args.[1].Range), declType, isStatic)
        let prop = prop.WithPropertyType propType
        // TODO: add indexer parameters (if any)
        let prop = ConvertPropertyMember parent scope prop node.Args.[2]
        prop :> IProperty

    /// A type member converter for property declarations.
    let PropertyDeclarationConverter =
        let convertProp (parent : INodeConverter) (node : LNode) (declType : FunctionalType, scope : GlobalScope) =
            match NodeHelpers.GetIdName node.Args.[1] with
            | None      -> 
                let message = new LogEntry("Unnamed property", 
                                           "A property was declared without an identifier.",
                                           NodeHelpers.ToSourceLocation node.Args.[1].Range)
                scope.Log.LogError(message)
                declType, scope
            | Some name ->
                ConvertPropertyDeclaration parent node scope name |> declType.WithProperty, scope
                
        let recognizeProperty (node : LNode) = node.ArgCount = 3
        new TypeMemberConverter(recognizeProperty, convertProp)

    /// Converts a type declaration.
    let ConvertTypeDeclaration (kindAttributes : IAttribute seq) (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declNs : INamespace) =
        let isStatic, attrs = ReadAttributes parent node scope
        let inferredAttrs = Seq.append kindAttributes attrs
        let newAttrs = if isStatic then 
                           Seq.singleton PrimitiveAttributes.Instance.StaticTypeAttribute |> Seq.append inferredAttrs 
                       else 
                           inferredAttrs
        let name, tParams = ReadName parent node.Args.[0] scope
        let baseTypes     = node.Args.[1].Args |> Seq.map (fun x -> lazy parent.ConvertType x (new LocalScope(scope)))
        let fType = new FunctionalType(new FunctionalMemberHeader(name, newAttrs, NodeHelpers.ToSourceLocation node.Args.[0].Range), declNs)
        let fType = baseTypes |> Seq.fold (fun (state : FunctionalType) item -> state.WithBaseType item) fType
        let fType = tParams |> Seq.fold (fun (state : FunctionalType) item -> state.WithGenericParameter item) fType
        let fType = node.Args.Slice(2) |> Seq.fold (fun (state : FunctionalType, newScope) item -> parent.ConvertTypeMember item newScope state) (fType, scope)
                                       |> fst
        fType :> IType
    
    /// A namespace member converter for type declarations.
    let TypeDeclarationConverter (kindAttributes : IAttribute seq) = 
        let recognizeType (node : LNode) = node.ArgCount = 3
        let convDecl parent node (declNs : IFunctionalNamespace, scope) =
            let t = ConvertTypeDeclaration kindAttributes parent node scope
            declNs.WithType t, scope

        new NamespaceMemberConverter(recognizeType, convDecl)

    /// Converts a namespace declaration.
    let ConvertNamespaceDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declNs : INamespaceBranch) =
        let _, attrs = ReadAttributes parent node scope
        let name, _ = ReadName parent node.Args.[0] scope
        let fNs = new FunctionalNamespace(new FunctionalMemberHeader(MemberExtensions.CombineNames(declNs.FullName, name), attrs, NodeHelpers.ToSourceLocation node.Args.[0].Range), declNs.DeclaringAssembly)
        let fType = node.Args.Slice(2) |> Seq.fold (fun (state : IFunctionalNamespace, newScope) item -> parent.ConvertNamespaceMember item newScope state) (fNs :> IFunctionalNamespace, scope.Binder.UseNamespace fNs |> scope.WithBinder)
                                       |> fst
        fType :> INamespaceBranch
    
    /// A namespace member converter for namespace declarations.
    let NamespaceDeclarationConverter = 
        let recognizeNs (node : LNode) = node.ArgCount = 3
        let convDecl parent node (declNs : IFunctionalNamespace, scope) =
            let ns = ConvertNamespaceDeclaration parent node scope
            declNs.WithNamespace ns, scope

        new NamespaceMemberConverter(recognizeNs, convDecl)

    let ConvertMemberBlock<'a> (selector : INodeConverter -> LNode -> GlobalScope -> 'a -> ('a * GlobalScope)) (parent : INodeConverter) (node : LNode) (decl : 'a, scope : GlobalScope) =
        let conv = selector parent
        node.Args |> Seq.fold (fun (state, newScope) item -> conv item newScope state) (decl, scope)

    let NamespaceMemberBlockConverter =
        new NamespaceMemberConverter(Constant true, ConvertMemberBlock (fun (parent : INodeConverter) -> parent.ConvertNamespaceMember))

    let TypeMemberBlockConverter =
        new TypeMemberConverter(Constant true, ConvertMemberBlock (fun (parent : INodeConverter) -> parent.ConvertTypeMember))

    /// A converter for `import` "expressions".
    let ImportConverter<'a> =
        let conv (parent : INodeConverter) (node : LNode) (ns : 'a, scope : GlobalScope) =
            ns, scope.Binder.UseNamespace (NodeHelpers.ToTypeName node.Args.[0]) |> scope.WithBinder
        let matchNode (x : LNode) = 
            x.Args.Count = 1

        new ScopedNodeConverter<'a * GlobalScope, 'a * GlobalScope>(matchNode, conv)
    
    /// A converter for types that are named by using a scope operator ('.' or '::').
    let ScopeOperatorConverter =
        let getSubtype (ty : IType) (name : TypeName) =
            match ty with
            | :? INamespace as ns -> ns.GetTypes() |> Array.tryFind (fun x -> x.Name = name.Name)
            | _                   -> None
                
        let convTy (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            match parent.TryConvertType node.Args.[0] scope with
            | Some ty -> 
                match getSubtype ty (NodeHelpers.ToTypeName node.Args.[1]) with
                | Some x -> x
                | None   -> NodeHelpers.ToTypeName node |> scope.Global.Binder.Bind
            | None    -> NodeHelpers.ToTypeName node |> scope.Global.Binder.Bind

        CreateBinaryConverter convTy

    /// A converter that converts array suffixes.
    let ArrayTypeConverter =
        let convTy (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let rank = CodeSymbols.CountArrayDimensions node.Args.[0].Name
            match parent.TryConvertType node.Args.[1] scope with
            | None    -> null
            | Some ty -> ty.MakeArrayType(rank) :> IType

        let matches (node : LNode) =
            node.ArgCount = 2 && CodeSymbols.IsArrayKeyword node.Args.[0].Name

        CreateConverter matches convTy