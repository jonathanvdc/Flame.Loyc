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
    let ReadAttributes (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (attrs : IAttribute seq) =
        let attrs    = node.Attrs |> Seq.fold (fun state attr -> parent.ConvertAttribute attr scope state) attrs
        let isStatic = node.Attrs |> Seq.exists (fun x -> x.Name = CodeSymbols.Static)
        isStatic, attrs

    let ConvertGenericParameter (parent : INodeConverter) (scope : GlobalScope) (node : LNode) (declMember : IGenericMember) =
        let name = node.Name.Name
        let descParam = new DescribedGenericParameter(name, declMember)
        descParam :> IGenericParameter

    let AliasGenericParameters (genMember : IGenericMember) (scope : GlobalScope) = 
        let foldParam (binder : FunctionalBinder) (tParam : IGenericParameter) = binder.AliasType (new TypeName(tParam.Name)) (lazy (tParam :> IType))
        genMember.GetGenericParameters() |> Seq.fold foldParam scope.Binder
                                         |> scope.WithBinder

    let rec ReadName (parent : INodeConverter) (node : LNode) (scope : GlobalScope) =
        if node.Name = CodeSymbols.Of then
            let tParams declMember = node.Args.Slice(1) |> Seq.map (fun node -> ConvertGenericParameter parent scope node declMember)
            let name, innerTParams = ReadName parent node.Args.[0] scope
            
            name, fun declMember -> Seq.append (innerTParams declMember) (tParams declMember)
        else if node.Name = CodeSymbols.Dot then
            let lName, lParams     = ReadName parent node.Args.[0] scope
            let rName, rParams     = ReadName parent node.Args.[1] scope
            MemberExtensions.CombineNames(lName, rName), fun declMember -> Seq.append (lParams declMember) (rParams declMember)
        else
            node.Name.Name, fun _ -> Seq.empty

    let private ConvertSingleField (inferType : LocalScope -> IExpression -> IType) (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        match NodeHelpers.GetIdNode node with
        | None -> 
            let message = new LogEntry("Invalid field declaration",
                                       "A field declaration must reference an identifier node, " +
                                       "which was not found in '" + node.Print() + "'.")
            None, (lazy null), lazy (ExpressionBuilder.VoidError message)
        | Some identifier ->
            let name = identifier.Name.Name
            let localScope = new LocalScope(AliasGenericParameters declType scope)
            match (inferType localScope null, NodeHelpers.GetAssignedValueNode node) with
            | (null, None) -> 
                let message = new LogEntry("Invalid field declaration",
                                           "A field declaration that infers its type must be assigned a value, " +
                                           "which was not found in '" + node.Print() + "'.")
                Some name, (lazy null), lazy (ExpressionBuilder.VoidError message)

            | (varType, None) ->
                Some name, (lazy varType), lazy null

            | (_, Some assignedVal) ->
                let name         = identifier.Name.Name
                let expr         = lazy (parent.ConvertExpression assignedVal localScope |> fst)
                let inferredType = expr |~ inferType localScope
                Some name, inferredType, expr

    let ConvertFieldDeclaration (parent : INodeConverter) (node : LNode) (declType : FunctionalType, scope : GlobalScope) =
        let isStatic, attrs = ReadAttributes parent node scope []
        let typeNode = node.Args.[0]

        let inferType declScope (expr : IExpression) = 
            if typeNode.Name = CodeSymbols.Missing then 
                expr.get_TypeOrNull()
            else 
                parent.ConvertType typeNode declScope

        let defineField name fieldType isStatic (attrs : seq<IAttribute>) initValue srcLoc (parentType : IType) =
            let header = new FunctionalMemberHeader(name, attrs, srcLoc)
            new FunctionalField(header, parentType, isStatic, fieldType, initValue) :> IField

        let foldDef (parentType : FunctionalType) item =
            let conv declType =
                match ConvertSingleField inferType parent item scope declType with
                | (None, fieldType, expr) ->
                    defineField "__bad_field" fieldType isStatic attrs expr (NodeHelpers.ToSourceLocation item.Range) declType
                | (Some name, fieldType, expr) ->
                    defineField name fieldType isStatic attrs expr (NodeHelpers.ToSourceLocation item.Range) declType

            parentType.WithField conv

        node.Args.Slice(1) |> Seq.fold foldDef declType, scope

    /// Converts a parameter attribute.
    let ConvertParameterAttribute (parent : INodeConverter) (scope : LocalScope) (parameter : DescribedParameter, attrs : IAttribute seq) (node : LNode) =
        if node.Name = CodeSymbols.Ref || node.Name = CodeSymbols.Out then
            let descParam = new DescribedParameter(parameter.Name, parameter.ParameterType.MakePointerType(PointerKind.ReferencePointer))
            if node.Name = CodeSymbols.Out then
                descParam, Seq.singleton PrimitiveAttributes.Instance.OutAttribute |> Seq.append attrs
            else
                descParam, attrs
        else
            parameter, parent.ConvertAttribute node scope.Global attrs

    /// Converts a parameter declaration node.
    let ConvertParameterDeclaration (parent : INodeConverter) (scope : LocalScope) (node : LNode) =
        let varType          = parent.ConvertType node.Args.[0] scope
        let name, _          = ReadName parent node.Args.[1] scope.Global
        let descParam        = new DescribedParameter(name, varType)
        let descParam, attrs = node.Attrs |> Seq.fold (ConvertParameterAttribute parent scope) (descParam, Seq.empty)
        attrs |> Seq.iter descParam.AddAttribute
        descParam :> IParameter

    /// Appends a `return(void);` statement to the given statement, provided the given
    /// type is either `null` or `void`.
    let AutoReturn (scope : LocalScope) (retType : IType) (body : IExpression) =
        if retType = null || retType = PrimitiveTypes.Void then
            new BlockStatement([| ExpressionBuilder.ToStatement body; new ReturnStatement() :> IStatement |]) :> IStatement
        else if body.Type <> PrimitiveTypes.Void then
            body |> ExpressionBuilder.Return scope
                 |> ExpressionBuilder.ToStatement
        else
            ExpressionBuilder.ToStatement body

    let private ConvertCommonMethodDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        let isStatic, attrs = ReadAttributes parent node scope []
        let name, tParams   = ReadName parent node.Args.[1] scope
        let fMethod         = new FunctionalMethod(new FunctionalMemberHeader(name, attrs, NodeHelpers.ToSourceLocation node.Args.[1].Range), declType, isStatic) 
        let fMethod         = fMethod.WithGenericParameters tParams

        let getParameters declMethod = 
            let localScope = new LocalScope(AliasGenericParameters declMethod scope)
            node.Args.[2].Args |> Seq.map (fun item -> ConvertParameterDeclaration parent localScope item)

        let fMethod         = fMethod.WithParameters getParameters

        let getBody declMethod =
            let scope      = AliasGenericParameters declMethod scope
            let localScope = new LocalScope(new FunctionScope(scope, declMethod))
            let body = if node.ArgCount > 3 then
                           parent.ConvertExpression node.Args.[3] localScope ||> ExpressionBuilder.Scope 
                       else
                           ExpressionBuilder.Void
            AutoReturn localScope declMethod.ReturnType body

        fMethod.WithBody getBody

    let ConvertMethodDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        let scope   = AliasGenericParameters declType scope
        let fMethod = ConvertCommonMethodDeclaration parent node scope declType
        let retType declMethod = parent.ConvertType node.Args.[0] (new LocalScope(AliasGenericParameters declMethod scope))
        let fMethod = fMethod.WithReturnType retType
        let fMethod = fMethod.WithBaseMethods (InferBaseMethods (scope.GetAllMembers >> OfType))
        fMethod :> IMethod

    let ConvertConstructorDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declType : IType) =
        let scope   = AliasGenericParameters declType scope
        let fMethod = ConvertCommonMethodDeclaration parent node scope declType
        let fMethod = fMethod.AsConstructor 
        fMethod :> IMethod

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
           
    /// Inherits accessor attributes from the declaring property, and merges
    /// them with the accessor's own attributes.
    let InheritAccessorAttributes =
        // We intend to inherit:
        //  * access modifier/visibility
        //  * virtualness, abstractness

        InheritAttributes [AccessAttribute.AccessAttributeType; 
                           PrimitiveAttributes.Instance.AbstractAttribute.AttributeType; 
                           PrimitiveAttributes.Instance.VirtualAttribute.AttributeType]
                            

    /// Convert an accessor declaration's signature, but not its body.
    let ConvertAccessorSignature (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (accType : AccessorType) (declProp : IProperty) =
        let isStatic, attrs = ReadAttributes parent node scope []
        if isStatic then
            let staticNode = node.Attrs |> Seq.find (fun x -> x.Name = CodeSymbols.Static)
            scope.Log.LogError(new LogEntry("Invalid 'static' attribute", 
                                            "'static' attributes cannot be applied to accessors directly. " + 
                                            "Apply them to the enclosing property instead.", 
                                            NodeHelpers.ToSourceLocation staticNode.Range))
        let attrs = InheritAccessorAttributes declProp attrs
        let acc = new FunctionalAccessor(new FunctionalMemberHeader(accType.ToString().ToLower() + "_" + declProp.Name, attrs, NodeHelpers.ToSourceLocation node.Range), declProp, accType)
        let retType, parameters = GetAccessorSignature accType (lazy declProp.PropertyType) (lazy (declProp.GetIndexerParameters() |> Seq.ofArray))
        let acc = acc.WithReturnType retType
        let acc = evalLazy parameters |> Seq.fold (fun (x : FunctionalAccessor) y -> x.WithParameter (lazy y)) acc
        let acc = acc.WithBaseMethods ((InferBaseAccessors (scope.GetAllMembers >> OfType<ITypeMember, IProperty>)) >> Seq.cast)
        acc

    /// Converts an accessor declaration.
    let ConvertAccessorDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (accType : AccessorType) (declProp : IProperty) : IAccessor =
        let acc = ConvertAccessorSignature parent node scope accType declProp

        let getBody declMethod =
            if node.ArgCount > 0 then
                let localScope      = new LocalScope(new FunctionScope(scope, declMethod))
                let body            = parent.ConvertExpression node.Args.[0] localScope ||> ExpressionBuilder.Scope 
                AutoReturn localScope declMethod.ReturnType body
            else if IsAbstractOrInterface declMethod then
                null // This accessor does not have a body, and that's okay.
            else
                ExpressionBuilder.Unknown declMethod.ReturnType |> ExpressionBuilder.ReturnUnchecked
                                                                |> ExpressionBuilder.Error (new LogEntry("Bodyless property accessor",
                                                                                                         "The '" + accType.ToString().ToLower() + "' accessor of property '" + declProp.Name + 
                                                                                                         "' does not have a body, but is neither abstract nor is it declared in an interface."))
                                                                |> ExpressionBuilder.Source (NodeHelpers.GetTargetRange node |> NodeHelpers.ToSourceLocation)
                                                                |> ExpressionBuilder.ToStatement

        acc.WithBody getBody :> IAccessor

    /// Generates an autoprop field name for the given property name.
    let private AutoPropertyFieldName propName =
        propName + "$value"

    /// Converts an autoprop accessor declaration.
    let ConvertAutoAccessorDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (accType : AccessorType) (declProp : IProperty) : IAccessor =
        let acc = ConvertAccessorSignature parent node scope accType declProp

        let getBody (declMethod : IMethod) =
            // An autoprop is syntactic sugar for `public T Property { get { return Property$value; } set { Property$value = value; return; } }`
            let scope      = new LocalScope(new FunctionScope(scope, declMethod))
            let thisExpr   = if declMethod.IsStatic then 
                                 let declType = declMethod.DeclaringType
                                 let finalType = if declType.get_IsGeneric() && declType.get_IsGenericDeclaration() then
                                                     declType.MakeGenericType(declType.GetGenericParameters() |> Seq.cast)
                                                 else
                                                     declType
                                 Global finalType
                             else 
                                 ExpressionBuilder.This scope |> ExpressionBuilder.GetAccessedExpression
            let valueField = thisExpr.Type.GetField(AutoPropertyFieldName declProp.Name, declProp.IsStatic)
            let fieldAcc   = ExpressionBuilder.AccessField scope valueField thisExpr
            if accType = AccessorType.GetAccessor then
                fieldAcc |> ExpressionBuilder.ReturnUnchecked
                         |> ExpressionBuilder.ToStatement
            else
                [ExpressionBuilder.Assign scope fieldAcc ((scope.GetVariable "value").Value.CreateGetExpression()); ExpressionBuilder.ReturnVoid] 
                    |> ExpressionBuilder.VoidBlock 
                    |> ExpressionBuilder.ToStatement

        acc.WithBody getBody :> IAccessor

    /// Converts a property "member", i.e. an accessor or a block that contains accessors.
    let rec ConvertPropertyMember (convAcc : INodeConverter -> LNode -> GlobalScope -> AccessorType -> IProperty -> IAccessor) (parent : INodeConverter) (scope : GlobalScope) (prop : FunctionalProperty) (node : LNode) : FunctionalProperty =
        if node.Name = CodeSymbols.Braces then
            node.Args |> Seq.fold (ConvertPropertyMember convAcc parent scope) prop
        else
            match GetAccessorType node.Name with
            | None         ->
                scope.Log.LogError(new LogEntry("Unrecognized accessor type", 
                                                "'" + node.Name.Name + "' was not recognized as a type of accessor. ", 
                                                NodeHelpers.GetTargetRange node |> NodeHelpers.ToSourceLocation))
                prop
            | Some accType ->
                let createAcc = convAcc parent node scope accType
                prop.WithAccessor createAcc

    /// Tests if the given node is an autoprop body node.
    let rec IsAutoPropertyBody (parent : INodeConverter) (scope : GlobalScope) (node : LNode) =
        if node.Name = CodeSymbols.Braces then
            node.Args |> Seq.exists (IsAutoPropertyBody parent scope >> not)
                      |> not
        else if node.IsId && (node.Name = CodeSymbols.get || node.Name = CodeSymbols.set) then
            let _, attrs = ReadAttributes parent node scope []
            attrs.HasAttribute(PrimitiveAttributes.Instance.AbstractAttribute.AttributeType) |> not
        else
            false

    /// Tests if the given property node qualifies as an autoprop.
    let IsAutoProperty (parent : INodeConverter) (scope : GlobalScope) (node : LNode) (declType : IType) (isStatic : bool, attrs : IAttribute seq) =
        IsAutoPropertyBody parent scope node.Args.[2] && not (declType.get_IsInterface()) 
                                                      && attrs.HasAttribute(PrimitiveAttributes.Instance.AbstractAttribute.AttributeType) 
                                                         |> not

    /// Converts a property declaration's signature, but not its contents.
    let ConvertPropertySignature (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (name : string) (isStatic : bool, attrs : IAttribute seq) (declType : IType) =
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
        prop

    /// Converts a single property declaration that is not an autoprop.
    let ConvertPropertyDeclaration (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (name : string) (isStatic : bool, attrs : IAttribute seq) (convAcc : INodeConverter -> LNode -> GlobalScope -> AccessorType -> IProperty -> IAccessor) (declType : IType) =
        let scope = AliasGenericParameters declType scope
        let prop  = ConvertPropertySignature parent node scope name (isStatic, attrs) declType
        let prop  = ConvertPropertyMember convAcc parent scope prop node.Args.[2]
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
                let attrs   = ReadAttributes parent node scope []
                let declType, convAcc = if IsAutoProperty parent scope node declType attrs then
                                            let createAutoPropField declType =
                                                // synthesize a `private hidden (static|instance) T <name>$value;` field.
                                                let fieldAttrs = [new AccessAttribute(AccessModifier.Private) :> IAttribute; 
                                                                  PrimitiveAttributes.Instance.HiddenAttribute]
                                                let header = new FunctionalMemberHeader(AutoPropertyFieldName name, fieldAttrs, NodeHelpers.ToSourceLocation node.Range)
                                                new FunctionalField(header, declType, fst attrs, lazy declType.GetProperties().GetProperty(name, fst attrs).PropertyType) :> IField
                                            declType.WithField createAutoPropField, ConvertAutoAccessorDeclaration
                                        else
                                            declType, ConvertAccessorDeclaration
                ConvertPropertyDeclaration parent node scope name attrs convAcc |> declType.WithProperty, scope
                
        let recognizeProperty (node : LNode) = node.ArgCount = 3
        new TypeMemberConverter(recognizeProperty, convertProp)

    /// Converts a type declaration.
    let ConvertTypeDeclaration (kindAttributes : IAttribute seq) (parent : INodeConverter) (node : LNode) (scope : GlobalScope) (declNs : INamespace) =
        let isStatic, attrs = ReadAttributes parent node scope kindAttributes
        let newAttrs = if isStatic then 
                           Seq.singleton PrimitiveAttributes.Instance.StaticTypeAttribute |> Seq.append attrs 
                       else 
                           attrs
        let name, tParams = ReadName parent node.Args.[0] scope

        // Gets the type's base types, including the root type if there is no clear parent type and the given type is no interface.
        let getBaseTypes (declType : IType) =
            let scope  = AliasGenericParameters declType scope
            let bTypes = node.Args.[1].Args |> Seq.map (fun x -> parent.ConvertType x (new LocalScope(scope)))
            let rootType = scope.Environment.RootType
            if rootType = null || declType.get_IsInterface() || bTypes |> Seq.exists (fun x -> not (x.get_IsInterface())) then
                bTypes
            else
                Seq.append (Seq.singleton rootType) bTypes

        let fType = new FunctionalType(new FunctionalMemberHeader(name, newAttrs, NodeHelpers.ToSourceLocation node.Args.[0].Range), declNs)
        let fType = fType.WithBaseTypes getBaseTypes
        let fType = fType.WithGenericParameters tParams
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
        let _, attrs = ReadAttributes parent node scope []
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

    /// A converter that converts pointer suffixes.
    let PointerTypeConverter pointerSymbol pointerKind =
        let convTy (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            match parent.ConvertType node.Args.[1] scope with
            | null -> null
            | ty   -> ty.MakePointerType(pointerKind) :> IType

        let matches (node : LNode) =
            node.ArgCount = 2 && node.Args.[0].Name = pointerSymbol

        CreateConverter matches convTy

    /// A converter that converts generic type instantiations.
    let GenericInstanceTypeConverter =
        let convTy (parent : INodeConverter) (node : LNode) (scope : LocalScope) =
            let rank    = node.ArgCount - 1
            let genName = NodeHelpers.ToTypeName node.Args.[0]
            let genName = new TypeName(Array.append genName.Start.Path [| genName.Name + "<" + (new System.String(',', rank - 1)) + ">" |])
            let genDecl = scope.Global.Binder.Bind genName
            if genDecl = null then
                null
            else
                Seq.skip 1 node.Args |> Seq.map (fun x -> parent.ConvertType x scope)
                                     |> genDecl.MakeGenericType

        let matches (node : LNode) =
            node.ArgCount > 1

        CreateConverter matches convTy