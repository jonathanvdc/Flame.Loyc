namespace Flame.Loyc

open Loyc
open Loyc.Syntax
open Flame
open Flame.Build
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Functional
open ExpressionConverters
open LazyHelpers

module AttributeConverters =
    /// A warning for attributes that do not change a program's meaning.
    let MeaninglessAttributeWarning = new WarningDescription("meaningless-attribute", Warnings.Instance.Extra)

    let GetAttributeType (value : IAttribute) = value.AttributeType

    /// Creates an attribute converter that adds a single attribute of a 
    /// unique type to the attribute list, along with the attribute's name.
    let ConstantAttributeConverter (value : IAttribute) attrName =
        let predicate (node : LNode) = node.ArgCount = 0
        let convert _ (node : LNode) (attrs, scope : GlobalScope) = 
            let attrType = value.AttributeType
            if MeaninglessAttributeWarning.UseWarning(scope.Log.Options) && attrs |> Seq.exists (GetAttributeType >> ((=) attrType)) then
                scope.Log.LogWarning(new LogEntry("Meaningless attribute", 
                                                  MeaninglessAttributeWarning.CreateMessage(
                                                      "This attribute is redundant: the attribute's target was already '" + attrName + "'. "),
                                                  NodeHelpers.ToSourceLocation node.Range))
            Seq.singleton value |> Seq.append attrs
        new AttributeConverter(predicate, convert)

    /// Names the given access modifier.
    let NameAccessModifier modifier = 
        match modifier with
        | AccessModifier.Public               -> "public"
        | AccessModifier.Private              -> "private"
        | AccessModifier.Protected            -> "protected"
        | AccessModifier.Assembly             -> "internal"
        | AccessModifier.ProtectedAndAssembly -> "protected and internal"
        | AccessModifier.ProtectedOrAssembly  -> "protected internal"
        | _                                   -> "unknown"

    /// Creates an access attribute converter for the given access modifier.
    let AccessAttributeConverter modifier =
        let predicate (node : LNode) = node.ArgCount = 0
        let convert _ (node : LNode) (attrs, scope : GlobalScope) = 
            let appendAttr target = new AccessAttribute(modifier) :> IAttribute |> Seq.singleton |> Seq.append target

            match attrs |> Seq.tryFind (GetAttributeType >> ((=) AccessAttribute.AccessAttributeType)) with
            | Some (preAttr : IAttribute) -> 
                let acc = (preAttr :?> AccessAttribute).Access
                if acc = modifier then
                    if MeaninglessAttributeWarning.UseWarning(scope.Log.Options) then
                        scope.Log.LogWarning(new LogEntry("Meaningless attribute", 
                                                          MeaninglessAttributeWarning.CreateMessage(
                                                              "This attribute is redundant: the attribute's target was already '" + 
                                                              (NameAccessModifier acc) + "'. "),
                                                          NodeHelpers.ToSourceLocation node.Range))
                    attrs
                else
                    attrs |> Seq.filter (GetAttributeType >> ((<>) AccessAttribute.AccessAttributeType))
                          |> appendAttr
            | None         ->
                appendAttr attrs
        new AttributeConverter(predicate, convert)

    /// An attribute converter that does not modify the given sequence of attributes.
    let EmptyAttributeConverter =
        new AttributeConverter(Constant true, fun _ _ (attrs, _) -> attrs)

    /// Creates an attribute converter that removes all attributes of the given type from the attribute list,
    /// given the target attribute's type, this attribute's name, and the target attribute's name.
    let RemoveAttributeConverter targetType attrName targetName =
        let predicate (node : LNode) = node.ArgCount = 0
        let convert _ (node : LNode) (attrs : IAttribute seq, scope : GlobalScope) =
            let rem, tgt = Seq.toList attrs |> List.partition (fun x -> x.AttributeType <> targetType)
            if Seq.isEmpty tgt && MeaninglessAttributeWarning.UseWarning(scope.Log.Options) then
                scope.Log.LogWarning(new LogEntry("Meaningless attribute", 
                                                  MeaninglessAttributeWarning.CreateMessage(
                                                      "Attribute '" + attrName + 
                                                      "' was meaningless here, because its target was not '" + 
                                                      targetName + "'. "),
                                                  NodeHelpers.ToSourceLocation node.Range))
            rem |> Seq.ofList

        new AttributeConverter(predicate, convert)