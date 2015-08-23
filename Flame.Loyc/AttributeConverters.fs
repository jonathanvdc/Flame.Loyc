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
    let ConstantAttributeConverter value =
        let predicate (node : LNode) = node.ArgCount = 0
        let converter = fun _ _ (attrs, _) -> Seq.singleton value |> Seq.append attrs
        new AttributeConverter(predicate, converter)

    let AccessAttributeConverter modifier =
        ConstantAttributeConverter (new AccessAttribute(modifier))

    /// An attribute converter that does not modify the given sequence of attributes.
    let EmptyAttributeConverter =
        new AttributeConverter(Constant true, fun _ _ (attrs, _) -> attrs)