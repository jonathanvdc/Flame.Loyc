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
        let converter = value |> Constant |> Constant |> Constant
        new AttributeConverter(predicate, converter)

    let AccessAttributeConverter modifier =
        ConstantAttributeConverter (new AccessAttribute(modifier))