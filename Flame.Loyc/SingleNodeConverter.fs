namespace Flame.Loyc

open Loyc
open Loyc.Syntax
open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Functional

type ScopedNodeConverter<'Result, 'Scope>(predicate : LNode -> bool, convert : INodeConverter -> LNode -> 'Scope -> 'Result) =
    
    /// Finds out if this node converter matches the given node.
    /// Note: this need not check the node's type, as this is done
    /// elsewhere.
    member this.Matches = predicate

    /// Converts the given node in the given scope,
    /// with the given parent node converter.
    member this.Convert = convert

type SingleNodeConverter<'a>  = ScopedNodeConverter<'a, LocalScope>

type CallConverter            = SingleNodeConverter<IExpression * LocalScope>
type TypeConverter            = SingleNodeConverter<IType>

type NamespaceMemberConverter = ScopedNodeConverter<IFunctionalNamespace, IFunctionalNamespace * GlobalScope>
type TypeMemberConverter      = ScopedNodeConverter<FunctionalType, FunctionalType * GlobalScope>

type AttributeConverter       = ScopedNodeConverter<IAttribute, GlobalScope>

type LiteralConverter         = obj -> IExpression