namespace Flame.Loyc

open Flame

module UnsafeHelpers =
    let UnsafeAttribute = new FlagAttribute("UnsafeAttribute")

    let rec IsUnsafe (item : IMember) =
        item.HasAttribute(UnsafeAttribute.AttributeType) || 
        match item with
        | :? ITypeMember as tyMember -> IsUnsafe tyMember.DeclaringType
        | :? IType       as ty       -> IsUnsafe ty.DeclaringNamespace
        | _                          -> false

    let MissingUnsafeWarningName = "missing-unsafe"
