module CheckedArithmeticCore

export safearg_type, safearg, safeconvert, accumulatortype, acc

"""
    newT = CheckedArithmeticCore.safearg_type(::Type{T})

Return a "reasonably safe" type `newT` for computation with numbers of type `T`.
For example, for `UInt8` one might return `UInt128`, because one is much less likely
to overflow with `UInt128`.
"""
function safearg_type end


"""
    xsafe = CheckedArithmeticCore.safearg(x)

Return a variant `xsafe` of `x` that is "reasonably safe" for non-overflowing computation.
For numbers, this uses [`CheckedArithmetic.safearg_type`](@ref).
For containers and other non-number types, specialize `safearg` directly.
"""
safearg(x::T) where T = convert(safearg_type(T), x)


"""
    xc = safeconvert(T, x)

Convert `x` to type `T`, "safely." This is designed for comparison to results computed by
[`CheckedArithmetic.@check`](@ref), i.e., for arguments converted by
[`CheckedArithmeticCore.safearg`](@ref).
"""
safeconvert(::Type{T}, x) where T = convert(T, x)


"""
    Tnew = accumulatortype(op, T1, T2, ...)
    Tnew = accumulatortype(T1, T2, ...)

Return a type `Tnew` suitable for accumulation (reduction) of elements of type `T` under
operation `op`.

# Examples

```jldoctest; setup = :(using CheckedArithmetic)
julia> accumulatortype(+, UInt8)
$UInt
```
"""
Base.@pure accumulatortype(op::Function, T1::Type, T2::Type, T3::Type...) =
    accumulatortype(op, promote_type(T1, T2, T3...))
Base.@pure accumulatortype(T1::Type, T2::Type, T3::Type...) =
    accumulatortype(*, T1, T2, T3...)
accumulatortype(::Type{T}) where T = accumulatortype(*, T)


"""
    xacc = acc(x)

Convert `x` to type [`accumulatortype`](@ref)`(typeof(x))`.
"""
acc(x) = convert(accumulatortype(typeof(x)), x)
acc(f::F, x) where F<:Function = convert(accumulatortype(f, typeof(x)), x)


end # module
