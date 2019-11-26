# CheckedArithmetic

This package aims to make it easier to detect overflow in numeric computations.
It exports two macros, `@check` and `@checked`, as well as functions
`accumulatortype` and `acc`.
Packages can add support for their own types to interact appropriately
with these tools.

## `@checked`

`@checked` converts arithmetic operators to their checked variants.
For example,

```julia
@checked z = x + y
```

rewrites this expression as

```julia
z = Base.Checked.checked_add(x, y)
```

Note that this macro only operates at the level of surface syntax, i.e.,

```julia
@checked z = f(x) + f(y)
```

will not detect overflow caused by `f`.

The [`Base.Checked` module](https://github.com/JuliaLang/julia/blob/master/base/checked.jl) defines numerous checked operations.
These can be specialized for custom types.

## `@check`

`@check` performs an operation in two different ways,
checking that both approaches agree.
`@check` is primarily useful in tests.

For example,

```julia
d = @check ssd(a, b)
```

would perform `ssd(a, b)` with the inputs as given, and also compute `ssd(asafe, bsafe)`
where `asafe` and `bsafe` are "safer" variants of `a` and `b`.
It then tests whether the result obtained from the "safe" arguments is consistent with
the result obtained from `a` and `b`.
If the two differ to within the precision of the "ordinary" (unsafe) result, an
error is thrown.

Packages can specialize `CheckedArithmetic.safearg` to control how `asafe` and `bsafe`
are generated. To guard against oversights, `safearg` must be explicitly defined for
each numeric type---the fallback method for `safearg(::Number)` is to throw an error.

## `accumulatortype` and `acc`

These functions are useful for writing overflow-safe algorithms.
`accumulatortype(T)` or `accumulatortype(T1, T2...)` takes types as input arguments
and returns a type suitable for limited-risk arithmetic operations.
`acc(x)` is just shorthand for `convert(accumulatortype(typeof(x)), x)`.

You can also specialize on the operation. For example,

```julia
julia> accumulatortype(+, UInt8)
UInt64

julia> accumulatortype(-, UInt8)
Int64
```

If you're computing a sum-of-squares and want to make sure you algorithm is (reasonably)
safe for an input array of `UInt8` numbers, you might want to write that as

```julia
function sumsquares(A::AbstractArray)
    s = zero(accumulatortype(eltype(A)))
    for a in A
        s += acc(a)^2
    end
    return s
end
```
