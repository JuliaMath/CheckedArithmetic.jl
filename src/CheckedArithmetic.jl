module CheckedArithmetic

using Base.Meta: isexpr
using Test
using LinearAlgebra: Factorization, UniformScaling
using Random: AbstractRNG
using Dates

export @checked, @check, accumulatortype, acc

# + and - are not listed because of the unary/binary issues
const opcheck = Dict(:abs => :(Base.Checked.checked_abs),
                     :+ => :(Base.Checked.checked_add),
                     :- => :(Base.Checked.checked_sub),
                     :* => :(Base.Checked.checked_mul),
                     :÷ => :(Base.Checked.checked_div),
                     :rem => :(Base.Checked.checked_rem),
                     :fld => :(Base.Checked.checked_fld),
                     :mod => :(Base.Checked.checked_mod),
                     :cld => :(Base.Checked.checked_cld),
                     )

function replace_checked!(expr::Expr)
    if expr.head == :call
        f, len = expr.args[1], length(expr.args)
        op = isexpr(f, :.) ? f.args[2].value : f # handle module-scoped functions
        if op === :+ && len == 2                 # unary +
            # no action required
        elseif op === :- && len == 2             # unary -
            op = :(Base.Checked.checked_neg)
            if isexpr(f, :.)
                f.args[2].value = op
                expr.args[1] = f
            else
                expr.args[1] = op
            end
        else                                     # arbitrary call
            op = get(opcheck, op, op)
            if isexpr(f, :.)
                f.args[2].value = op
                expr.args[1] = f
            else
                expr.args[1] = op
            end
        end
        for a in Iterators.drop(expr.args, 1)
            if isa(a, Expr)
                replace_checked!(a)
            end
        end
    else
        for a in expr.args
            if isa(a, Expr)
                replace_checked!(a)
            end
        end
    end
    return expr
end

"""
    @checked expr

Perform all the operations in `expr` using checked arithmetic.

# Examples

```jldoctest
julia> 0xff + 0x10    # operation that overflows
0x0f

julia> @checked 0xff + 0x10
ERROR: OverflowError: 255 + 16 overflowed for type UInt8
```

You can also wrap method definitions (or blocks of code) in `@checked`:

```jldoctest
julia> plus(x, y) = x + y; minus(x, y) = x - y
minus (generic function with 1 method)

julia> @show plus(0xff, 0x10) minus(0x10, 0x20);
plus(0xff, 0x10) = 0x0f
minus(0x10, 0x20) = 0xf0

julia> @checked (plus(x, y) = x + y; minus(x, y) = x - y)
minus (generic function with 1 method)

julia> plus(0xff, 0x10)
ERROR: OverflowError: 255 + 16 overflowed for type UInt8

julia> minus(0x10, 0x20)
ERROR: OverflowError: 16 - 32 overflowed for type UInt8
```
"""
macro checked(expr)
    isa(expr, Expr) || return expr
    expr = copy(expr)
    return esc(replace_checked!(expr))
end

macro check(expr)
    isexpr(expr, :call) || error("expected :call expression, got ",
                                 isa(expr, Expr) ? QuoteNode(expr.head) : typeof(expr))
    safeexpr = copy(expr)
    for i = 2:length(expr.args)
        safeexpr.args[i] = Expr(:call, :(CheckedArithmetic.safearg), expr.args[i])
    end
    return esc(quote
        val = $expr
        valcmp = CheckedArithmetic.safeconvert(typeof(val), $safeexpr)
        @test val == valcmp
        return val
    end)
end

"""
    newT = CheckedArithmetic.safearg_type(::Type{T})

Return a "reasonably safe" type `newT` for computation with numbers of type `T`.
For example, for `UInt8` one might return `UInt128`, because one is much less likely
to overflow with `UInt128`.
"""
function safearg_type end

safearg_type(::Type{BigInt})  = BigInt
safearg_type(::Type{Int128})  = Int128
safearg_type(::Type{Int64})   = Int128
safearg_type(::Type{Int32})   = Int128
safearg_type(::Type{Int16})   = Int128
safearg_type(::Type{Int8})    = Int128
safearg_type(::Type{UInt128}) = UInt128
safearg_type(::Type{UInt64})  = UInt128
safearg_type(::Type{UInt32})  = UInt128
safearg_type(::Type{UInt16})  = UInt128
safearg_type(::Type{UInt8})   = UInt128

safearg_type(::Type{Bool}) = Bool   # these have a special meaning that must be preserved

safearg_type(::Type{BigFloat}) = BigFloat
safearg_type(::Type{Float64})  = Float64
safearg_type(::Type{Float32})  = Float64
safearg_type(::Type{Float16})  = Float64
safearg_type(::Type{T}) where T<:Base.TwicePrecision = T

"""
    xsafe = CheckedArithmetic.safearg(x)

Return a variant `xsafe` of `x` that is "reasonably safe" for non-overflowing computation.
For numbers, this uses [`CheckedArithmetic.safearg_type`](@ref).
For containers and other non-number types, specialize `safearg` directly.
"""
safearg(x::Number) = convert(safearg_type(typeof(x)), x)

# Containers
safearg(t::Tuple) = map(safearg, t)
safearg(t::NamedTuple) = map(safearg, t)
safearg(p::Pair) = safearg(p.first) => safearg(p.second)
## AbstractArrays and similar
safearg(A::AbstractArray{T}) where T<:Number = convert(AbstractArray{safearg_type(T)}, A)
safearg(A::BitArray) = A
safearg(A::AbstractArray{Bool}) = A
safearg(rng::LinRange) = LinRange(safearg(rng.start), safearg(rng.stop), rng.len)
safearg(rng::StepRangeLen) = StepRangeLen(safearg(rng.ref), safearg(rng.step), rng.len, rng.offset)
safearg(rng::StepRange) = StepRange(safearg(rng.start), safearg(rng.step), safearg(rng.stop))
safearg(rng::AbstractUnitRange{Int}) = rng  # AbstractUnitRange{Int} is usually used for indexing, preserve this
safearg(rng::AbstractUnitRange{T}) where T<:Integer = convert(AbstractUnitRange{safearg_type(T)}, rng)
safearg(I::UniformScaling) = UniformScaling(safearg(I.λ))
safearg(F::Factorization{Float64}) = F
safearg(ref::Ref) = Ref(safearg(ref[]))
## AbstractDicts
safearg(d::Dict) = Dict(safearg(p) for p in d)
safearg(d::Base.EnvDict) = d
safearg(d::Base.ImmutableDict) = Base.ImmutableDict(safearg(p) for p in d)
safearg(d::Iterators.Pairs) = Iterators.Pairs(safearg(p) for p in d)
safearg(d::IdDict) = IdDict(safearg(p) for p in d)
safearg(d::WeakKeyDict) = WeakKeyDict(k=>safearg(v) for (k, v) in d)  # do not convert keys
## AbstractSets
safearg(s::Set) = Set(f(x) for x in s)
safearg(s::Base.IdSet) = Base.IdSet(f(x) for x in s)
safearg(s::BitSet) = s

# Other common types
safearg(::Nothing) = nothing
safearg(m::Missing) = m
safearg(s::Some) = Some(safearg(s.value))
safearg(rng::AbstractRNG) = rng
safearg(mode::RoundingMode) = mode
safearg(str::AbstractString) = str
safearg(c::AbstractChar) = c
safearg(sym::Symbol) = sym
safearg(mime::MIME) = mime
safearg(rex::Regex) = rex
safearg(rex::RegexMatch) = rex
safearg(chan::AbstractChannel) = chan
safearg(i::Base.AbstractCartesianIndex) = i
safearg(i::Base.UUID) = i
safearg(cmd::Base.AbstractCmd) = cmd
if isdefined(Base, :AbstractLock)
    safearg(lock::Base.AbstractLock) = lock
end
safearg(f::Function) = f
safearg(io::IO) = io
safearg(m::Module) = m
safearg(v::Val) = v
safearg(t::Task) = t
safearg(ord::Base.Order.Ordering) = ord
safearg(t::Timer) = t
safearg(d::Dates.AbstractDateTime) = d
safearg(t::Dates.AbstractTime) = t
safearg(t::Dates.AbstractDateToken) = t


"""
    xc = safeconvert(T, x)

Convert `x` to type `T`, "safely." This is designed for comparison to results computed by
[`@check`](@ref), i.e., for arguments converted by [`CheckedArithmetic.safearg`](@ref).
"""
safeconvert(::Type{T}, x) where T = convert(T, x)

safeconvert(::Type{T}, x) where T<:Integer = round(T, x)
safeconvert(::Type{T}, x) where T<:AbstractFloat = T(x)
safeconvert(::Type{AA}, A::AbstractArray) where AA<:AbstractArray{T} where T<:Integer = round.(T, A)


"""
    Tnew = accumulatortype(op, T1, T2, ...)
    Tnew = accumulatortype(T1, T2, ...)

Return a type `Tnew` suitable for accumulation (reduction) of elements of type `T` under
operation `op`.

# Examples

```jldoctest
julia> accumulatortype(+, UInt8)
$UInt

julia> accumulatortype
"""
Base.@pure accumulatortype(op::Function, T1::Type, T2::Type, T3::Type...) =
    accumulatortype(op, promote_type(T1, T2, T3...))
Base.@pure accumulatortype(T1::Type, T2::Type, T3::Type...) =
    accumulatortype(*, T1, T2, T3...)
accumulatortype(::Type{T}) where T = accumulatortype(*, T)

const SignPreserving = Union{typeof(+), typeof(*)}
const ArithmeticOp = Union{SignPreserving,typeof(-)}

accumulatortype(::ArithmeticOp, ::Type{BigInt})    = BigInt
accumulatortype(::ArithmeticOp, ::Type{Int128})    = Int128
accumulatortype(::ArithmeticOp, ::Type{Int64})     = Int64
accumulatortype(::ArithmeticOp, ::Type{Int32})     = Int
accumulatortype(::ArithmeticOp, ::Type{Int16})     = Int
accumulatortype(::ArithmeticOp, ::Type{Int8})      = Int
accumulatortype(::ArithmeticOp, ::Type{Bool})      = Int
accumulatortype(::SignPreserving, ::Type{UInt128}) = UInt128
accumulatortype(::SignPreserving, ::Type{UInt64})  = UInt64
accumulatortype(::SignPreserving, ::Type{UInt32})  = UInt
accumulatortype(::SignPreserving, ::Type{UInt16})  = UInt
accumulatortype(::SignPreserving, ::Type{UInt8})   = UInt
accumulatortype(::typeof(-), ::Type{UInt128})      = Int128
accumulatortype(::typeof(-), ::Type{UInt64})       = Int64
accumulatortype(::typeof(-), ::Type{UInt32})       = Int
accumulatortype(::typeof(-), ::Type{UInt16})       = Int
accumulatortype(::typeof(-), ::Type{UInt8})        = Int

accumulatortype(::ArithmeticOp, ::Type{BigFloat})  = BigFloat
accumulatortype(::ArithmeticOp, ::Type{Float64})   = Float64
accumulatortype(::ArithmeticOp, ::Type{Float32})   = Float64
accumulatortype(::ArithmeticOp, ::Type{Float16})   = Float64

"""
    xacc = acc(x)

Convert `x` to type [`accumulatortype`](@ref)`(typeof(x))`.
"""
acc(x) = convert(accumulatortype(typeof(x)), x)
acc(f::F, x) where F<:Function = convert(accumulatortype(f, typeof(x)), x)

end # module
