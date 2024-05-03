module CheckedArithmetic

using OverflowContexts
using CheckedArithmeticCore
import CheckedArithmeticCore: safearg_type, safearg, safeconvert, accumulatortype, acc

using Base.Meta: isexpr
using Base.Checked: checked_neg, checked_add, checked_sub, checked_mul, checked_abs
if VERSION ≥ v"1.11-alpha"
    using Base.Checked: checked_pow
end

using LinearAlgebra: Factorization, UniformScaling
using Random: AbstractRNG
using Dates

export @check
export accumulatortype, acc # re-export
export @default_checked, @default_unchecked, @checked, @unchecked,
    unchecked_neg, unchecked_add, unchecked_sub, unchecked_mul, unchecked_negsub, unchecked_pow, unchecked_abs,
    checked_neg, checked_add, checked_sub, checked_mul, checked_pow, checked_negsub, checked_abs # re-export

macro check(expr, kws...)
    isexpr(expr, :call) || error("expected :call expression, got ",
                                 isa(expr, Expr) ? QuoteNode(expr.head) : typeof(expr))
    safeexpr = copy(expr)
    for i = 2:length(expr.args)
        safeexpr.args[i] = Expr(:call, :(CheckedArithmetic.safearg), expr.args[i])
    end
    cmpexpr = isempty(kws) ? :(val == valcmp || error(val, " is not equal to ", valcmp)) :
                             :(isapprox(val, valcmp; $(esc(kws...))) || error(val, " is not approximately equal to ", valcmp))
    return quote
        local val = $(esc(expr))
        local valcmp = CheckedArithmetic.safeconvert(typeof(val), $(esc(safeexpr)))
        if ismissing(val) && ismissing(valcmp)
            val
        else
            $cmpexpr
            val
        end
    end
end


# safearg_type
# ------------
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

safearg_type(::Type{<:Rational}) = Float64


# safearg
#--------
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
safearg(d::Iterators.Pairs) = Iterators.Pairs(safearg(d.data), d.itr)
safearg(d::IdDict) = IdDict(safearg(p) for p in d)
safearg(d::WeakKeyDict) = WeakKeyDict(k=>safearg(v) for (k, v) in d)  # do not convert keys
## AbstractSets
safearg(s::Set) = Set(safearg(x) for x in s)
safearg(s::Base.IdSet) = Base.IdSet(safearg(x) for x in s)
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


# safeconvert
# -----------
safeconvert(::Type{T}, x) where T<:Integer = round(T, x)
safeconvert(::Type{T}, x) where T<:AbstractFloat = T(x)
safeconvert(::Type{AA}, A::AbstractArray) where AA<:AbstractArray{T} where T<:Integer = round.(T, A)


# accumulatortype
# ---------------
const SignPreserving = Union{typeof(+), typeof(*)}
const ArithmeticOp = Union{SignPreserving, typeof(-)}

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

end # module
