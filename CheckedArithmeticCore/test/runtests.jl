using CheckedArithmeticCore
using Test

struct MyType end
struct MySafeType end
Base.convert(::Type{MySafeType}, ::MyType) = MySafeType()
CheckedArithmeticCore.safearg_type(::Type{MyType}) = MySafeType
CheckedArithmeticCore.accumulatortype(::typeof(+), ::Type{MyType}) = MyType
CheckedArithmeticCore.accumulatortype(::typeof(+), ::Type{MySafeType}) = MySafeType
CheckedArithmeticCore.accumulatortype(::typeof(*), ::Type{MyType}) = MySafeType
CheckedArithmeticCore.accumulatortype(::typeof(*), ::Type{MySafeType}) = MySafeType
Base.promote_rule(::Type{MyType}, ::Type{MySafeType}) = MySafeType

# fallback
@test safearg(MyType()) === MySafeType()

# fallback
@test safeconvert(UInt16, 0x12) === 0x0012

# fallback
@test accumulatortype(MyType) === MySafeType
@test accumulatortype(MyType, MyType) === MySafeType
@test accumulatortype(+, MyType) === MyType
@test accumulatortype(*, MyType) === MySafeType
@test accumulatortype(+, MyType, MySafeType) === MySafeType
@test accumulatortype(*, MySafeType, MyType) === MySafeType

# acc
@test acc(MyType()) === MySafeType()
@test acc(+, MyType()) === MyType()
