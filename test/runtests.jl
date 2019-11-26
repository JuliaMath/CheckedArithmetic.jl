using CheckedArithmetic
using Test

@test isempty(detect_ambiguities(CheckedArithmetic, Base, Core))

@checked begin
    plus(x, y) = x + y
    minus(x, y) = x - y
end

function sumsquares(A::AbstractArray)
    s = zero(accumulatortype(eltype(A)))
    for a in A
        s += acc(a)^2
    end
    return s
end

@testset "CheckedArithmetic.jl" begin
    @testset "@checked" begin
        @test @checked(abs(Int8(-2))) === Int8(2)
        @test_throws OverflowError @checked(abs(typemin(Int8)))
        @test @checked(+2) === 2
        @test @checked(+UInt(2)) === UInt(2)
        @test @checked(-2) === -2
        @test_throws OverflowError @checked(-UInt(2))
        @test @checked(0x10 + 0x20) === 0x30
        @test_throws OverflowError @checked(0xf0 + 0x20)
        @test @checked(0x30 - 0x20) === 0x10
        @test_throws OverflowError @checked(0x10 - 0x20)
        @test @checked(-7) === -7
        @test_throws OverflowError @checked(-UInt(7))
        @test @checked(0x10*0x02) === 0x20
        @test_throws OverflowError @checked(0x10*0x10)
        @test @checked(7 ÷ 2) === 3
        @test_throws DivideError @checked(typemin(Int8)÷Int8(-1))
        @test @checked(rem(typemin(Int8), Int8(-1))) === Int8(0)
        @test_throws DivideError @checked(rem(typemax(Int8), Int8(0)))
        @test @checked(fld(typemax(Int8), Int8(-1))) === -typemax(Int8)
        @test_throws DivideError @checked(fld(typemin(Int8), Int8(-1)))
        @test @checked(mod(typemax(Int8), Int8(1))) === Int8(0)
        @test_throws DivideError @checked(mod(typemin(Int8), Int8(0)))
        @test @checked(cld(typemax(Int8), Int8(-1))) === -typemax(Int8)
        @test_throws DivideError @checked(cld(typemin(Int8), Int8(-1)))

        @test plus(0x10, 0x20) === 0x30
        @test_throws OverflowError plus(0xf0, 0x20)
        @test minus(0x30, 0x20) === 0x10
        @test_throws OverflowError minus(0x20, 0x30)
    end

    @testset "check" begin
        @test @check(3+5) == 8
        @test_throws InexactError @check(0xf0+0x15)
        @test @check([3]+[5]) == [8]
        @test_throws InexactError @check([0xf0]+[0x15])
        f(t) = t[1] + t[2]
        @test @check(f((1,2))) === 3
        @test_throws InexactError @check(f((0xf0, 0x20)))
        times2(x) = 0x02*x
        times2(d::Dict) = Dict(k=>times2(v) for (k,v) in d)
        @test @check(times2(Dict("a"=>7))) == Dict("a"=>14)
        @test_throws InexactError @check(times2(Dict("a"=>0xf0)))
        for item in Any["hi", :hi, (3,), (silly="hi",), trues(3), [true], 1:3, 1:2:5,
                        LinRange(1, 3, 3), StepRangeLen(1.0, 3.0, 3), 0x01:0x03, Ref(2),
                        pairs((silly="hi",)), Set([1,3]), BitSet(7), nothing, missing,
                        Some(nothing), 'c', MIME("text/plain"), IOBuffer(), r"\d+",
                        Channel(), CartesianIndex(1, 3), Base.UUID(0), `ls`,
                        sum, Base, Val(3), Task(()->1), Base.Order.Forward, Timer(0.1),
                        now()]
            @test @check(identity(item)) === item
        end
    end

    @testset "acc" begin
        @test acc(0x02) === UInt(2)
        @test acc(-, 0x02) === 2
        @test accumulatortype(-, UInt8) === Int
        @test accumulatortype(*, Int16, Float16) === Float64

        @test sumsquares(0x00:0xff) == sumsquares(0:255)
    end
end
