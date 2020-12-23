using CheckedArithmeticCore
using BenchmarkTools
using Base.Checked # TODO: re-export

BenchmarkTools.DEFAULT_PARAMETERS.seconds = 1

xs = Dict{Type, Matrix}()
ys = Dict{Type, Matrix}()
zs = Dict{Type, Matrix}()

eltypes = (Int8, UInt8, Int16, UInt16, Int32, UInt32)
for T in eltypes
    push!(xs, T => rand(T, 1000, 1000))
    push!(ys, T => rand(T, 1000, 1000))
    push!(zs, T => zeros(T, 1000, 1000))
end

SUITE = BenchmarkGroup()
SUITE["add"] = BenchmarkGroup([],
    "wrapping" => BenchmarkGroup(),
    "saturating" => BenchmarkGroup(),
    "checked" => BenchmarkGroup(),
)

for T in eltypes
    x = xs[T]::Matrix{T}
    y = ys[T]::Matrix{T}
    z = zs[T]::Matrix{T}
    t = string(T)
    SUITE["add"]["checked"   ][t] = @benchmarkable    checked_add.($x, $z)
end
