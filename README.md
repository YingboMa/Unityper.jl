# Unityper

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://YingboMa.github.io/Unityper.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://YingboMa.github.io/Unityper.jl/dev)
[![Build Status](https://github.com/YingboMa/Unityper.jl/workflows/CI/badge.svg)](https://github.com/YingboMa/Unityper.jl/actions)
[![Coverage](https://codecov.io/gh/YingboMa/Unityper.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/YingboMa/Unityper.jl)

Unityper's main capability is to "compactify" structures in **static single
inheritance**. For instance
```julia
abstract type AA end
Base.@kwdef struct A2 <: AA
    common_field::Int = 0
    a::Bool = true
    b::Int = 10
end
Base.@kwdef struct B2 <: AA
    common_field::Int = 0
    a::Int = 1
    b::Float64 = 1.0
    d::Complex = 1 + 1.0im # not isbits
end
Base.@kwdef struct C2 <: AA
    common_field::Int = 0
    b::Float64 = 2.0
    d::Bool = false
    e::Float64 = 3.0
    k::Complex{Real} = 1 + 2im # not isbits
end
Base.@kwdef struct D2 <: AA
    common_field::Int = 0
    b::Any = "hi" # not isbits
end
```
can be compactified by
```julia
@compactify begin
    @abstract struct AT
        common_field::Int = 0
    end
    struct A <: AT
        a::Bool = true
        b::Int = 10
    end
    struct B <: AT
        a::Int = 1
        b::Float64 = 1.0
        d::Complex = 1 + 1.0im # not isbits
    end
    struct C <: AT
        b::Float64 = 2.0
        d::Bool = false
        e::Float64 = 3.0
        k::Complex{Real} = 1 + 2im # not isbits
    end
    struct D <: AT
        b::Any = "hi" # not isbits
    end
end
```
Note that the concrete types `A`, `B`, `C`, and `D` here are only conceptual,
and Unityper compactifies these types into a single `AT` type. Hence, to check
concrete types, ones needs to use Unityper's `@compactified` macro
```julia
foo!(xs) = for i in eachindex(xs)
    @inbounds x = xs[i]
    @inbounds xs[i] = @compactified x::AT begin
        A => D()
        B => A()
        C => B()
        D => A()
    end
end
```
The above code is equivalent with
```julia
goo!(xs) = for i in eachindex(xs)
    @inbounds x = xs[i]
    @inbounds xs[i] = x isa A2 ? D2() :
                      x isa B2 ? A2() :
                      x isa C2 ? B2() :
                      x isa D2 ? A2() : error()
end
```
Now, let's benchmark these implementations
```julia
using Random
rng = Random.MersenneTwister(123)
gs = map(x->rand(rng, (A2(), B2(), C2(), D2())), 1:10000);
rng = Random.MersenneTwister(123)
xs = map(x->rand(rng, (A(), B(), C(), D())), 1:10000);
using BenchmarkTools
@btime foo!($xs);
@btime goo!($gs);
```
On my laptop, the benchmark result is
```julia
julia> @btime foo!($xs);
  58.619 μs (0 allocations: 0 bytes)

julia> @btime goo!($gs);
  116.980 μs (10000 allocations: 312.50 KiB)
```
Keep in mind that the `goo!` function is optimal in the sense that it explicitly
checks all the sub-types of `AA`. We can see that Unityper gives a 2x speed up
even in the case where the ordinary Julia code is close to optimal.
