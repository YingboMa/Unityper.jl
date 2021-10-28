using Unityper, Test

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

@test length(fieldnames(AT)) == 6
at = AT(1, 1, 1.0, true, 1 + 1im, reinterpret(var"###AT###1", Int32(0)))
# This is actually mapped to Float64! Surprise!
@test !at.a
@test at.b isa Int
@test_throws Any at.c

a = A()
@test a.a
@test a.b === 10
a = A(; a=false, b=42)
@test !a.a
@test a.b === 42
io = IOBuffer()
show(io, MIME"text/plain"(), a)
@test String(take!(io)) == "A(a = false, b = 42)::AT"

b = B()
@test b.a === 1
@test b.b === 1.0
@test b.d === 1 + 1.0im
b = B(a=12, b=2.0, d=2+1im)
@test b.a === 12
@test b.b === 2.0
@test b.d === 2 + 1im

c = C()
@test c.b === 2.0
@test !c.d
@test c.e === 3.0
@test c.k === 1 + 2im

c = C(b=8.0, d=true, e=5.0, k=10+10im)
@test c.b === 8.0
@test c.d
@test c.e === 5.0
@test c.k === 10 + 10im

d = D()
@test d.b == "hi"
d = D(b=100)
@test d.b === 100

abstract type AAT end
@compactify begin
    @abstract struct BT <: AAT
        common_field::Int = 0
    end
    struct A1 <: BT
        a::Bool = true
        b::Int = 10
    end
    struct B1 <: BT
        a::Int = 1
        b::Float64 = 1.0
        d::Complex = 1 + 1.0im # not isbits
    end
    struct C1 <: BT
        b::Float64 = 2.0
        d::Bool = false
        e::Float64 = 3.0
        k::Complex{Real} = 1 + 2im # not isbits
    end
    struct D1 <: BT
        b::Any = "hi" # not isbits
    end
end
@test BT <: AAT

@test length(fieldnames(BT)) == 6
at = BT(1, 1, 1.0, true, 1 + 1im, reinterpret(var"###BT###1", Int32(0)))
# This is actually mapped to Float64! Surprise!
@test !at.a
@test at.b isa Int
@test_throws Any at.c

a = A1()
@test a.a
@test a.b === 10
a = A1(; a=false, b=42)
@test !a.a
@test a.b === 42

b = B1()
@test b.a === 1
@test b.b === 1.0
@test b.d === 1 + 1.0im
b = B1(a=12, b=2.0, d=2+1im)
@test b.a === 12
@test b.b === 2.0
@test b.d === 2 + 1im

c = C1()
@test c.b === 2.0
@test !c.d
@test c.e === 3.0
@test c.k === 1 + 2im

c = C1(b=8.0, d=true, e=5.0, k=10+10im)
@test c.b === 8.0
@test c.d
@test c.e === 5.0
@test c.k === 10 + 10im

d = D1()
@test d.b == "hi"
d = D1(b=100)
@test d.b === 100
