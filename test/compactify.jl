using Unityper, Test, Setfield, ConstructionBase

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

@test Unityper.subtypes_fun(Val(AT)) == (:A, :B, :C, :D)

a = A()
@test a.a
@test a.b === 10
a = A(; a=false, b=42)
@test !a.a
@test a.b === 42
# 3-arg show:
@test sprint(show,  MIME"text/plain"(), a) == "A(a = false, b = 42)::AT"
# 2-arg show:
@test sprint(show, a) == "A(a = false, b = 42)::AT"
@test sprint(show, [a]) == "AT[A(a = false, b = 42)::AT]"
@test Unityper.isa_type_fun(Val(AT), Val(:A), a)
@test !Unityper.isa_type_fun(Val(AT), Val(:D), a)

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

@test_throws Any @macroexpand @compactified a::AT begin
    A => "A"
    B => "B"
    C => "C"
    A => "D"
end
@test_throws Any @macroexpand @compactified a::AT begin
    A => "A"
    B => "B"
    C => "C"
end
foo(a) = @compactified a::AT begin
    A => "A"
    B => "B"
    C => "C"
    D => "D"
end
@test foo(a) == "A"
@test foo(b) == "B"
@test foo(c) == "C"
@test foo(d) == "D"

@test_throws Any @macroexpand @compactified a::AT begin
    A => "A"
    _ => "B"
    C => "C"
end
goo(a) = @compactified a::AT begin
    A => "A"
    _ => "B"
end
@test goo(a) == "A"
@test goo(b) == "B"
@test goo(c) == "B"
@test goo(d) == "B"

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
@test Unityper.subtypes_fun(Val(BT)) == (:A1, :B1, :C1, :D1)

a = A1()
@test a.a
@test a.b === 10
a = A1(; a=false, b=42)
@test !a.a
@test a.b === 42
@test_throws Any Unityper.isa_type_fun(Val(AT), Val(:A1), a)
@test_throws Any Unityper.isa_type_fun(Val(BT), Val(:A), a)
@test Unityper.isa_type_fun(Val(BT), Val(:A1), a)
@test !Unityper.isa_type_fun(Val(BT), Val(:D1), a)

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


@compactify begin
    @abstract struct AT′{T}
        common_field::Int = 0
    end
    struct A′{T} <: AT′{T}
        a::Bool = true
        b::Int = 10
    end
    struct B′{T} <: AT′{T}
        a::Int = 1
        b::Float64 = 1.0
        d::Complex = 1 + 1.0im # not isbits
    end
end

a = A′{Int}()
@compactified a::AT′ begin
    A′ => "A"
    B′ => "B"
    _ => error("unreachable")
end

@test typeof(A′{Real}()) == AT′{Real}
@test typeof(B′{Int}()) == AT′{Int}

function ConstructionBase.setproperties_object(obj::AT′{T}, patch) where T
    nt = getproperties(obj)
    nt_new = merge(nt, patch)
    Unityper.rt_constructor(obj){T}(;nt_new...)
end

@set! a.a = false
@set! a.b = 1234
@test !a.a
@test a.b === 1234
