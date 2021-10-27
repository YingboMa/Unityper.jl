using Unityper, Test

@compactify begin
    @abstract struct AT
        common_field::Int
    end
    struct A <: AT
        a::Bool
        b::Int
    end
    struct B <: AT
        a::Int
        b::Float64
        d::Complex # not isbits
    end
    struct C <: AT
        b::Float64
        d::Bool
        e::Float64
        k::Complex{Real} # not isbits
    end
    struct D <: AT
        b::Any # not isbits
    end
end

@test length(fieldnames(AT)) == 6
at = AT(1, 1, 1.0, true, 1 + 1im, reinterpret(var"###AT###1", Int32(0)))
# This is actually mapped to Float64! Surprise!
@test !at.a
@test at.b isa Int
@test_throws Any at.c

abstract type AAT end
@compactify begin
    @abstract struct BT <: AAT
        common_field::Int
    end
    struct A1 <: BT
        a::Bool
        b::Int
    end
    struct B1 <: BT
        a::Int
        b::Float64
        d::Complex # not isbits
    end
    struct C1 <: BT
        b::Float64
        d::Bool
        e::Float64
        k::Complex{Real} # not isbits
    end
    struct D1 <: BT
        b::Any # not isbits
    end
end
@test BT <: AAT

@test length(fieldnames(BT)) == 6
at = BT(1, 1, 1.0, true, 1 + 1im, reinterpret(var"###BT###1", Int32(0)))
# This is actually mapped to Float64! Surprise!
@test !at.a
@test at.b isa Int
@test_throws Any at.c
