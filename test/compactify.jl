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
