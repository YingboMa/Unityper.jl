#=
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
end
struct C <: AT
    b::Float64
end
struct D <: AT
    b::Any
end

end
=#

using Base.Meta: isexpr

macro compactify(block)
    _compactify(block)
    nothing
    #block
end

function _compactify(block)
    isexpr(block, :block) || error("@compatify takes a block!")
    stmts = block.args
    hasabstract = false
    abstract2concrete = Dict() # abstract type name => ismutable, body, [(name, concete fields), ...]
    names = []
    expr = Expr(:block)
    # notation: T means abstract name, S means concrete name
    for ex in stmts; ex isa LineNumberNode && continue
        if isexpr(ex, :macrocall) && ex.args[1] === Symbol("@abstract")
            hasabstract = true
            struct_body = ex.args[3]
            @assert isexpr(struct_body, :struct)
            ismutable, T, fields = struct_body.args
            if T in names
                error("$T struct is already defined")
            else
                push!(names, T)
                abstract2concrete[T] = ismutable, struct_body, []
            end
        elseif isexpr(ex, :struct)
            @info "" ex
            struct_body = ex
            ismutable, S, fields = struct_body.args
            isexpr(S, :(<:)) || error("$S must be a subtype of some @abstract type!")
            S, T = S.args
            if S in names
                error("$S struct is already defined")
            else
                T in keys(abstract2concrete) || error("$T >: $S is not a @abstract type.")
                ismut, = abstract2concrete[T]
                ismut == ismutable || error("$S and $T should have the same mutability!")
                push!(names, S)
                fields = filter(x->!(x isa LineNumberNode), fields.args)
                push!(abstract2concrete[T][end], (S, fields))
            end
        else
            push!(expr.args, ex)
        end
    end
    @info "" abstract2concrete

    hasabstract || error("There must be at least one `@abstract`!")
    #pushfirst!(expr.args, :(@enum $(concrete_names...)))
    enumexpr = Expr(:block)

    for (T, (ismutable, struct_body, Ss)) in pairs(abstract2concrete)
        afields = filter(x->!(x isa LineNumberNode), struct_body.args[3].args)
        common_fields = [isexpr(f, :(::)) ? f.args[1] : f for f in afields]

        compact_fields = []
        for (S, cfields) in Ss, f in cfields
            f, t = isexpr(f, :(::)) ? f.args : (f, :Any)

            @info "" afields cfields
        end
    end

    @info "" expr
end
