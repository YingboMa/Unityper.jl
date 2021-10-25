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
    _compactify(__module__, block)
    nothing
    #block
end

function _compactify(mod, block)
    isexpr(block, :block) || error("@compatify takes a block!")
    stmts = block.args
    hasabstract = false
    abstract2concrete = Dict() # abstract type name => ismutable, body, [(name, concete fields), ...]
    names = []
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
            error("What is this? $ex")
        end
    end

    hasabstract || error("There must be at least one `@abstract`!")

    expr = Expr(:block)

    for (T, (ismutable, struct_body, Ss)) in pairs(abstract2concrete)
        ex = Expr(:block)
        enumtype = gensym("ENUM")
        push!(ex.args, :(@enum $enumtype $(first.(Ss)...)))

        # common fields in the abstract type T
        common_fields = [
                         isexpr(f, :(::)) ? f.args[1] : f
                         for f in filter(x->!(x isa LineNumberNode), struct_body.args[3].args)
                        ]

        compact_fields = []
        compact_types = []

        S2fields = Dict{
                        Symbol, # S: concrete type's name. We need this to later build getproperty
                        Tuple{
                              Vector{Symbol},Dict{Symbol,Any}
                             }, # fieldnames, oldname => (newname, newtype=>oldtype)
                       }()

        for (S, cfields) in Ss
            fields = Symbol[]
            name_map = Dict{Symbol,Any}()
            S2fields[S] = fields, name_map

            for f in cfields
                f, t = isexpr(f, :(::)) ? (f.args[1], getproperty(mod, f.args[2])) : (f, t) # field and type

                push!(fields, f)
                if isempty(compact_fields) ||
                    !isconcretetype(t) ||
                    (idx = findfirst(sizeof(t) <= sizeof(t′) for t′ in compact_types); idx === nothing)
                    # add new fields
                    #
                    # initialize or we cannot optimize this case
                    newname = gensym(f)
                    push!(compact_fields, newname)
                    push!(compact_types, t)
                    name_map[f] = newname, t
                else # reuse old field
                    name_map[f] = compact_fields[idx], (compact_types[idx] => t)
                end
            end
        end

        for (f, t) in zip(compact_fields, compact_types)
            push!(struct_body.args[3].args, :($f::$t))
        end
        push!(ex.args, struct_body)

        push!(expr.args, ex)
        @info "" S2fields compact_fields compact_types
    end

    @show expr
end
