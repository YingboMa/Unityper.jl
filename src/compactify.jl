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
    b::Float64
end
struct C <: AT
    b::Float64
    d::Bool
    e::Int
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

        Ss = [(S, [
                   isexpr(f, :(::)) ? (f.args[1], getproperty(mod, f.args[2])) : (f, t) # field and type
                   for f in cfields
                  ])
              for (S, cfields) in Ss]

        # There are two cases which we want to compactify fields.
        # (1): We have
        # ```
        # struct A
        #     a::Float64
        #     b::Bool
        # end
        # struct B
        #     a::Float64
        #     b::Float64
        # end
        # compact_fields = [Float64, Bool]
        # ```
        # In this case, we want to replace the `Bool` with `Float64`.
        #
        # (2): We have
        # ```
        # struct B
        #     a::Float64
        #     b::Float64
        # end
        # struct A
        #     a::Float64
        #     b::Bool
        # end
        # compact_fields = [Float64, Float64]
        # ```
        # In this case, we simply reuse the `Float64`
        #
        # To handle these two cases, we just need to lexicographically sort the
        # concrete structs by their field sizes, i.e. (Float64, Bool) -> (8, 1).
        # After the sorting, we only need to consider the easy-to-handle second
        # case.
        foreach(fts->sort!(fts[2], rev=true, by=x->sizeof(x[2])), Ss) # sort for each field by its size
        function bylex(x)
            _, fts = x
            idxs = findall(isbitstype(t) for (f, t) in fts)
            (map(i->sizeof(fts[i][2]), idxs)...,)
        end
        sort!(Ss, rev=true, by=bylex) # sort for each concrete type by the size of the first field

        # We should never replace reuse fields in the same type, so we only
        # search in previously added types.
        searchsize = 0
        for (S, fts) in Ss # for each concrete type
            fields = Symbol[]
            name_map = Dict{Symbol,Any}()
            S2fields[S] = fields, name_map

            for (f, t) in fts # for each field
                push!(fields, f)
                if isempty(compact_fields) ||
                    !isconcretetype(t) ||
                    (idx = findfirst(sizeof(t) <= sizeof(t′) for t′ in view(compact_types, 1:searchsize)); idx === nothing)
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

            searchsize = length(compact_types)
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
