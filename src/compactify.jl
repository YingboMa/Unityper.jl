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

=>

@enum TAGS A B C D
struct AT
    tag::TAGS
    common_field::Int
    a::Bool
    b::Int -> Float64
    c::Float64
    d::Any
end
=#

# TODO: Constructors!!!!!!!!!!!!!

# TODO: compactify Bools to flags
# TODO: composite isbits conversion
# TODO: aggregates isbits conversion (Maybe just NTuple{N, UInt64})

using Base.Meta: isexpr

macro compactify(debug, block)
    _compactify(__module__, block; debug=debug)
end

macro compactify(block)
    _compactify(__module__, block; debug=false)
end

function _compactify(mod, block; debug=false)
    isexpr(block, :block) || error("@compatify takes a block!")
    stmts = block.args
    hasabstract = false
    abstract2concrete = Dict() # abstract type name => ismutable, struct body, [(name, concete fields), ...]
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
                # destructs fields of the form `a::T` to field name and type.
                fields = [isexpr(f, :(::)) ? (f.args[1], expr_to_type(mod, f.args[2])) : (f, Any) for f in fields]
                push!(abstract2concrete[T][end], S => fields)
            end
        else
            error("What is this? $ex")
        end
    end

    hasabstract || error("There must be at least one `@abstract`!")

    expr = Expr(:block)

    gensymidx = Ref(0)
    gensym = let gensymidx = gensymidx
        x -> Symbol("###$x###", (gensymidx[] += 1))
    end

    for (T, (ismutable, struct_body, Ss)) in pairs(abstract2concrete)
        enumtype = gensym("$T")
        EnumNumType = Int32
        # S1=0 S2=1 ...
        enum_pairs = map(i->:($(Ss[i][1]) = $(EnumNumType(i-1))), 1:length(Ss))
        push!(expr.args, :(@enum $enumtype::$EnumNumType $(enum_pairs...)))

        push!(expr.args, struct_body)
        if debug
            @info "Parsed:"
            Base.print_array(stdout, Ss); println()
        end

        isbits_S_ft = []
        nonisbits_S_ft = []
        max_num_isbits = max_num_nonisbits = 0
        for S #=S: struct name | f: field name | t: field type=# in Ss
            S, fts = S
            isbits_ft = []
            nonisbits_ft = []
            push!(isbits_S_ft, S => isbits_ft)
            push!(nonisbits_S_ft, S => nonisbits_ft)
            for (f, t) in fts
                if isbitstype(t)
                    push!(isbits_ft, (f, t))
                else
                    push!(nonisbits_ft, (f, t))
                end
            end
            max_num_isbits = max(max_num_isbits, length(isbits_ft))
            max_num_nonisbits = max(max_num_nonisbits, length(nonisbits_ft))
            sort!(isbits_ft, by=sizeof ∘ last, rev=true)
        end

        # common fields in the abstract type T
        common_fields = [
                         isexpr(f, :(::)) ? f.args[1] : f
                         for f in filter(x->!(x isa LineNumberNode), struct_body.args[3].args)
                        ]

        #compact_fields = []
        #compact_types = []
        S2fields = Dict{
                        Symbol, # S: concrete type's name. We need this to later build getproperty
                                          # :a => (:b, Any=>Complex)
                        Dict{Symbol,Any}, # oldname => (newname, newtype=>oldtype)
                       }()
        #=
        S2fields[:A] => ...
        struct A <: S
            a::T
        end
        struct B <: S
            a::T2
        end
        =#

        for i in 1:max_num_isbits
            siz = idx = 0
            for (j, (S, fts)) in enumerate(isbits_S_ft); i > length(fts) && continue
                f, t = fts[i]
                current_siz = sizeof(t)
                if siz < current_siz
                    siz = current_siz
                    idx = j
                end
            end
            S, fts = isbits_S_ft[idx]
            f, newtype = fts[i]
            newname = gensym(f)
            for (S, fts) in isbits_S_ft; i > length(fts) && continue
                f, t = fts[i]
                namemap = get!(() -> Dict{Symbol,Any}(), S2fields, S)
                namemap[f] = (newname, newtype => t)
            end
            push!(struct_body.args[end].args, :($newname::$newtype))

            #push!(compact_fields, newname)
            #push!(compact_types, newtype)
        end
        for i in 1:max_num_nonisbits
            newtype = Any
            newname = gensym("Any")
            for (S, fts) in nonisbits_S_ft; i > length(fts) && continue
                f, t = fts[i]
                namemap = get!(() -> Dict{Symbol,Any}(), S2fields, S)
                namemap[f] = (newname, newtype => t)
            end
            push!(struct_body.args[end].args, :($newname::$newtype))
        end

        debug && @info "" isbits_S_ft nonisbits_S_ft

        tagname = gensym("tag")
        push!(struct_body.args[end].args, :($tagname::$enumtype))

        # build getproperty
        getprop = :(function (::$(typeof(getproperty)))(x::$T, s::$Symbol) end)
        push!(expr.args, getprop)
        body = getprop.args[end].args
        push!(body, Expr(:meta, :inline))
        ifold = expr
        ifold_original = ifold
        for cs in common_fields
            uninitialized = expr === ifold
            qncs = QuoteNode(cs)
            behavior = Expr(:call, getfield, :x, qncs)
            ifnew = Expr(ifelse(uninitialized, :if, :elseif), :(s === $qncs), behavior)
            uninitialized ? push!(body, ifnew) : push!(ifold.args, ifnew)
            uninitialized && (ifold_original = ifnew)
            ifold = ifnew
        end
        for (S, namemap) in pairs(S2fields)
            enum_num = EnumNumType(findfirst(x->x[1] == S, Ss) - 1)
            # if we are simulating for type `S`.
            behavior = expr
            behavior_og = behavior
            for (oldname, (newname, (newtype, oldtype))) in pairs(namemap)
                uninitialized = expr === behavior
                newf = QuoteNode(newname)
                oldf = QuoteNode(oldname)
                condition = :(s === $oldf)
                behavior′ = :($getfield(x, $newf))
                if newtype === Any
                    behavior′ = :($behavior′::$oldtype)
                else
                    @assert isbitstype(oldtype) && isbitstype(newtype)
                    behavior′ = :($reconstruct($oldtype, $behavior′)::$oldtype)
                end
                if uninitialized
                    behavior_og = behavior = Expr(:if, condition, behavior′)
                else
                    ifnew = Expr(:elseif, condition, behavior′)
                    push!(behavior.args, ifnew)
                    behavior = ifnew
                end
            end
            error_message = :($throw_no_field($(Val(S)), s))
            condition = :($reinterpret($EnumNumType, $getfield(x, $(Meta.quot(tagname)))) === $enum_num)
            uninitialized = expr === ifold
            if behavior === expr
                behavior_og = error_message
            else
                push!(behavior.args, error_message)
            end
            ifnew = Expr(ifelse(uninitialized, :if, :elseif), condition, behavior_og)
            uninitialized ? push!(body, ifnew) : push!(ifold.args, ifnew)
            ifold = ifnew
        end
        @assert expr !== ifold "no getproperty matches?"
    end
    esc(expr)
end

@generated function reconstruct(::Type{T}, x::S) where {T,S}
    @assert isbitstype(T)
    @assert sizeof(T) ≤ sizeof(S)
    if sizeof(T) == 0
        return T.instance
    elseif sizeof(T) == sizeof(S)
        return :(reinterpret($T, x))
    else
        IS = Symbol(:UInt, 8*sizeof(S))
        IT = Symbol(:UInt, 8*sizeof(T))
        return :(reinterpret($T, reinterpret($IS, x) % $IT))
    end
end

@noinline throw_no_field(::Val{S}, s) where {S} = error("type $S has no field $s.")

function reusefield(compact_types, compact_fields, f, t)
    if isempty(compact_types)
        return nothing
    elseif (isbits = isbitstype(t);
            idx = findfirst(t == t′ || !(isbits || isbitstype(t′)) for t′ in compact_types);
            idx !== nothing)
        return compact_fields[idx], (compact_types[idx] => t)
    elseif !isbits
        return gensym(f), (t => Any)
    elseif (idx = findfirst(isbitstype(t′) && sizeof(t) < sizeof(t′) for t′ in compact_types); idx !== nothing)
        return compact_fields[idx], (compact_types[idx] => t)
    else
        return nothing
    end
end

@nospecialize
function expr_to_type(mod::Module, typ)
    typ isa Symbol && return getproperty(mod, typ)
    typ isa Expr || error("oof: $typ")
    base = typ.args[1]
    if base isa Symbol
        baset = getproperty(mod, base)
    else
        @assert Meta.isexpr(base, :curly)
        baset = expr_to_type(mod, base)
    end
    curlytypes = Vector{Any}(undef, length(typ.args)-1)
    for i ∈ eachindex(curlytypes)
        c = typ.args[1+i]
        if c isa Symbol
            curlytypes[i] = getproperty(mod, c)
        elseif Meta.isexpr(base, :curly)
            curlytypes[i] = expr_to_type(mod, c)
        else
            @assert isbitstype(c)
            curlytypes[i] = c
        end
    end
    baset{curlytypes...}
end
@specialize
