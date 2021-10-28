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
            if isexpr(T, :<:) # if there's a super type
                T = T.args[1]
            end
            T in names && error("$T struct is already defined")

            field2val = parse_default_value!(struct_body)
            push!(names, T)
            abstract2concrete[T] = ismutable, struct_body, field2val, []
        elseif isexpr(ex, :struct)
            struct_body = ex
            field2val = parse_default_value!(struct_body)
            ismutable, S, fields = struct_body.args
            isexpr(S, :(<:)) || error("$S must be a subtype of some @abstract type!")
            S, T = S.args
            S in names && error("$S struct is already defined")

            T in keys(abstract2concrete) || error("$T >: $S is not a @abstract type.")
            ismut, = abstract2concrete[T]
            ismut == ismutable || error("$S and $T should have the same mutability!")
            push!(names, S)
            fields = filter(x->!(x isa LineNumberNode), fields.args)
            # destructs fields of the form `a::T` to field name and type.
            fields = [isexpr(f, :(::)) ? (f.args[1], expr_to_type(mod, f.args[2])) : (f, Any) for f in fields]
            push!(abstract2concrete[T][end], (S, fields, field2val))
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

    for (T, (ismutable, struct_body, T_field2val, Ss)) in pairs(abstract2concrete)
        EnumType = gensym(T)
        EnumNumType = Int32
        # S1=0 S2=1 ...
        # We need some very weird names so there won't be a name collision.
        # @enum doesn't work with var"##xy".
        enum_pairs = map(i->:($(Symbol(:₋₃₋₁₂₉, T, :₋__, Ss[i][1], :₋₃₋₁₉₉₂₋₋)) = $(EnumNumType(i-1))), 1:length(Ss))
        push!(expr.args, :(@enum $EnumType::$EnumNumType $(enum_pairs...)))

        push!(expr.args, struct_body)
        if debug
            @info "Parsed:"
            Base.print_array(stdout, Ss); println()
        end

        # S: struct name | f: field name | t: field type
        #
        # For non-isbits types, we just make them `Any` and type assert it in
        # the `getproperty` function.
        #
        # First: we initially have something like
        #   sz\S   S1       S2       S3       S4
        #  sizeof  8        8        1        1
        #  sizeof  8        Any      1        1
        #  sizeof  4        1        4        1
        #  sizeof  1        2        8        4
        #  sizeof  2        Any               Any
        #
        # Here, we sort the isbits' field size (the first dim) and separate
        # non-isbits fields. We then have:
        #   sz\S   S1       S2       S3       S4
        #  sizeof  8        8        8        4
        #  sizeof  8        2        4        1
        #  sizeof  4        1        1        1
        #  sizeof  2                 1        1
        #  sizeof  1
        #  Anys             x                 x
        #  Anys             x
        isbits_S_ft = []
        nonisbits_S_ft = []
        max_num_isbits = max_num_nonisbits = 0
        for S in Ss
            S, fts, S_field2val = S
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

        # This structure is needed to generate `getproperty` to simulate the
        # `Ss` types.
        S2fields = Dict{
                        Symbol, # S: concrete type's name. We need this to later build getproperty
                                          # :a => (:b, Any, Complex)
                        Dict{Symbol,Any}, # oldname => (newname, newtype, oldtype)
                       }()
        # This structure is needed to generate constructors.
        back_edges = Dict{Symbol,Any}() # newname => [(S, oldname, oldtype), ...]
        compact_fields = Symbol[]
        compact_types = []

        # We then just need to read-off the sorted fields by taking the max
        # horizonally.
        #
        #   sz\S   S1       S2       S3       S4    [compactified]
        #  sizeof  8        8        8        4           8
        #  sizeof  8        2        4        1           8
        #  sizeof  4        1        1        1           4
        #  sizeof  2                 1        1           2
        #  sizeof  1                                      1
        #  Anys             x                 x           x
        #  Anys             x                             x
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
            edgs = back_edges[newname] = []
            for (S, fts) in isbits_S_ft; i > length(fts) && continue
                f, t = fts[i]
                namemap = get!(() -> Dict{Symbol,Any}(), S2fields, S)
                namemap[f] = (newname, newtype, t)
                push!(edgs, (S, f, t))
            end
            push!(compact_fields, newname)
            push!(compact_types, newtype)
            push!(struct_body.args[end].args, :($newname::$newtype))
        end
        for i in 1:max_num_nonisbits
            newtype = Any
            newname = gensym("Any")
            edgs = back_edges[newname] = []
            for (S, fts) in nonisbits_S_ft; i > length(fts) && continue
                f, t = fts[i]
                namemap = get!(() -> Dict{Symbol,Any}(), S2fields, S)
                namemap[f] = (newname, newtype, t)
                push!(edgs, (S, f, t))
            end
            push!(compact_fields, newname)
            push!(compact_types, newtype)
            push!(struct_body.args[end].args, :($newname::$newtype))
        end

        debug && @info "" isbits_S_ft nonisbits_S_ft

        tagname = gensym("tag")
        tagname_q = Meta.quot(tagname)
        push!(struct_body.args[end].args, :($tagname::$EnumType))

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
            for (oldname, (newname, newtype, oldtype)) in pairs(namemap)
                uninitialized = expr === behavior
                newf = QuoteNode(newname)
                oldf = QuoteNode(oldname)
                condition = :(s === $oldf)
                behavior′ = :($getfield(x, $newf))
                if newtype === Any
                    # type assert doesn't really work here, because
                    # (1+1im)::Complex{Real} errors. Alternatively, we could add
                    # `convert(Complex{Real}, 1+1im)` but this is even worse as
                    # it allocates. So for the time being, we will not check the
                    # type and just return.
                    behavior′ = :($behavior′)
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
            condition = :($reinterpret($EnumNumType, $getfield(x, $tagname_q)) === $enum_num)
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

        # Let's now make the constructor
        construct_args = []
        append!(construct_args, common_fields)
        append!(construct_args, compact_fields)

        for (S, fts, S_field2val) in Ss
            parameters = Expr(:parameters)
            for f in common_fields
                push!(parameters.args, Expr(:kw, f, T_field2val[f]))
            end
            for (f, _) in fts
                push!(parameters.args, Expr(:kw, f, S_field2val[f]))
            end

            constructor = :(function $S($parameters) end)
            constructor_body = constructor.args[end].args

            # We check if the compact field is native in the struct S. If it is
            # native, then we only need to translate oldname to the newname. If
            # it is not native, then we need to set it to some default value.
            for (newname, newtype) in zip(compact_fields, compact_types)
                isany = newtype === Any
                edgs = back_edges[newname]
                is_native = false
                for (S2, oldname, oldtype) in edgs
                    if S2 === S
                        is_native = true
                        if isany # don't call simulate_type for Anys
                            push!(constructor_body, :($newname = $oldname))
                        else
                            push!(constructor_body, :($newname = $simulate_type($newtype, $oldname)))
                        end
                        break
                    end
                end
                is_native && continue
                S2, oldname, oldtype = first(edgs)
                idx = findfirst(x->x[1] === S2, Ss)
                defval = Ss[idx][3][oldname]
                if isany # don't call simulate_type for Anys
                    push!(constructor_body, :($newname = $defval))
                else
                    push!(constructor_body, :($newname = $simulate_type($newtype, $defval)))
                end
            end

            # call the real constructor.
            construct_expr = Expr(:call, T)
            append!(construct_expr.args, construct_args)
            # the type tag is the last arg
            enum_num = EnumNumType(findfirst(x->x[1] == S, Ss) - 1)
            push!(construct_expr.args, Expr(:call, reinterpret, EnumType, enum_num))
            push!(constructor_body, construct_expr)
            push!(expr.args, constructor)
        end

        # Let's do pretty print
        pretty_print = :(function (::$(typeof(Base.show)))(io::$IO, ::$(MIME"text/plain"), obj::$T) end)
        body = pretty_print.args[end].args
        ifold = expr
        for (S, fts, S_field2val) in Ss
            uninitialized = ifold === expr
            enum_num = EnumNumType(findfirst(x->x[1] == S, Ss) - 1)
            enum = Expr(:call, reinterpret, EnumType, enum_num)
            condition = :($enum === $getfield(obj, $tagname_q))
            behavior = Expr(:call, print, :io, Meta.quot(S), "(")
            n = length(fts)
            for (i, (f, _)) in enumerate(fts)
                f = Meta.quot(f)
                push!(behavior.args, f)
                push!(behavior.args, " = ")
                push!(behavior.args, :($getproperty(obj, $f)))
                i == n || push!(behavior.args, ", ")
            end
            push!(behavior.args, ")::")
            push!(behavior.args, T)
            ifnew = Expr(ifelse(uninitialized, :if, :elseif), condition, behavior)
            uninitialized ? push!(body, ifnew) : push!(ifold.args, ifnew)
            ifold = ifnew
        end
        push!(expr.args, pretty_print)
    end
    expr = esc(expr)
    debug && print(expr)
    expr
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

@generated function simulate_type(::Type{T}, x::S) where {T,S}
    @assert isbitstype(T)
    @assert sizeof(T) ≥ sizeof(S)
    if sizeof(T) == 0
        return T.instance
    elseif sizeof(T) == sizeof(S)
        return :(reinterpret($T, x))
    else
        IS = Symbol(:UInt, 8*sizeof(S))
        IT = Symbol(:UInt, 8*sizeof(T))
        return :(reinterpret($T, $IT(reinterpret($IS, x))))
    end
end


@noinline throw_no_field(::Val{S}, s) where {S} = error("type $S has no field $s.")
@noinline throw_type_error(::Val{S}, s) where {S} = error("Expected $S, got $s::$(typeof(s)).")

function parse_default_value!(expr::Expr)
    @assert isexpr(expr, :struct)
    field2val = Dict{Symbol, Any}()
    body = expr.args[end].args
    for (i, ex) in enumerate(body); ex isa LineNumberNode && continue
        isexpr(ex, :(=)) || error("$ex doesn't have a default value!")
        ft, val = ex.args
        body[i] = ft
        f = isexpr(ft, :(::)) ? ft.args[1] : ft
        field2val[f] = val
    end
    field2val
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
