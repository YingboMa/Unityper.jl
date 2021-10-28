module Unityper

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

export @compactify, @compactified
include("compactify.jl")
include("compactified.jl")

end
