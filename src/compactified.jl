macro compactified(head, block)
    _compactified(__module__, head, block; debug=true)
end

function _compactified(mod, head, block; debug=false)
    @assert isexpr(head, :(::)) "The head must be in the form of `a::AT`!"
    @assert isexpr(block, :block) "There must be a block!"
    var, T = head.args
    T = expr_to_type(mod, T)
    Ss = collect(subtypes_fun(Val(T)))
    has_match_all = false
    seenS = Symbol[]
    expr = Expr(:block)
    body = expr.args
    ifold = expr
    lnn = LineNumberNode(0, "")
    islast = false
    for ex in block.args
        islast && error("_ must be the last case!")
        if ex isa LineNumberNode # debug info
            lnn = ex
            continue
        end
        # error checking
        @assert isexpr(ex, :call) && ex.args[1] === :(=>) "Matching must be in the form of `A => println(\"A\")`!"
        _, S, s_body = ex.args
        if S == :_
            has_match_all = true
            islast = true
        else
            S in Ss || error("$S is not a subtype of $(T)!")
            S in seenS && error("$S handling is duplicated!")
            push!(seenS, S)
        end

        uninitialized = ifold === expr
        condition = islast ? true : Expr(:call, isa_type_fun, Val(T), Val(S), var)
        ifnew = Expr(ifelse(uninitialized, :if, :elseif), condition, Expr(:block, lnn, s_body))
        uninitialized ? push!(body, ifnew) : push!(ifold.args, ifnew)
        ifold = ifnew
    end
    if !has_match_all
        unhandled = setdiff(Ss, seenS)
        @assert isempty(unhandled) "Non-exhaustive handling of $T: $(join(unhandled, ", ")) are not handled!"
    end
    esc(expr)
end
