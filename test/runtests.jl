using Unityper
using Test

@testset "Unityper.jl" begin
    @time include("compactify.jl")
end
