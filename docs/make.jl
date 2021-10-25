using Unityper
using Documenter

DocMeta.setdocmeta!(Unityper, :DocTestSetup, :(using Unityper); recursive=true)

makedocs(;
    modules=[Unityper],
    authors="Yingbo Ma <mayingbo5@gmail.com> and contributors",
    repo="https://github.com/Yingbo Ma/Unityper.jl/blob/{commit}{path}#{line}",
    sitename="Unityper.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://Yingbo Ma.github.io/Unityper.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/Yingbo Ma/Unityper.jl",
    devbranch="master",
)
