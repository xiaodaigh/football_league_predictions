include("0a_setup.jl")

R"""
fixtures = fst::read_fst("data/fixtures.fst")
""";

@rget fixtures;

clubelo = FileIO.load("data/clubelo.jld","clubelo");