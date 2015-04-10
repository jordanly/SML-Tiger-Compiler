signature LIVENESS =
sig
    structure TempKeyGraph : FUNCGRAPH
    type igraphentry = {}

    val interferenceGraph :
            MakeGraph.graphentry StrKeyGraph.graph ->
                igraphentry TempKeyGraph.graph *
                string StrKeyGraph.node Temp.Map.map

    val show : TextIO.outstream * igraphentry TempKeyGraph.graph -> unit
end
