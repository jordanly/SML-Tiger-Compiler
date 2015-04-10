structure Liveness : LIVENESS =
struct
    structure TempKeyGraph = FuncGraph(Temp.TempOrd)

    type igraphentry = {}

    fun interferenceGraph(flowgraph : MakeGraph.graphentry StrKeyGraph.graph) 
        = (TempKeyGraph.empty, Temp.Map.empty)
    fun show(out, igraph) = ()
end
