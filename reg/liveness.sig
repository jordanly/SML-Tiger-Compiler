signature LIVENESS =
sig
    structure TempKeyGraph : FUNCGRAPH
    structure FlowNodeTempMap : ORD_MAP
    type igraphentry = {}

    val interferenceGraph :
            MakeGraph.graphentry StrKeyGraph.graph ->
                igraphentry TempKeyGraph.graph *
                Temp.temp list FlowNodeTempMap.map

    val printGraphNode : TempKeyGraph.nodeID * 'Z -> string
    val show : TextIO.outstream * igraphentry TempKeyGraph.graph -> unit
end
