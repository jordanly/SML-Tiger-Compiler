signature LIVENESS =
sig
    structure FlowNodeTempMap : ORD_MAP
    type igraphentry = {}
    type liveSetEntry = {liveIn : Temp.Set.set, liveOut: Temp.Set.set}

    val interferenceGraph :
            MakeGraph.graphentry StrKeyGraph.graph ->
                igraphentry TempKeyGraph.graph *
                liveSetEntry FlowNodeTempMap.map *
                (TempKeyGraph.nodeID * TempKeyGraph.nodeID) list

    val printGraphNode : TempKeyGraph.nodeID * 'Z -> string
    val show : TextIO.outstream * igraphentry TempKeyGraph.graph -> unit
end
