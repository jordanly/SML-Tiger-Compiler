structure Liveness : LIVENESS =
struct
    structure FlowNodeTempMap = SplayMapFn(
                                  struct
                                    type ord_key = string
                                    val compare = String.compare
                                  end)


    type igraphentry = {}

    fun interferenceGraph(flowgraph : MakeGraph.graphentry StrKeyGraph.graph) 
        = (TempKeyGraph.empty, FlowNodeTempMap.empty)
    fun show(out, igraph) = ()
end
