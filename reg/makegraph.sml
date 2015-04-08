structure G = FuncGraph(Temp.TempOrd)

structure MakeGraph =
struct
    structure A = Assem
    fun makeFlowgraph assemlist = 
        let 
            fun addNodes graph = 
                let
                    val currentTemp = Temp.currentTemp()
                    val startValue = 100
                    fun addTemp (graph, i) =
                        if i <= currentTemp
                        then addTemp(G.addNode(graph, i, i), i+1)
                        else graph
                in
                    addTemp(graph, startValue)
                end
            fun addEdges (oper as A.OPER{assem,dst,src,jump}, graph) = graph
              | addEdges (label as A.LABEL{assem,lab}, graph) = graph
              | addEdges (move as A.MOVE{assem,dst,src}, graph) = graph
        in
            foldl addEdges (addNodes G.empty) assemlist
        end
end