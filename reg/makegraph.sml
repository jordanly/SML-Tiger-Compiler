structure FlowGraph =
    FuncGraph(
        struct
            type ord_key = string
            val compare = String.compare
        end)

structure MakeGraph =
struct
    structure A = Assem
    val stmNum = ref 0
    fun getStmNum () = 
        (stmNum := !stmNum + 1; !stmNum)
    fun makeFlowgraph assemlist = 
        let
            fun addStm (oper as A.OPER{assem,dst,src,jump}, graph) = FlowGraph.addNode(graph, "stm" ^ Int.toString (getStmNum()) ^ " - " ^ assem, oper)
              | addStm (label as A.LABEL{assem,lab}, graph) = FlowGraph.addNode(graph, Symbol.name lab, label)
              | addStm (move as A.MOVE{assem,dst,src}, graph) = FlowGraph.addNode(graph, "stm" ^ Int.toString (getStmNum()) ^ " - " ^ assem, move)
        in
            foldl addStm FlowGraph.empty assemlist
        end
end

(* Useful later for liveness analysis *)
        (*let 
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
        end*)