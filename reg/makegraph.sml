structure StrKeyGraph =
    FuncGraph(
        struct
            type ord_key = string
            val compare = String.compare
        end)

structure MakeGraph =
struct
    structure A = Assem

    type graphentry =
            {def: Temp.temp list,
             use: Temp.temp list}

    fun assemID stmNum assem = "stm" ^ (Int.toString stmNum) (*^ " - " ^ assem*)

    fun makeFlowgraphNodes assemlist = 
        let
            val stmNum = ref 0
            fun getStmNum () = !stmNum
            fun incStmNum () = stmNum := !stmNum + 1        
            fun addStm (oper as A.OPER{assem,dst,src,jump}, graph) = (incStmNum(); StrKeyGraph.addNode(graph, assemID (getStmNum()) assem, {def=dst, use=src}))
              | addStm (label as A.LABEL{assem,lab}, graph) = (incStmNum(); StrKeyGraph.addNode(graph, Symbol.name lab, {def=[], use=[]}))
              | addStm (move as A.MOVE{assem,dst,src}, graph) = (incStmNum(); StrKeyGraph.addNode(graph, assemID (getStmNum()) assem, {def=[dst], use=[src]}))
        in
            foldl addStm StrKeyGraph.empty assemlist
        end

    fun addFlowgraphEdges (graph, assemlist) = 
        let
            val stmNum = ref 0
            fun getStmNum () = !stmNum
            fun incStmNum () = stmNum := !stmNum + 1  

            (* Third arg represents fall-through instruction:
               SOME nodeID if previous assembly instruction can
               fall through to this one, or NONE otherwise *)
            fun addEdgeHelper (graph, [], _) = graph
              | addEdgeHelper (graph, a::l, NONE) =
                    (incStmNum();
                    case a of
                        A.OPER{assem,dst,src,jump=NONE} => addEdgeHelper(graph, l, SOME (assemID (getStmNum()) assem))
                      | A.OPER{assem,dst,src,jump=SOME jumplist} => 
                            let
                                val graph' = foldl
                                            (fn (label, graph) => StrKeyGraph.addEdge(graph, {from=assemID (getStmNum()) assem, to=Symbol.name label}))
                                            graph
                                            jumplist
                                handle NoSuchNode => Err.impossible "can't find node" (* TODO *) 
                            in
                                addEdgeHelper(graph', l, NONE)
                            end
                      | A.LABEL{assem,lab} => addEdgeHelper(graph, l, SOME (Symbol.name lab))
                      | A.MOVE{assem,dst,src} => addEdgeHelper(graph, l, SOME (assemID (getStmNum()) assem))
                    )
              | addEdgeHelper (graph, a::l, SOME fallthroughpred) =
                    (incStmNum();
                    case a of
                        A.OPER{assem,dst,src,jump=NONE} =>
                            let val graph' = StrKeyGraph.addEdge(graph, {from=fallthroughpred, to=assemID (getStmNum()) assem})
                            in addEdgeHelper(graph', l, SOME (assemID (getStmNum()) assem))
                            end
                      | A.OPER{assem,dst,src,jump=SOME jumplist} => 
                            let
                                val graph' = foldl
                                            (fn (label, graph) => StrKeyGraph.addEdge(graph, {from=assemID (getStmNum()) assem, to=Symbol.name label}))
                                            graph
                                            jumplist
                                val graph'' = StrKeyGraph.addEdge(graph', {from=fallthroughpred, to=assemID (getStmNum()) assem})
                                handle NoSuchNode => Err.impossible "can't find node" (* TODO: Debug why there are no edges being added *)      
                            in
                                addEdgeHelper(graph'', l, NONE)
                            end
                      | A.LABEL{assem,lab} =>
                            let val graph' = StrKeyGraph.addEdge(graph, {from=fallthroughpred, to=Symbol.name lab})
                            in addEdgeHelper(graph', l, SOME (Symbol.name lab))
                            end
                      | A.MOVE{assem,dst,src} =>
                            let val graph' = StrKeyGraph.addEdge(graph, {from=fallthroughpred, to=assemID (getStmNum()) assem})
                            in addEdgeHelper(graph', l, SOME (assemID (getStmNum()) assem))
                            end
                    )
        in
            addEdgeHelper(graph, assemlist, NONE)
        end

    fun makeFlowgraph assemlist = addFlowgraphEdges (makeFlowgraphNodes assemlist, assemlist)
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