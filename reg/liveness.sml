structure Liveness : LIVENESS =
struct
    structure FlowNodeTempMap = SplayMapFn(
                                  struct
                                    type ord_key = string
                                    val compare = String.compare
                                  end)

    type igraphentry = {}
    type liveSetEntry = 
          {liveIn: Temp.Set.set,
           liveOut: Temp.Set.set}

    (* DEBUG *)
    fun printSet(set, name) = 
      (print ("Printing set " ^ name ^ "-----\n");
      app (fn temp => print (Temp.makestring temp ^ ", ")) (Temp.Set.listItems set);
      print "\n")

    (* Pre-process step, create empty in/out sets for each flow now *)
    fun createEmptyLiveNodes(flowgraph : MakeGraph.graphentry StrKeyGraph.graph) =
        let
          val nodeList = map StrKeyGraph.getNodeID (StrKeyGraph.nodes flowgraph)
          fun addEntry(nodeID, curMap) = FlowNodeTempMap.insert(curMap, nodeID, {liveIn=Temp.Set.empty, liveOut=Temp.Set.empty})
        in
          foldl addEntry FlowNodeTempMap.empty nodeList
        end

    fun processNode(flowGraph, nodeID, liveMap) =
        let
          val liveEntry = case FlowNodeTempMap.find(liveMap, nodeID) of
                              SOME(entry) => entry
                            | NONE => (print ("Liveness.sml: could not find node: " ^ nodeID);
                                       {liveIn=Temp.Set.empty, liveOut=Temp.Set.empty})
          val flowGraphNode = StrKeyGraph.getNode(flowGraph, nodeID)
          val succs = StrKeyGraph.succs flowGraphNode
          fun calcOut(nodeID) =
              let
                fun getIn(nodeID) = 
                  case FlowNodeTempMap.find(liveMap, nodeID) of
                       SOME({liveIn, liveOut}) => liveIn
                     | NONE => (print ("Liveness.sml: could not find node: " ^ nodeID);
                                Temp.Set.empty)
                val succInList = map getIn succs
              in
                foldl Temp.Set.union Temp.Set.empty succInList
              end
          fun calcIn(liveEntry as {liveIn, liveOut}, flowGraphEntry : MakeGraph.graphentry) =
              let
                fun getOut(nodeID) = 
                  case FlowNodeTempMap.find(liveMap, nodeID) of
                       SOME({liveIn, liveOut}) => liveOut
                     | NONE => (print ("Liveness.sml: could not find node: " ^ nodeID);
                                Temp.Set.empty)
                val use = #use flowGraphEntry
                val def = #def flowGraphEntry
                val defSet = foldl Temp.Set.add' Temp.Set.empty def
                val useSet = foldl Temp.Set.add' Temp.Set.empty use
              in
                Temp.Set.union(useSet, Temp.Set.difference(defSet, liveOut))
              end
        in
          {liveIn=calcIn(liveEntry, StrKeyGraph.nodeInfo flowGraphNode), liveOut=calcOut(nodeID)}
        end

    fun computeLiveness(flowGraph, liveMap) =
        let
          fun checkChanged(liveEntry as {liveIn=liveIn, liveOut=liveOut}, liveEntry' as {liveIn=liveIn', liveOut=liveOut'}) =
              not (Temp.Set.equal(liveIn, liveIn') andalso Temp.Set.equal(liveOut, liveOut'))
          fun iterator(nodeID, (curMap, isChanged)) = 
              let
                val liveEntry = case FlowNodeTempMap.find(curMap, nodeID) of
                                     SOME(entry) => entry
                                   | NONE => (print ("Liveness.sml: could not find node: " ^ nodeID);
                                             {liveIn=Temp.Set.empty, liveOut=Temp.Set.empty})
                val liveEntry' = processNode(flowGraph, nodeID, curMap)
                val newMap = FlowNodeTempMap.insert(curMap, nodeID, liveEntry')
                val changedStatus = if isChanged = false
                                    then checkChanged(liveEntry, liveEntry')
                                    else true
              in
                (newMap, changedStatus)
              end
          fun runUntilFixed(flowGraph, liveMap) = 
              let
                val nodeIDs = map StrKeyGraph.getNodeID (StrKeyGraph.nodes flowGraph)
                val (newMap, changed) = foldl iterator (liveMap, false) nodeIDs
              in
                if changed
                then runUntilFixed(flowGraph, newMap)
                else newMap
              end
        in
          runUntilFixed(flowGraph, liveMap)
        end

    fun interferenceGraph(flowGraph : MakeGraph.graphentry StrKeyGraph.graph) = 
        let
          val liveMap = createEmptyLiveNodes(flowGraph)
          val liveness = computeLiveness(flowGraph, liveMap)
        in
          (TempKeyGraph.empty, FlowNodeTempMap.empty)
        end

    fun printGraphNode(id, node) = (Int.toString id)

    fun show(out, igraph) = TempKeyGraph.printGraph printGraphNode igraph
end
