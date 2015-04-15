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

    fun printGraphNode(id, node) = (Int.toString id)

    fun show(out, igraph) = TempKeyGraph.printGraph printGraphNode igraph

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
                Temp.Set.union(useSet, Temp.Set.difference(liveOut, defSet))
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

    fun createInterferenceGraph(flowGraph, liveMap) =
        let
          val nodes : MakeGraph.graphentry StrKeyGraph.node list = StrKeyGraph.nodes flowGraph
          val iGraph =
              let
                fun getTemps(node : MakeGraph.graphentry StrKeyGraph.node, set) = 
                    let
                      val def = #def (StrKeyGraph.nodeInfo node)
                      val use = #use (StrKeyGraph.nodeInfo node)
                    in
                      Temp.Set.addList (set, use @ def)
                    end
                fun addNode(temp, graph) = TempKeyGraph.addNode(graph, temp, {})
                val tempList = Temp.Set.listItems (foldl getTemps Temp.Set.empty nodes)
              in
                foldl addNode TempKeyGraph.empty tempList
              end
          (* Assumes every node is already in interference graph *)
          fun processDefs(node : MakeGraph.graphentry StrKeyGraph.node, iGraph) = 
              let
                val nodeID = StrKeyGraph.getNodeID node
                val nodeInfo = StrKeyGraph.nodeInfo node
                val defs = #def nodeInfo
                val liveEntry = case FlowNodeTempMap.find(liveMap, nodeID) of
                                     SOME(entry) => entry
                                   | NONE => (print ("Liveness.sml: could not find node: " ^ nodeID);
                                             {liveIn=Temp.Set.empty, liveOut=Temp.Set.empty})
                val liveOutTemps = Temp.Set.listItems(#liveOut liveEntry)
                fun process(defTemp, [], iGraph) = iGraph
                  | process(defTemp, outTemp::l, iGraph) = 
                      let
                        (* If move and temps are same, do not create edge *)
                        val newGraph = 
                          case #ismove nodeInfo of
                               true => if Temp.compare(defTemp, outTemp) = EQUAL
                                       then iGraph
                                       else TempKeyGraph.doubleEdge(iGraph, defTemp, outTemp)
                             | false => TempKeyGraph.doubleEdge(iGraph, defTemp, outTemp)
                      in
                        process(defTemp, l, newGraph)
                      end

                fun iterate([], outTempList, iGraph) = iGraph
                  | iterate(defTemp::l, outTempList, iGraph) = iterate(l, outTempList, process(defTemp, outTempList, iGraph))
              in
                iterate(defs, liveOutTemps, iGraph)
              end
        in
          foldl processDefs iGraph nodes
        end
    
    fun createMoveList(flowGraph : MakeGraph.graphentry StrKeyGraph.graph, iGraph : igraphentry TempKeyGraph.graph) =
        let
          fun iterator(node, curList) =
              let
                val {def=defs, use=uses, ismove=ismove} = StrKeyGraph.nodeInfo node
                fun getZeroth(tempList) = TempKeyGraph.getNode(iGraph, List.nth(tempList, 0))
              in
                (* if it's a move and there's no interference, valid for coalesce *)
                if ismove = true andalso not (TempKeyGraph.isAdjacent(getZeroth(defs), getZeroth(uses)))
                then (List.nth(defs, 0), List.nth(uses, 0))::curList
                else curList
              end
        in
          foldl iterator [] (StrKeyGraph.nodes flowGraph)
        end

    fun interferenceGraph(flowGraph : MakeGraph.graphentry StrKeyGraph.graph) = 
        let
          val liveMap = createEmptyLiveNodes(flowGraph)
          val liveMap' = computeLiveness(flowGraph, liveMap)
          fun printNodes(curMap) = 
              let
                fun getEntry(nodeID) = case FlowNodeTempMap.find(curMap, nodeID) of
                                     SOME(entry) => entry
                                   | NONE => (print ("Liveness.sml: could not find node: " ^ nodeID);
                                             {liveIn=Temp.Set.empty, liveOut=Temp.Set.empty})
                fun printNode(node) = (print ("Node name = " ^ StrKeyGraph.getNodeID node ^ "\n");
                                       printSet((#liveIn (getEntry(StrKeyGraph.getNodeID node))), "LIVEIN");
                                       printSet((#liveOut (getEntry(StrKeyGraph.getNodeID node))), "LIVEOUT"))
              in
                app printNode (StrKeyGraph.nodes flowGraph)
              end
          val _ = print ("========== printing nodes for live-in/live-out =========\n")
          val _ = printNodes(liveMap')
          val _ = print ("========== printing interference graph =========\n")
          val iGraph : igraphentry TempKeyGraph.graph = createInterferenceGraph(flowGraph, liveMap')
          val moveList = createMoveList(flowGraph, iGraph)
          val _ = TempKeyGraph.printGraph printGraphNode iGraph
          val _ = print "moving\n"
          val _ = app (fn (x, y) => print ("move from " ^ Int.toString(x) ^ " to " ^ Int.toString(y) ^ "\n")) moveList
        in
          (iGraph, liveMap', moveList)
        end
end
