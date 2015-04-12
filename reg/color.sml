signature COLOR = 
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Table.table
    val color: {igraph: Liveness.igraphentry TempKeyGraph.graph, 
                initial: allocation,
                spillCost: Liveness.igraphentry -> int,
                registers: Frame.register list}
                -> allocation * Temp.temp list
end

structure Color : COLOR = 
struct
    structure Frame = MipsFrame
    structure TG = TempKeyGraph
    structure TT = Temp.Table
    type allocation = Frame.register Temp.Table.table

    (* Returns true if the 'a option argument is NONE *)
    fun isNone NONE = true | isNone (SOME _) = false

    (* Returns a list of temps that are in the graph but not in the register allocation table *)
    fun findNodesToSpill(graph, allocation) = 
        let
            fun addNodeIfNecessary(node, listSoFar) = 
                if (isNone(TT.look(allocation, TG.getNodeID node)))
                then (TG.getNodeID node)::listSoFar
                else listSoFar 
        in
            foldl addNodeIfNecessary [] (TG.nodes graph)
        end

    (* Finds a simplifiable ID in an igraph, or none if no node is simplifiable *)
    fun getSimplifiableID (_, _, _, []) = NONE
      | getSimplifiableID (igraph, initial, registers, possibleNode::rest) = 
            let
                val numregisters = List.length registers
                val possibleID = TG.getNodeID(possibleNode)
                val degree = TG.outDegree(possibleNode)
            in
                if degree < numregisters andalso isNone(TT.look(initial, possibleID))
                then SOME possibleID
                else getSimplifiableID(igraph, initial, registers, rest)
            end

    (* Returns SOME suitable color for the given node, or NONE if it's not colorable*)
    fun findColor(igraph, nodeID, initial, []) = NONE
      | findColor(igraph, nodeID, initial, possibleColor::rest) = 
            let
                fun foldHelper (adjnode, boolean) : bool = boolean orelse
                    case TT.look(initial, adjnode) of
                        SOME adjColor => (possibleColor = adjColor)
                      | NONE => false
                val colorIsBad : bool = foldl foldHelper false (TG.adj(TG.getNode(igraph, nodeID)))
            in
                if colorIsBad
                then findColor(igraph, nodeID, initial, rest)
                else SOME possibleColor
            end

    (* Augment the initial register allocation, also returns a list of spills *)
    fun color {igraph, initial, spillCost, registers} =
        case getSimplifiableID (igraph, initial, registers, TG.nodes igraph) of
            SOME simplifiableID => 
                let
                    val simplifiableNode = TG.getNode(igraph, simplifiableID)
                    val simplifiedGraph = TG.removeNode(igraph, simplifiableID)
                    val (tempAlloc, spilllist) = color{igraph=simplifiedGraph,
                                    initial=initial,
                                    spillCost=spillCost,
                                    registers=registers}
                in
                    case findColor(igraph, simplifiableID, tempAlloc, registers) of
                        SOME foundcolor => (TT.enter(tempAlloc, simplifiableID, foundcolor), spilllist)
                      | NONE =>  (tempAlloc, simplifiableID::spilllist)
                end
          | NONE => (initial, findNodesToSpill(igraph, initial))
end