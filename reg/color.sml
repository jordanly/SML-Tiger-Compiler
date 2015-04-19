signature COLOR = 
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Table.table
    val color: {igraph: Liveness.igraphentry TempKeyGraph.graph, 
                initial: allocation,
                spillCost: Liveness.igraphentry -> int,
                registers: Frame.register list,
                movelist : (Temp.temp * Temp.temp) list}
                -> allocation * Temp.temp list
end

structure Color : COLOR = 
struct
    structure Frame = MipsFrame
    structure TG = TempKeyGraph
    structure TT = Temp.Table
    type allocation = Frame.register Temp.Table.table

    (* SOME unwrapper *)
    fun getSome (SOME x) = x | getSome NONE = "NO REGISTER FOUND"

    (* Returns true if the 'a option argument is NONE *)
    fun isNone NONE = true | isNone (SOME _) = false

    (* Returns SOME move partner of the given temp or NONE *)
    fun getMovePartner([], temp) = NONE
      | getMovePartner((src, dst)::rest, temp) =
        if src = temp
        then SOME dst
        else if dst = temp
        then SOME src
        else getMovePartner(rest, temp)

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
    fun getSimplifiableID (_, _, _, movelist, []) = NONE
      | getSimplifiableID (igraph, initial, registers, movelist, possibleNode::rest) = 
            let
                val numregisters = List.length registers
                val possibleID = TG.getNodeID(possibleNode)
                val degree = TG.outDegree(possibleNode)
            in
                if degree < numregisters andalso isNone(TT.look(initial, possibleID)) andalso isNone(getMovePartner(movelist, possibleID))
                then SOME possibleID
                else getSimplifiableID(igraph, initial, registers, movelist, rest)
            end

    (* Returns SOME suitable color for the given node, or NONE if it's not colorable*)
    fun findColor(igraph, nodeID, initial, []) = NONE
      | findColor(igraph, nodeID, initial, possibleColor::rest : Frame.register list) = 
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

    (* Returns SOME suitable color for the given nodes, or NONE if they're not colorable*)
    fun findColorCoalesced(igraph, t1, t2, initial, []) = NONE
      | findColorCoalesced(igraph, t1, t2, initial, possibleColor::rest) = 
            let
                fun foldHelper (adjnode, boolean) : bool = boolean orelse
                    case TT.look(initial, adjnode) of
                        SOME adjColor => (possibleColor = adjColor)
                      | NONE => false
                val colorIsBad : bool = foldl foldHelper false (TG.adj(TG.getNode(igraph, t1)) @ TG.adj(TG.getNode(igraph, t2)))
            in
                if colorIsBad
                then findColorCoalesced(igraph, t1, t2, initial, rest)
                else SOME possibleColor
            end

    (* Augment the initial register allocation, also returns a list of spills *)
    fun color {igraph, initial, spillCost, registers, movelist} =
        case getSimplifiableID (igraph, initial, registers, movelist, TG.nodes igraph) of
            SOME simplifiableID => 
                let
                    val simplifiableNode = TG.getNode(igraph, simplifiableID)
                    val simplifiedGraph = TG.removeNode(igraph, simplifiableID)
                    val (tempAlloc, spilllist) = color{igraph=simplifiedGraph,
                                    initial=initial,
                                    spillCost=spillCost,
                                    registers=registers,
                                    movelist=movelist}
                in
                    case findColor(igraph, simplifiableID, tempAlloc, registers) of
                        SOME foundcolor => (TT.enter(tempAlloc, simplifiableID, foundcolor), spilllist)
                      | NONE =>  (tempAlloc, simplifiableID::spilllist)
                end
          | NONE => 
                if List.length(movelist) = 0
                then (initial, findNodesToSpill(igraph, initial))
                else

                let
                    val (srcTemp, dstTemp) = List.hd(movelist)
                    val srcNode = TG.getNode(igraph, srcTemp)
                    val dstNode = TG.getNode(igraph, dstTemp)
                in
                    if not (isNone(TT.look(initial, srcTemp))) andalso isNone(TT.look(initial, dstTemp)) andalso (TG.inDegree(srcNode) + TG.inDegree(dstNode) < 30)
                    then color{igraph=igraph,
                                    initial=TT.enter(initial, dstTemp, getSome(TT.look(initial, srcTemp))),
                                    spillCost=spillCost,
                                    registers=registers,
                                    movelist=List.drop(movelist, 1)}
                    else if (isNone(TT.look(initial, srcTemp))) andalso not (isNone(TT.look(initial, dstTemp))) andalso (TG.inDegree(srcNode) + TG.inDegree(dstNode) < 30)
                    then color{igraph=igraph,
                                    initial=TT.enter(initial, srcTemp, getSome(TT.look(initial, dstTemp))),
                                    spillCost=spillCost,
                                    registers=registers,
                                    movelist=List.drop(movelist, 1)}
                    else if (isNone(TT.look(initial, srcTemp))) andalso (isNone(TT.look(initial, dstTemp))) andalso (TG.inDegree(srcNode) + TG.inDegree(dstNode) < 30)
                    then
                        let 
                            val newcolor = getSome(findColorCoalesced(igraph, srcTemp, dstTemp, initial, registers))
                            val table2 = TT.enter(initial, srcTemp, newcolor)
                            val table3 = TT.enter(table2, dstTemp, newcolor)
                        in 
                            color{igraph=igraph,
                                    initial=table3,
                                    spillCost=spillCost,
                                    registers=registers,
                                    movelist=List.drop(movelist, 1)}
                        end
                    else color{igraph=igraph,
                                    initial=initial,
                                    spillCost=spillCost,
                                    registers=registers,
                                    movelist=List.drop(movelist, 1)}
                end
end
