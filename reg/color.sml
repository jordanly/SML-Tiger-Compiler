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

    fun getSimplifiableNode (igraph, initial, registers) = 
        if List.length(TG.nodes igraph) = 0
        then NONE
        else

        let
            fun isNone NONE = false | isNone (SOME _) = true
            val numregisters = List.length registers
            val possibleNode = List.hd(TG.nodes(igraph))
            val possNodeID = TG.getNodeID(possibleNode)
            val degree = TG.degree(possibleNode)
        in
            if degree < numregisters andalso isNone(TT.look(initial, possNodeID))
            then SOME possibleNode
            else getSimplifiableNode(TG.removeNode(igraph, possNodeID), initial, registers)
        end

    fun findColor(igraph, node, initial, []) = NONE
      | findColor(igraph, node, initial, possibleColor::rest) = 
            let
                fun foldHelper (adjnode, boolean) : bool = boolean orelse
                    case TT.look(initial, TG.getNodeID(node)) of
                        SOME adjColor => (possibleColor = adjColor)
                      | NONE => false
                val colorIsBad : bool = foldl foldHelper false (TG.adj node)
            in
                if colorIsBad
                then findColor(igraph, node, initial, rest)
                else SOME possibleColor
            end

    fun color {igraph, initial, spillCost, registers} =
        case getSimplifiableNode (igraph, initial, registers) of
            SOME node => 
                (case findColor(igraph, node, initial, registers) of
                    SOME foundcolor => color{igraph=igraph,
                                    initial=TT.enter(initial, TG.getNodeID node, foundcolor),
                                    spillCost=spillCost,
                                    registers=registers}
                  | NONE => (print "bad"; (initial, []))
                )
          | NONE => (initial, []) (* Can't simplify, TODO *)
end