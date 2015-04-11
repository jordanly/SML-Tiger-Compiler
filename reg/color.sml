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
    type allocation = Frame.register Temp.Table.table
    fun color {igraph, initial, spillCost, registers} = (initial, [])
end