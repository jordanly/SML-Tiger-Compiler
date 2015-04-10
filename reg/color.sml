signature COLOR = 
sig
    structure Frame : Frame
    type allocation = Frame.register Temp.Table.table
    val color: {igraph: igraphentry TempKeyGraph.graph, 
                initial: allocation,
                spillCost: igraphentry -> int,
                registers: Frame.register list}
                -> allocation * Temp.temp list
end

structure Color : COLOR = 
struct
    color {igraph, initial, spillCost, registers} = (initial, [])
end