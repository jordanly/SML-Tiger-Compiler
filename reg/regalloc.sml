signature REGALLOC = 
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Table.table
    val initialAlloc : allocation
    val printAlloc : Frame.register Temp.Table.table * Temp.temp list -> unit
    val regList : Frame.register list
    val allocateRegisters : Liveness.igraphentry TempKeyGraph.graph * (Temp.temp * Temp.temp) list -> allocation * bool
end

structure RegAlloc : REGALLOC = 
struct
    structure Frame = MipsFrame
    structure TT = Temp.Table
    type allocation = Frame.register Temp.Table.table
    fun dummySpillCost x = 1;
    fun printAlloc(regTable : Frame.register Temp.Table.table, tempList : Temp.temp list) =
        let
          fun printTemp(temp) = 
            case Temp.Table.look(regTable, temp) of
                 SOME(x) => print(Temp.makestring(temp) ^ " -> " ^ x ^ "\n")
               | NONE => print("== Failed to allocate: " ^ Temp.makestring(temp) ^ "==\n")
        in
          app printTemp tempList
        end

    val initialAlloc = 
        let fun addToTable ((temp, assignedRegister), table) = TT.enter(table, temp, assignedRegister)
        in foldl addToTable TT.empty (Frame.specialregs @ Frame.argregs @ Frame.calleesaves @ Frame.callersaves)
        end
    val regList = 
        let fun addToList ((temp, assignedRegister), listSoFar) = assignedRegister::listSoFar
        in foldl addToList [] (Frame.specialregs @ Frame.argregs @ Frame.calleesaves @ Frame.callersaves)
        end
    fun allocateRegisters (igraph, movelist) =
        let
            val (newalloc, spilllist) = Color.color {igraph=igraph, initial=initialAlloc, spillCost=dummySpillCost, registers=regList, movelist=movelist}
        in
            (newalloc, List.length(spilllist) > 0)
        end
end
