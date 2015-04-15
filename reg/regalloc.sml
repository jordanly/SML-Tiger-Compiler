signature REGALLOC = 
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Table.table
    val initialAlloc : allocation
    val regList : Frame.register list
    val performAllocation : string * allocation -> string
    (*val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation*)
end

structure RegAlloc : REGALLOC = 
struct
    structure Frame = MipsFrame
    structure TT = Temp.Table
    type allocation = Frame.register Temp.Table.table
    val initialAlloc = 
        let fun addToTable ((temp, assignedRegister), table) = TT.enter(table, temp, assignedRegister)
        in foldl addToTable TT.empty (Frame.specialregs @ Frame.argregs @ Frame.calleesaves @ Frame.callersaves)
        end
    val regList = 
        let fun addToList ((temp, assignedRegister), listSoFar) = assignedRegister::listSoFar
        in foldl addToList [] (Frame.specialregs @ Frame.argregs @ Frame.calleesaves @ Frame.callersaves)
        end
    fun performAllocation (code, allocation) =
        let
            fun getSomeInt (SOME x) = x | getSomeInt NONE = 0
            fun getSomeString (SOME x) = x | getSomeString NONE = "NO REG FOUND"
            fun findTemp(str, index) =
                if String.size(str) < (index + 4)   
                then NONE
                else
                
                if String.sub(str, index) = #"t" andalso Char.isDigit(String.sub (str, index+1))
                    andalso Char.isDigit(String.sub (str, index+2)) andalso Char.isDigit(String.sub (str, index+3))
                then SOME index
                else findTemp(str, index+1)
        in
            case findTemp(code, 0) of
                NONE => code
              | SOME tIndex =>
                    let
                        val preString = String.substring(code, 0, tIndex)
                        val temp : Temp.temp = getSomeInt(Int.fromString(String.substring(code, tIndex + 1, 3)))
                        val postString = String.substring(code, tIndex+4, String.size(code) - tIndex - 4)
                        val regString = getSomeString(TT.look(allocation, temp))
                    in
                        performAllocation(preString ^ regString ^ postString, allocation)
                    end
        end
end