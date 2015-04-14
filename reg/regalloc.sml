signature REGALLOC = 
sig
	structure Frame : FRAME
	type allocation = Frame.register Temp.Table.table
	val initialAlloc : allocation
	val regList : Frame.register list
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
end