signature FRAME = 
sig
    type register = string
    type frame
    type access
    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
            
    val R0 : Temp.temp
    val FP : Temp.temp
    val RV : Temp.temp
    val SP : Temp.temp
    val RA : Temp.temp
    val V1 : Temp.temp
    val specialregs : (Temp.temp * register) list
    val argregs : (Temp.temp * register) list
    val calleesaves : (Temp.temp * register) list
    val callersaves : (Temp.temp * register) list
    val tempMap : register Temp.Table.table
    val getRegisterTemps : (Temp.temp * register) list -> Temp.temp list
    val makestring : Temp.temp -> register
    val wordSize : int
    val exp : access * Tree.exp -> Tree.exp
    val newFrame : {name: Temp.label, formals: bool list} -> frame
    val name : frame -> Temp.label
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
    val printFrame : frame -> unit
    val printAccess : access -> unit
    val string : (Temp.label * string) -> string

    val externalCall : string * Tree.exp list -> Tree.exp
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
    val procEntryExit3 : frame * Assem.instr list * int -> {prolog: string, body: Assem.instr list, epilog: string}
    val procEntryExit4 : frame * Assem.instr list * Temp.temp list -> Assem.instr list
end
