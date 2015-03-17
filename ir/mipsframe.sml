structure MipsFrame : FRAME = 
struct
    type frame = int (* TODO *)
    datatype access = InFrame of int | InReg of Temp.temp
    fun newFrame {name, formals} = 0 (* TODO *)
    fun name frame' = Temp.newlabel() (* TODO *)
    fun formals frame' = [] (* TODO *)
    fun allocLocal frame' _ = InFrame 0 (* TODO *)
end