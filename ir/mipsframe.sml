structure MipsFrame : FRAME = 
struct
    type frame = int (* TODO *)
    type access = int (* TODO *)
    fun newFrame {name, formals} = 0 (* TODO *)
    fun name frame' = Temp.newlabel() (* TODO *)
    fun formals frame' = [] (* TODO *)
    fun allocLocal frame' _ = 0 (* TODO *)
end