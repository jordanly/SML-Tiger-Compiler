structure MipsFrame : FRAME = 
struct
    datatype access = InFrame of int | InReg of Temp.temp
    type frame = {name: Temp.label, formals: access list,
                  numLocals: int ref, curOffset: int ref}

    fun name {name=name, formals=_, numLocals=_, curOffset=_} = name
    fun formals {name=_, formals=formals, numLocals=_, curOffset=_} = formals
    
    (* Where to put instruction to copy stack pointer to frame pointer ? *)
    fun newFrame {name, formals} = 
    let
      (* Assume all escape right now *)
      (* TODO handle false escapes? *)
      fun allocFormals(offset, [], allocList) = allocList
        | allocFormals(offset, curFormal::l, allocList) = 
          (
          case curFormal of
               true => (InFrame offset)::allocFormals(offset + 4, l, allocList)
             | false => (InReg(Temp.newtemp()))::allocFormals(offset, l, allocList)
          )
    in
      {name=name, formals=allocFormals(0, formals, []),
       numLocals=ref 0, curOffset=ref 0}
    end

    fun allocLocal frame' escape = 
    let
      fun incrementNumLocals {name=_, formals=_, numLocals=x, curOffset=_} = x := !x + 1
      fun incrementOffset {name=_, formals=_, numLocals=_, curOffset=x} = x := !x - 4
      fun getOffsetValue {name=_, formals=_, numLocals=_, curOffset=x} = !x
    in
      incrementNumLocals frame';
      case escape of
           true => (incrementOffset frame'; InFrame(getOffsetValue frame'))
         | false => InReg(Temp.newtemp())
    end
end
