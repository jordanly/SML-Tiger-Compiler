structure Tr = Tree
structure Err = ErrorMsg

structure MipsFrame : FRAME = 
struct
    datatype access = InFrame of int | InReg of Temp.temp
    type frame = {name: Temp.label, formals: access list,
                  numLocals: int ref, curOffset: int ref}
    val FP = Temp.newtemp()
    val wordSize = 4

    fun name {name=name, formals=_, numLocals=_, curOffset=_} = name
    fun formals {name=_, formals=formals, numLocals=_, curOffset=_} = formals
    
    (* Where to put instruction to copy stack pointer to frame pointer ? *)
    fun newFrame {name, formals} = 
        let
            (* TODO handle false escapes? *)
            fun allocFormals(offset, [], allocList) = allocList
              | allocFormals(offset, curFormal::l, allocList) = 
                  (
                  case curFormal of
                       true => (InFrame offset)::allocFormals(offset + wordSize, l, allocList)
                     | false => (InReg(Temp.newtemp()))::allocFormals(offset, l, allocList)
                  )
        in
            {name=name, formals=allocFormals(0, formals, []),
            numLocals=ref 0, curOffset=ref 0}
        end

    fun allocLocal frame' escape = 
        let
            fun incrementNumLocals {name=_, formals=_, numLocals=x, curOffset=_} = x := !x + 1
            fun incrementOffset {name=_, formals=_, numLocals=_, curOffset=x} = x := !x - wordSize
            fun getOffsetValue {name=_, formals=_, numLocals=_, curOffset=x} = !x
        in
            incrementNumLocals frame';
            case escape of
                true => (incrementOffset frame'; InFrame(getOffsetValue frame'))
              | false => InReg(Temp.newtemp())
        end

    fun printFrame {name=n, formals=f, numLocals=nl, curOffset=co} =
        print ("numlocals = " ^ Int.toString(!nl) ^ " curOffset = " ^ Int.toString(!co) ^ "\n")

    fun printAccess fraccess =
        case fraccess of
             InFrame offset => print ("inframe " ^ Int.toString(offset) ^ "\n")
           | _ => print "temp\n"

    fun exp (fraccess, frameaddr) = 
        case fraccess of
            InFrame offset => Tr.MEM(Tr.BINOP(Tr.PLUS, frameaddr, Tr.CONST offset))
          | InReg temp => Tr.TEMP(temp)

    fun externalCall (s, args) =
      Tr.CALL(Tr.NAME(Temp.namedlabel s), args)
      
end
