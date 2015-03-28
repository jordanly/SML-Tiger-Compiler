structure Tr = Tree
structure Err = ErrorMsg

structure MipsFrame : FRAME = 
struct
    type register = string
    datatype access = InFrame of int | InReg of Temp.temp
    type frame = {name: Temp.label, formals: access list,
                  numLocals: int ref, curOffset: int ref}
    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
                           
    val FP = Temp.newtemp()
    val RV = Temp.newtemp() (* TODO? *)
    val registers = [] (* TODO *)
    val tempMap = 
        let
            fun addtotable ((t, s), table) = Temp.Table.enter(table, t, s)
            val toadd = [
                (FP, "FP"),
                (RV, "RV")
            ]
        in
            foldr addtotable Temp.Table.empty toadd
        end
    fun makestring t = (* replacement for temp.makestring *)
        case Temp.Table.look(tempMap, t) of
             SOME(r) => r
           | NONE => Temp.makestring t
    val wordSize = 4

    val ARGREGS = 4 (* registers allocated for arguments in mips *)

    fun name {name=name, formals=_, numLocals=_, curOffset=_} = name
    fun formals {name=_, formals=formals, numLocals=_, curOffset=_} = formals
    fun string(lab, s) = "TODO implement " ^ (Symbol.name lab) ^ " : " ^ s ^ "\n"
    
    fun newFrame {name, formals} = 
        let
            fun allocFormals(offset, [], allocList, numRegs) = allocList
              | allocFormals(offset, curFormal::l, allocList, numRegs) = 
                  (
                  case curFormal of
                       true => allocFormals(offset + wordSize, l, (InFrame offset)::allocList, numRegs)
                     | false => 
                         if numRegs < ARGREGS
                         then allocFormals(offset, l, (InReg(Temp.newtemp()))::allocList, numRegs + 1)
                         else allocFormals(offset + wordSize, l, (InFrame offset)::allocList, numRegs)
                  )
        in
            {name=name, formals=allocFormals(0, formals, [], 0),
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
        (
        print ("FRAME with name = " ^ (Symbol.name n) ^ "\n");
        print ("numlocals = " ^ Int.toString(!nl) ^ " curOffset = " ^ Int.toString(!co) ^ "\n")
        )

    fun printAccess fraccess =
        case fraccess of
             InFrame offset => print ("inframe " ^ Int.toString(offset) ^ "\n")
           | _ => print "temp\n"

    fun exp (fraccess, frameaddr) = 
        case fraccess of
            InFrame offset => Tr.MEM(Tr.BINOP(Tr.PLUS, frameaddr, Tr.CONST offset))
          | InReg temp => Tr.TEMP(temp)

    (* TODO account for Tiger vs. C distinctions *)
    fun externalCall (s, args) =
      Tr.CALL(Tr.NAME(Temp.namedlabel s), args)

    fun seq[] = Tr.EXP(Tr.CONST 0)
      | seq[stm] = stm
      | seq(stm::stms) = Tr.SEQ(stm,seq(stms))  

    fun procEntryExit1(frame' : frame, stm : Tr.stm) = 
        let
          val label' = name frame' 
        in
          seq [Tr.LABEL(label'), stm]
        end
      
end
