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
                           
    val R0 = Temp.newtemp() (* zero register *)
    val AT = Temp.newtemp() (* assembler temporary, reserved *)

    val RV = Temp.newtemp() (* return value *)
    val V1 = Temp.newtemp()

    val A0 = Temp.newtemp() (* args *)
    val A1 = Temp.newtemp()
    val A2 = Temp.newtemp()
    val A3 = Temp.newtemp()

    val T0 = Temp.newtemp()
    val T1 = Temp.newtemp()
    val T2 = Temp.newtemp()
    val T3 = Temp.newtemp()
    val T4 = Temp.newtemp()
    val T5 = Temp.newtemp()
    val T6 = Temp.newtemp()
    val T7 = Temp.newtemp()

    val S0 = Temp.newtemp()
    val S1 = Temp.newtemp()
    val S2 = Temp.newtemp()
    val S3 = Temp.newtemp()
    val S4 = Temp.newtemp()
    val S5 = Temp.newtemp()
    val S6 = Temp.newtemp()
    val S7 = Temp.newtemp()

    val T8 = Temp.newtemp()
    val T9 = Temp.newtemp()

    val K0 = Temp.newtemp() (* reserved for kernel *)
    val K1 = Temp.newtemp()

    val GP = Temp.newtemp()
    val SP = Temp.newtemp()
    val FP = Temp.newtemp() (* frame pointer *)
    val RA = Temp.newtemp() (* return address *)

    val specialregs = [
        (R0, "R0"),
        (AT, "AT"), 
        (RV, "RV"),
        (V1, "V1"),
        (K0, "K0"),
        (K1, "K1"),
        (GP, "GP"),
        (SP, "SP"),
        (FP, "FP"),
        (RA, "RA")
    ]
    val argregs = [
        (A0, "A0"),
        (A1, "A1"),
        (A2, "A2"),
        (A3, "A3")
    ]
    val calleesaves = [
        (S0, "S0"),
        (S1, "S1"),
        (S2, "S2"),
        (S3, "S3"),
        (S4, "S4"),
        (S5, "S5"),
        (S6, "S6"),
        (S7, "S7")
    ]
    val callersaves = [
        (T0, "T0"),
        (T1, "T1"),
        (T2, "T2"),
        (T3, "T3"),
        (T4, "T4"),
        (T5, "T5"),
        (T6, "T6"),
        (T7, "T7"),
        (T8, "T8"),
        (T9, "T9")
    ]

    val tempMap = 
        let
            fun addtotable ((t, s), table) = Temp.Table.enter(table, t, s)
            val toadd = specialregs @ argregs @ calleesaves @ callersaves
        in
            foldr addtotable Temp.Table.empty toadd
        end
    fun makestring t = (* replacement for temp.makestring *)
        case Temp.Table.look(tempMap, t) of
             SOME(r) => r
           | NONE => Temp.makestring t
    val wordSize = 4

    fun name {name=name, formals=_, numLocals=_, curOffset=_} = name
    fun formals {name=_, formals=formals, numLocals=_, curOffset=_} = formals
    fun string(lab, s) = (Symbol.name lab) ^ " : .ascii \"" ^ s ^ "\"\n"
    
    val ARGREGS = 4 (* registers allocated for arguments in mips *)
    fun newFrame {name, formals} = 
        let
            fun allocFormals(offset, [], allocList, numRegs) = allocList
              | allocFormals(offset, curFormal::l, allocList, numRegs) = 
                  (
                  case curFormal of
                       true => (InFrame offset)::allocFormals(offset + wordSize, l, allocList, numRegs)
                     | false => 
                         if numRegs < ARGREGS
                         then (InReg(Temp.newtemp()))::allocFormals(offset + wordSize, l, allocList, numRegs + 1)
                         else (InFrame offset)::allocFormals(offset + wordSize, l, allocList, numRegs)
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

    fun exp2loc (Tr.MEM exp') = Tr.MEMLOC exp'
      | exp2loc (Tr.TEMP temp') = Tr.TEMPLOC temp'
      | exp2loc (Tr.ESEQ (stm', exp' as Tr.MEM(_))) = Tr.ESEQLOC(stm', exp2loc exp')
      | exp2loc (Tr.ESEQ (stm', exp' as Tr.TEMP(_))) = Tr.ESEQLOC(stm', exp2loc exp')
      | exp2loc _ = (Err.error 0 "Can't convert exp to loc"; Tr.TEMPLOC(Temp.newtemp()))

    (* TODO account for Tiger vs. C distinctions *)
    fun externalCall (s, args) =
      Tr.CALL(Tr.NAME(Temp.namedlabel s), args)

    fun seq[] = Tr.EXP(Tr.CONST 0)
      | seq[stm] = stm
      | seq(stm::stms) = Tr.SEQ(stm,seq(stms))  
    
    fun getRegisterTemps rList = map (fn (t, r) => t) rList

    fun procEntryExit1(frame' : frame, stm : Tr.stm) = 
        let
          val label' = name frame'
          val argTemps = getRegisterTemps argregs
          fun moveArgs([], seqList, offset) = seqList
            | moveArgs(a::access, seqList, offset) =
                (
                if offset < 4
                then Tr.MOVE(exp2loc (exp(a, Tr.TEMP FP)), Tr.TEMP (List.nth(argTemps, offset)))::moveArgs(access, seqList, offset + 1)
                else Tr.MOVE(exp2loc (exp(a, Tr.TEMP FP)), Tr.CONST 0)::moveArgs(access, seqList, offset + 1)
                )
          val moveStms = moveArgs(formals frame', [], 0)
        in
          seq ([Tr.LABEL(label')] @ moveStms @ [stm])
        end

    fun procEntryExit2(frame, body) = 
        body @
        [Assem.OPER {assem="",
                 src=getRegisterTemps (specialregs @ calleesaves),
                 dst=[], jump=SOME[]}
        ]
      
    fun procEntryExit3(frame' : frame, body) =
        {prolog = "PROCEDURE " ^ Symbol.name (name frame') ^ "\n",
         body = body,
         epilog = "END " ^ Symbol.name (name frame') ^ "\n"}
end
