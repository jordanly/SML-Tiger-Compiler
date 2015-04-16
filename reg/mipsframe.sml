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
        (R0, "$zero"),
        (AT, "$at"), 
        (RV, "$v0"),
        (V1, "$v1"),
        (K0, "$k0"),
        (K1, "$k1"),
        (GP, "$gp"),
        (SP, "$sp"),
        (FP, "$fp"),
        (RA, "$ra")
    ]
    val argregs = [
        (A0, "$a0"),
        (A1, "$a1"),
        (A2, "$a2"),
        (A3, "$a3")
    ]
    val calleesaves = [
        (S0, "$s0"),
        (S1, "$s1"),
        (S2, "$s2"),
        (S3, "$s3"),
        (S4, "$s4"),
        (S5, "$s5"),
        (S6, "$s6"),
        (S7, "$s7")
    ]
    val callersaves = [
        (T0, "$t0"),
        (T1, "$t1"),
        (T2, "$t2"),
        (T3, "$t3"),
        (T4, "$t4"),
        (T5, "$t5"),
        (T6, "$t6"),
        (T7, "$t7"),
        (T8, "$t8"),
        (T9, "$t9")
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
    fun string(lab, s) = (Symbol.name lab) ^ ": .ascii \"" ^ s ^ "\"\n"
    
    val ARGREGS = 4 (* registers allocated for arguments in mips *)
    fun newFrame {name, formals} = 
        let
            fun allocFormals(offset, [], allocList, index) = allocList
              | allocFormals(offset, curFormal::l, allocList, index) = 
                  (
                  case curFormal of
                       true => (InFrame offset)::allocFormals(offset + wordSize, l, allocList, index + 1)
                     | false => (InReg(Temp.newtemp()))::allocFormals(offset + wordSize, l, allocList, index + 1)
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
          val copySpToFp = Tr.MOVE(Tr.TEMPLOC(FP), Tr.TEMP(SP))

          (* move args === *)
          val argTemps = getRegisterTemps argregs
          fun moveArgs([], seqList, offset) = seqList
            | moveArgs(a::access, seqList, offset) =
                if offset < 4
                then Tr.MOVE(exp2loc (exp(a, Tr.TEMP FP)), Tr.TEMP (List.nth(argTemps, offset)))::moveArgs(access, seqList, offset + 1)
                else 
                    let
                      val temp = Temp.newtemp()
                    in
                      case a of 
                           InFrame off => moveArgs(access, seqList, offset + 1)
                             (* do nothing? already in correct place 
                              Tr.MOVE(Tr.TEMPLOC temp, (exp(a, Tr.TEMP FP)))::
                              Tr.MOVE(exp2loc (exp(a, Tr.TEMP FP)), Tr.TEMP temp)::
                              moveArgs(access, seqList, offset + 1) *)
                         | InReg te => 
                             (* load from frame into temp reg *)
                             Tr.MOVE(exp2loc (exp(a, Tr.TEMP FP)), Tr.TEMP te)::moveArgs(access, seqList, offset + 1)
                    end
          val moveArgStms = moveArgs(formals frame', [], 0)
          (* === *)

          (* move calleesaves to temps === *)
          val calleeSaveTemps = RA::(getRegisterTemps calleesaves) (* add RA *)
          val newCalleeSaveLocs = map (fn x => Temp.newtemp()) calleeSaveTemps

          (* callee list and list of new temps should be same size *)
          fun moveCalleeSaves(a::from, b::dest, newList) = (a, b)::newList
            | moveCalleeSaves([], l, newList) = newList
            | moveCalleeSaves(l, [], newList) = newList

          (* tuple list of callee save temps and their new locations *)
          val saveTempMap = moveCalleeSaves(calleeSaveTemps, newCalleeSaveLocs, [])

          fun moveTemp((from, to)) = Tr.MOVE(exp2loc(Tr.TEMP(to)), Tr.TEMP from)
          val tempMoveStms = map moveTemp saveTempMap
          (* === *)

          (* end if function move callee saves back *) 
          fun moveTempRev((to, from)) = Tr.MOVE(exp2loc(Tr.TEMP(to)), Tr.TEMP from)
          val tempMoveBackStms = map moveTempRev saveTempMap
        in
          seq ([Tr.LABEL(label')] 
               @ [copySpToFp]
               @ tempMoveStms
               @ moveArgStms
               @ [stm]
               @ tempMoveBackStms)
        end

    fun procEntryExit2(frame, body) = 
        body @
        [Assem.OPER {assem="",
                 src=getRegisterTemps (specialregs @ calleesaves),
                 dst=[], jump=SOME[]}
        ]
      
    fun procEntryExit3(frame' as {name=name', formals=formals', numLocals=numLocals', curOffset=curOffset'} : frame,
                       body, maxNumArgs) =
        let
            val spOffset = if maxNumArgs < ARGREGS
                           then !curOffset' - (ARGREGS * wordSize) 
                           else !curOffset' - (maxNumArgs * wordSize)
        in
            {prolog = "PROCEDURE " ^ Symbol.name (name frame') ^ "\n",
            body = body,
            epilog = "END " ^ Symbol.name (name frame') ^ "\n"}
        end
end
