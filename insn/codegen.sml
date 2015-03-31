signature CODEGEN = 
sig
  structure Frame : FRAME
  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct
  structure Frame = MipsFrame
  structure A = Assem
  structure T = Tree
  fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
      let
        val ilist = ref (nil: A.instr list)
        fun emit x = ilist := x:: !ilist
        fun result(gen) =
            let
              val t = Temp.newtemp()
            in
              gen t; t
            end

        fun relopToStr T.EQ = "beq"
          | relopToStr T.NE = "bne"
          | relopToStr T.LT = "blt"
          | relopToStr T.GT = "bgt"
          | relopToStr T.LE = "ble"
          | relopToStr T.GE = "bge"
          | relopToStr T.ULT = "bltu"
          | relopToStr T.UGT = "bgtu"
          | relopToStr T.ULE = "bleu"
          | relopToStr T.UGE = "bgeu"

        fun munchStm(T.SEQ(s1, s2)) = (munchStm s1; munchStm s2)
          | munchStm(T.JUMP(T.NAME(lab), l)) =
              emit(
                A.OPER {assem="j " ^ Symbol.name lab ^ "\n",
                        src=[], dst=[], jump=SOME(l)}
              )
          | munchStm(T.JUMP(e1, labelList)) =
              emit(
                A.OPER {assem="jr `j0\n",
                       src=[munchExp e1], dst=[], jump=SOME(labelList)}
              )
          (* MEMLOC moves *)
          | munchStm(T.MOVE(T.MEMLOC(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
              emit(
                A.OPER {assem="sw `s0, " ^ (Int.toString i) ^ "(`s1)\n",
                        src=[munchExp e2, munchExp e1], dst=[], jump=NONE}
              )
          | munchStm(T.MOVE(T.MEMLOC(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
              emit(
                A.OPER {assem="sw `s0, " ^ (Int.toString i) ^ "(`s1)\n",
                        src=[munchExp e2, munchExp e1], dst=[], jump=NONE}
              )
          | munchStm(T.MOVE(T.MEMLOC(T.BINOP(T.MINUS, e1, T.CONST i)), e2)) = 
              emit(
                A.OPER {assem="sw `s0, " ^ (Int.toString (~i)) ^ "(`s1)\n",
                        src=[munchExp e2, munchExp e1], dst=[], jump=NONE}
              )
          | munchStm(T.MOVE(T.MEMLOC(e1), e2)) =
              emit(
                A.OPER {assem="sw `s0, 0(`s1)\n",
                        src=[munchExp e2, munchExp e1], dst=[], jump=NONE}
              )
          (* TEMPLOC moves *)
          | munchStm(T.MOVE(T.TEMPLOC(t1), T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)))) = 
              emit(
                A.OPER {assem="lw `d0, " ^ (Int.toString i) ^ "(`s0)\n",
                        src=[munchExp e1], dst=[t1], jump=NONE}
              )
          | munchStm(T.MOVE(T.TEMPLOC(t1), T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)))) = 
              emit(
                A.OPER {assem="lw `d0, " ^ (Int.toString i) ^ "(`s0)\n",
                        src=[munchExp e1], dst=[t1], jump=NONE}
              )
          | munchStm(T.MOVE(T.TEMPLOC(t1), T.MEM(T.BINOP(T.MINUS, e1, T.CONST i)))) = 
              emit(
                A.OPER {assem="lw `d0, " ^ (Int.toString (~i)) ^ "(`s0)\n",
                        src=[munchExp e1], dst=[t1], jump=NONE}
              )
          | munchStm(T.MOVE(T.TEMPLOC(t1), T.NAME(lab))) = 
              emit(
                A.OPER {assem="la `d0, " ^ (Symbol.name lab) ^ "\n",
                        src=[], dst=[t1], jump=NONE}
              )
          | munchStm(T.MOVE(T.TEMPLOC(t1), T.CONST i)) = 
              emit(
                A.OPER {assem="li `d0, " ^ (Int.toString i) ^ "\n",
                        src=[], dst=[t1], jump=NONE}
              )
          | munchStm(T.MOVE(T.TEMPLOC(t1), e1)) = 
              emit(
                A.MOVE {assem="move `d0, `s0\n",
                        src=munchExp e1, dst=t1}
              )
          (* ESEQLOC moves *)
          | munchStm(T.MOVE(T.ESEQLOC(s1, T.MEMLOC(e1)), e2)) = 
              (munchStm s1; emit(
                A.MOVE {assem="sw `s0, (`d0)\n",
                        src=munchExp e2, dst=munchExp e1}
              ))
          | munchStm(T.MOVE(T.ESEQLOC(s1, T.TEMPLOC(t1)), e1)) = 
              (munchStm s1; emit(
                A.MOVE {assem="move `d0, `s0\n",
                        src=munchExp e1, dst=t1}
              ))
          | munchStm(T.MOVE(T.ESEQLOC(s1, e1loc as T.ESEQLOC _), e1)) = 
              (munchStm s1; munchStm(T.MOVE(e1loc, e1)))
          | munchStm(T.EXP e1) = (munchExp e1; ())
          | munchStm(T.LABEL lab) = 
              emit(
                A.LABEL {assem=Symbol.name lab ^ ":\n", lab=lab}
              )
          (* CJUMPs *)
          | munchStm(T.CJUMP(relop, e1, e2, l1, l2)) =
                emit(
                    A.OPER {assem=(relopToStr relop) ^ " `s0, `s1, " ^ Symbol.name l1 ^ "\n",
                            dst=[],
                            src=[munchExp e1, munchExp e2],
                            jump=SOME [l1, l2]}
                )
        and munchExp(T.CONST i) =
              result(fn r => emit(
                     A.OPER {assem="addi `d0, r0, " ^ Int.toString i ^ "\n",
                             src=[], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) =
              result(fn r => emit(
                     A.OPER {assem="lw `d0, " ^ Int.toString i ^ "(`s0)" ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) =
              result(fn r => emit(
                     A.OPER {assem="lw `d0, " ^ Int.toString i ^ "(`s0)" ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.MEM(T.BINOP(T.MINUS, e1, T.CONST i))) =
              result(fn r => emit(
                     A.OPER {assem="lw `d0, " ^ (Int.toString (~i)) ^ "(`s0)" ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.MEM(e1)) =
              result(fn r => emit(
                     A.OPER {assem="lw `d0, 0(`s0)\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.PLUS, e1, T.CONST i)) =
              result(fn r => emit(
                     A.OPER {assem="addi `d0, `s0, " ^ Int.toString i ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.PLUS, T.CONST i, e1)) =
              result(fn r => emit(
                     A.OPER {assem="addi `d0, `s0, " ^ Int.toString i ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.PLUS, e1, e2)) =
              result(fn r => emit(
                     A.OPER {assem="add `d0, `s0, `s1\n",
                             src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.MINUS, e1, T.CONST i)) =
              result(fn r => emit(
                     A.OPER {assem="addi `d0, `s0, " ^ (Int.toString (~i)) ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.MINUS, e1, e2)) =
              result(fn r => emit(
                     A.OPER {assem="sub `d0, `s0, `s1\n",
                             src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.MUL, e1, e2)) =
              result(fn r => emit(
                     A.OPER {assem="mult `d0, `s0, `s1\n",
                             src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.DIV, e1, e2)) =
              result(fn r => emit(
                     A.OPER {assem="div `d0, `s0, `s1\n",
                             src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.AND, e1, T.CONST i)) =
              result(fn r => emit(
                     A.OPER {assem="andi `d0, `s0, " ^ Int.toString i ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.AND, T.CONST i, e1)) =
              result(fn r => emit(
                     A.OPER {assem="andi `d0, `s0, " ^ Int.toString i ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.AND, e1, e2)) =
              result(fn r => emit(
                     A.OPER {assem="and `d0, `s0, `s1\n",
                             src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.OR, e1, T.CONST i)) =
              result(fn r => emit(
                     A.OPER {assem="ori `d0, `s0, " ^ Int.toString i ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.OR, T.CONST i, e1)) =
              result(fn r => emit(
                     A.OPER {assem="ori `d0, `s0, " ^ Int.toString i ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.OR, e1, e2)) =
              result(fn r => emit(
                     A.OPER {assem="or `d0, `s0, `s1\n",
                             src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.LSHIFT, e1, T.CONST i)) =
              result(fn r => emit(
                     A.OPER {assem="sll `d0, `s0, " ^ (Int.toString i) ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.LSHIFT, e1, e2)) = (Err.error 0 "tried to create sll with non-constant shift amount"; Temp.newtemp())
          | munchExp(T.BINOP(T.RSHIFT, e1, T.CONST i)) =
              result(fn r => emit(
                     A.OPER {assem="srl `d0, `s0, " ^ (Int.toString i) ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.RSHIFT, e1, e2)) = (Err.error 0 "tried to create srl with non-constant shift amount"; Temp.newtemp())
          | munchExp(T.BINOP(T.ARSHIFT, e1, T.CONST i)) =
              result(fn r => emit(
                     A.OPER {assem="sra `d0, `s0, " ^ (Int.toString i) ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.ARSHIFT, e1, e2)) = (Err.error 0 "tried to create sra with non-constant shift amount"; Temp.newtemp())
          | munchExp(T.BINOP(T.XOR, e1, e2)) =
              result(fn r => emit(
                     A.OPER {assem="sll `d0, `s0, `s1\n",
                             src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.TEMP(t1)) = t1
          | munchExp(T.ESEQ(s1, e1)) = (munchStm s1; munchExp e1)
          | munchExp(T.NAME(l1)) = (Err.error 0 "tried to munch T.NAME"; Temp.newtemp())
          | munchExp(T.CALL(T.NAME(n1), args)) = 
              let
                val calldefs = F.RA::F.RV::(F.getRegisterTemps F.calleesaves)
              in
                result(fn r => emit(
                      A.OPER {assem="jal " ^ Symbol.name n1 ^ "\n",
                              src=munchArgs(0, args, 16), dst=calldefs,
                              jump=NONE}
                              )
                      )
              end
        and munchArgs(i, [], offset) = []
          | munchArgs(i, a::l, offset) =
              let
                val argTemp = if i < 4
                              then List.nth ((F.getRegisterTemps F.argregs), i)
                              else Temp.newtemp() (* not used *)
                fun moveArgToTemp arg = munchStm(T.MOVE(T.TEMPLOC(argTemp), arg))
                fun moveArgToFrame(arg, offset) =
                  munchStm(T.MOVE(T.MEMLOC(T.BINOP(T.PLUS, T.TEMP(F.SP), T.CONST offset)), arg))
              in
                if i < 4
                then (moveArgToTemp a; argTemp::munchArgs(i + 1, l, offset))
                else (moveArgToFrame(a, offset); munchArgs(i + 1, l, offset + 4))
              end
      in
        munchStm stm;
        rev(!ilist)
      end
end
