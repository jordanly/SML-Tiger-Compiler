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

        fun munchStm(T.SEQ(s1, s2)) = (munchStm s1; munchStm s2) (* TODO *)
          | munchStm(T.JUMP(T.NAME(lab), l)) =
              emit(
                A.OPER {assem="j " ^ Symbol.name lab ^ "\n",
                        src=[], dst=[], jump=SOME(l)}
              )
          | munchStm(T.MOVE(T.MEMLOC(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
              emit(
                A.MOVE {assem="sw `s0, " ^ Int.toString i ^ "(`d0)\n",
                        src=munchExp e2, dst=munchExp e1}
              )
          | munchStm(T.MOVE(T.MEMLOC(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
              emit(
                A.MOVE {assem="sw `s0, " ^ Int.toString i ^ "(`d0)\n",
                        src=munchExp e2, dst=munchExp e1}
              )
          | munchStm(T.MOVE(T.MEMLOC(e1), e2)) =
              emit(
                A.MOVE {assem="sw `s0, (`d0)\n",
                        src=munchExp e2, dst=munchExp e1}
              )
          | munchStm(T.MOVE(T.TEMPLOC(t1), e1)) = 
              emit(
                A.MOVE {assem="move `d0, `s0\n",
                        src=munchExp e1, dst=t1}
              )
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
          | munchStm(T.EXP e1) = (munchExp e1; ())
          | munchStm(T.LABEL lab) = 
              emit(
                A.LABEL {assem=Symbol.name lab ^ ":\n", lab=lab}
              )
            (* Verify "fall through = false label" assumption for all CJUMPs*)
          | munchStm(T.CJUMP(T.EQ, e1, e2, l1, l2)) =
                emit(
                    A.OPER {assem="beq `s0, `s1, " ^ Symbol.name l1 ^ "\n",
                            dst=[],
                            src=[munchExp e1, munchExp e2],
                            jump=SOME [l1, l2]}
                )
          | munchStm(T.CJUMP(T.NE, e1, e2, l1, l2)) =
                emit(
                    A.OPER {assem="bne `s0, `s1, " ^ Symbol.name l1 ^ "\n",
                            dst=[],
                            src=[munchExp e1, munchExp e2],
                            jump=SOME [l1, l2]}
                )
          | munchStm(T.CJUMP(T.LT, e1, e2, l1, l2)) =
                emit(
                    A.OPER {assem="blt `s0, `s1, " ^ Symbol.name l1 ^ "\n",
                            dst=[],
                            src=[munchExp e1, munchExp e2],
                            jump=SOME [l1, l2]}
                )
          | munchStm(T.CJUMP(T.GT, e1, e2, l1, l2)) =
                emit(
                    A.OPER {assem="bgt `s0, `s1, " ^ Symbol.name l1 ^ "\n",
                            dst=[],
                            src=[munchExp e1, munchExp e2],
                            jump=SOME [l1, l2]}
                )
          | munchStm(T.CJUMP(T.LE, e1, e2, l1, l2)) =
                emit(
                    A.OPER {assem="ble `s0, `s1, " ^ Symbol.name l1 ^ "\n",
                            dst=[],
                            src=[munchExp e1, munchExp e2],
                            jump=SOME [l1, l2]}
                )
          | munchStm(T.CJUMP(T.GE, e1, e2, l1, l2)) =
                emit(
                    A.OPER {assem="bge `s0, `s1, " ^ Symbol.name l1 ^ "\n",
                            dst=[],
                            src=[munchExp e1, munchExp e2],
                            jump=SOME [l1, l2]}
                )
          | munchStm(_) = ()
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
                     A.OPER {assem="addi `d0, `s0, -" ^ Int.toString i ^ "\n",
                             src=[munchExp e1], dst=[r], jump=NONE}
                             )
                    )
          | munchExp(T.BINOP(T.MINUS, T.CONST i, e1)) = Temp.newtemp() (* TODO *)
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
          | munchExp(T.TEMP(t1)) = t1
          | munchExp(T.ESEQ(s1, e1)) = (munchStm s1; munchExp e1)
          | munchExp(T.NAME(l1)) = (Err.error 0 "tried to munch T.NAME"; Temp.newtemp())
          | munchExp(_) = Temp.newtemp()
      in
        munchStm stm;
        rev(!ilist)
      end
end
