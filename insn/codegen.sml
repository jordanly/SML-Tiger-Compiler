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
          | munchStm(T.EXP e1) = (munchExp e1; ())
          | munchStm(T.LABEL lab) = 
              emit(
                A.LABEL {assem=Symbol.name lab ^ ":\n", lab=lab}
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
          | munchExp(_) = Temp.newtemp()
      in
        munchStm stm;
        rev(!ilist)
      end
end
