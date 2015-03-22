signature FINDESCAPE = 
sig
	val findEscape : Absyn.exp -> unit
end

structure FindEscape : FINDESCAPE = 
struct
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table
 
  fun traverseVar(env:escEnv, d:depth, s:Absyn.var) : unit =
      let
        fun trvar(A.SimpleVar(sym,pos)) = () (* TODO *)
          | trvar(A.FieldVar(var, sym,pos)) = () (* TODO *)
          | trvar(A.SubscriptVar(var, exp, pos)) = () (* TODO *)
      in
        trvar s
      end
  and traverseExp(env:escEnv, d:depth, s:Absyn.exp) : unit =
      let
        fun trexp(A.VarExp(var)) = () (* TODO *)
          | trexp(A.NilExp) = ()
          | trexp(A.IntExp(_)) = ()
          | trexp(A.StringExp(_)) = ()
          | trexp(A.CallExp(_)) = ()
          | trexp(A.OpExp {left, oper, right, pos}) = (trexp left; trexp right)
          | trexp(A.RecordExp {fields, typ, pos}) = () (* TODO *)
          | trexp(A.SeqExp expList) = () (* TODO *)
          | trexp(A.AssignExp {var, exp, pos}) = () (* TODO *)
          | trexp(A.IfExp {test, then', else', pos}) = () (* TODO *)
          | trexp(A.WhileExp {test, body, pos}) = () (* TODO *)
          | trexp(A.ForExp {var, escape, lo, hi, body, pos}) = () (* TODO *)
          | trexp(A.BreakExp pos) = () (* TODO *)
          | trexp(A.LetExp {decs, body, pos}) = () (* TODO *)
          | trexp(A.ArrayExp {typ, size, init, pos}) = () (* TODO *)
      in
        trexp s
      end (* TODO *)
  and traverseDecs(env:escEnv, d:depth, s:Absyn.dec list): escEnv = 
      let
        fun trdec(A.FunctionDec(fundec)) = S.empty (* TODO *)
          | trdec(A.VarDec {name, escape, typ, init, pos}) = S.empty (* TODO *)
          | trdec(A.TypeDec(typedec)) = S.empty (* TODO *)
        and foldDecs (dec, env') = trdec dec
      in
        foldl foldDecs env s
      end
  fun findEscape(prog: Absyn.exp): unit = () (* TODO *)
end
