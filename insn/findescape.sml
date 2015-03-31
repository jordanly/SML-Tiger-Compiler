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
        fun trvar(A.SimpleVar(sym,pos)) =
              (
                case S.look(env, sym) of
                     SOME(d', escape') => if d' < d
                                          then escape' := true
                                          else ()
                   | _ => ()
              )
          | trvar(A.FieldVar(var, sym, pos)) = trvar var
          | trvar(A.SubscriptVar(var, exp, pos)) =
              (
                trvar var;
                traverseExp(env, d, exp)
              )
      in
        trvar s
      end
  and traverseExp(env:escEnv, d:depth, s:Absyn.exp) : unit =
      let
        fun trexp(A.VarExp(var)) = traverseVar(env, d, var)
          | trexp(A.NilExp) = ()
          | trexp(A.IntExp(_)) = ()
          | trexp(A.StringExp(_)) = ()
          | trexp(A.CallExp {func, args, pos}) = app trexp args
          | trexp(A.OpExp {left, oper, right, pos}) = (trexp left; trexp right)
          | trexp(A.RecordExp {fields, typ, pos}) = 
              let
                fun evalField((sym, exp, pos)) = trexp exp
              in
                app evalField fields
              end
          | trexp(A.SeqExp expList) = 
              let
                fun evalExp((exp, pos)) = trexp exp
              in
                app evalExp expList
              end
          | trexp(A.AssignExp {var, exp, pos}) = 
              (
                traverseVar(env, d, var);
                trexp exp
              )
          | trexp(A.IfExp {test, then', else', pos}) =
              (
                trexp test;
                trexp then';
                case else' of
                     SOME(exp') => trexp exp'
                   | _ => ()
              )
          | trexp(A.WhileExp {test, body, pos}) = (trexp test; trexp body)
          | trexp(A.ForExp {var, escape, lo, hi, body, pos}) =
              (
                let
                  val env' = S.enter(env, var, (d, escape))
                in
                  traverseExp(env', d, lo);
                  traverseExp(env', d, hi);
                  traverseExp(env', d, body)
                end
              )
          | trexp(A.BreakExp pos) = ()
          | trexp(A.LetExp {decs, body, pos}) = 
              let
                val env' = traverseDecs(env, d, decs);
              in
                traverseExp(env', d, body)
              end
          | trexp(A.ArrayExp {typ, size, init, pos}) = (trexp size; trexp init)
      in
        trexp s
      end
  and traverseDecs(env:escEnv, d:depth, s:Absyn.dec list): escEnv = 
      let
        fun trdec(A.FunctionDec(fundecs), env) =
            let
              fun addParamToEnv({name=name', escape=escape', typ=_, pos=_}, env) =
                S.enter(env, name', (d + 1, escape'))
              fun evalFundec {name=_, params=params', result=_, body=body', pos=_} =
                  let
                    val env' = foldl addParamToEnv env params'
                  in
                    traverseExp(env', d + 1, body')
                  end
            in
              app evalFundec fundecs;
              env
            end
          | trdec(A.VarDec {name, escape, typ, init, pos}, env) =
              let
                val env' = S.enter(env, name, (d, escape))
              in
                traverseExp(env', d, init);
                env'
              end
          | trdec(A.TypeDec(typedecs), env) = env
        and foldDecs (dec, env') = trdec(dec, env')
      in
        foldl foldDecs env s
      end
  fun findEscape(prog: Absyn.exp): unit = traverseExp(S.empty, 0, prog)
end
