signature FINDESCAPE = 
sig
	val findEscape : Absyn.exp -> unit
end

structure FindEscape : FINDESCAPE = 
struct

  structure A = Absyn

	type depth = int
	type escEnv = (depth * bool ref) Symbol.table

	fun traverseVar (env:escEnv, d:depth, s:Absyn.var) : unit =
    case s of
       A.SubscriptVar (var, exp, _) => (traverseExp (env, d, exp);
                                        traverseVar (env, d, var))
    | A.FieldVar (var, _, _) => traverseVar (env, d, var)
    | A.SimpleVar (sym, _) =>
      case Symbol.look (env, sym) of
        SOME (depth', escape) => if depth' < d then
                                    escape := true
                                 else ()
      | NONE => ()

	and traverseExp(env:escEnv, d:depth, s:Absyn.exp): unit =
    let
      fun traverse exp = traverseExp (env, d, exp)
    in
      case s of
        A.VarExp var => traverseVar (env, d, var)
      | A.CallExp {func, args, pos} => app traverse args
      | A.OpExp {left, oper, right, pos} => (traverse left;
                                             traverse right)
      | A.RecordExp {fields, typ, pos} => app traverse (map #2 fields)
      | A.SeqExp (exps) => app traverse (map #1 exps)
      | A.AssignExp {var, exp, pos} => (traverseVar (env, d, var);
                                        traverse exp)
      | A.IfExp {test, then', else', pos} =>
        (traverse test;
         traverse then';
         case else' of
          SOME exp => traverse exp
        | NONE => ())
      | A.WhileExp {test, body, pos} => (traverse test;
                                         traverse body)
      | A.ForExp {var, escape, lo, hi, body, pos} =>
        (traverse lo;
         traverse hi;
         traverseExp (Symbol.enter (env, var, (d, escape)), d, body))
      | A.LetExp {decs, body, pos} => traverseExp (traverseDecs (env, d, decs), d, body)
      | A.ArrayExp {typ, size, init, pos} => (traverse size; traverse init)
      | _ => ()
    end

	and traverseDecs (env:escEnv, _, []): escEnv = env
    | traverseDecs (env:escEnv, depth, dec :: decs:Absyn.dec list):escEnv =
      let
        fun extendEnv ({name, escape, typ, pos}, env) =
          Symbol.enter (env, name, (depth + 1, escape))
        fun traverseFundec {name, params, result, body, pos} =
          traverseExp (foldr extendEnv env params, depth + 1, body)
      in
        case dec of
          A.FunctionDec fundecs => (app traverseFundec fundecs;
                                    env)
        | A.VarDec {name, escape, typ, init, pos} =>
            traverseDecs (Symbol.enter (env, name, (depth, escape)), depth, decs)
        | A.TypeDec _ => traverseDecs (env, depth, decs)
      end

	fun findEscape(prog: A.exp) = traverseExp (Symbol.empty, 0, prog)
end