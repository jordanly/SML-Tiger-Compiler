structure Semant =
struct

    type venv = Env.enventry Symbol.table
    type tenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}

    (* Global helper functions *)
    fun checkInt ({exp=_, ty=T.INT}, pos) = ()
      | checkInt ({exp=_, ty=_ }, pos) = Err.error pos "Expected int"

    fun checkEqualityOp ({exp=_, ty=T.INT}, {exp=_, ty=T.INT}, pos) = ()
      | checkEqualityOp ({exp=_, ty=T.STRING}, {exp=_, ty=T.STRING}, pos) = ()
      | checkEqualityOp ({exp=_, ty=T.RECORD(_, _)}, {exp=_, ty=T.RECORD(_, _)}, pos) = ()
      | checkEqualityOp ({exp=_, ty=T.ARRAY(_, _)}, {exp=_, ty=T.ARRAY(_, _)}, pos) = ()
      | checkEqualityOp ({exp=_, ty=_}, {exp=_, ty=_}, pos) = Err.error pos "Expected both int, string, record, or array"

    fun checkComparisonOp ({exp=_, ty=T.INT}, {exp=_, ty=T.INT}, pos) = ()
      | checkComparisonOp ({exp=_, ty=T.STRING}, {exp=_, ty=T.STRING}, pos) = ()
      | checkComparisonOp ({exp=_, ty=_ }, {exp=_, ty=_ }, pos) = Err.error pos "Expected both string or int"

    fun getFieldType ((fSymbol, fTy)::l, id, pos) = if S.name fSymbol = S.name id
                                                    then fTy
                                                    else getFieldType(l, id, pos)
      | getFieldType ([], id, pos) = (Err.error pos "no such field"; T.INT)


    (* Main recursive type-checking functions *)
    fun transExp (venv, tenv, exp) = 
        let fun
            trexp (A.VarExp(var)) = trvar var
          | trexp (A.NilExp) = {exp=(), ty=T.NIL}
          | trexp (A.IntExp(intvalue)) = {exp=(), ty=T.INT}
          | trexp (A.StringExp(stringvalue, pos)) = {exp=(), ty=T.STRING}
          | trexp (A.CallExp({func, args, pos})) = 
                (case S.look(venv, func) of
                    SOME(Env.FunEntry({formals, result})) => (checkArgs(formals, args, pos); {exp=(), ty=result})
                  | SOME(_) => (Err.error pos ("symbol not function " ^ S.name func); {exp=(), ty=T.INT})
                  | NONE => (Err.error pos ("no such function " ^ S.name func); {exp=(), ty=T.INT})
                )
          | trexp (A.OpExp{left, oper, right, pos}) = 
                (case oper of
                    A.PlusOp => (checkInt(trexp left, pos);
                                 checkInt(trexp right, pos);
                                 {exp=(), ty=T.INT})
                  | A.MinusOp => (checkInt(trexp left, pos); 
                                  checkInt(trexp right, pos);
                                  {exp=(), ty=T.INT})
                  | A.TimesOp => (checkInt(trexp left, pos); 
                                  checkInt(trexp right, pos);
                                  {exp=(), ty=T.INT})
                  | A.DivideOp => (checkInt(trexp left, pos); 
                                   checkInt(trexp right, pos);
                                   {exp=(), ty=T.INT})
                  | A.EqOp => (checkEqualityOp(trexp left, trexp right, pos);
                               {exp=(), ty=T.INT})
                  | A.NeqOp => (checkEqualityOp(trexp left, trexp right, pos);
                                {exp=(), ty=T.INT})
                  | A.LtOp => (checkComparisonOp(trexp left, trexp right, pos);
                               {exp=(), ty=T.INT})
                  | A.LeOp => (checkComparisonOp(trexp left, trexp right, pos);
                               {exp=(), ty=T.INT})
                  | A.GtOp => (checkComparisonOp(trexp left, trexp right, pos);
                               {exp=(), ty=T.INT})
                  | A.GeOp => (checkComparisonOp(trexp left, trexp right, pos);
                               {exp=(), ty=T.INT})
                )
          | trexp (A.RecordExp({fields, typ, pos})) = {exp=(), ty=T.NIL} (* TODO *)
          | trexp (A.SeqExp(expList)) = 
                (case expList of
                    [] => {exp=(), ty=T.UNIT}
                  | [(exp, pos)] => trexp(exp)
                  | a::l => trexp(#1 (List.last l))
                )
          | trexp (A.AssignExp({var, exp, pos})) = {exp=(), ty=T.NIL} (* TODO *)
          | trexp (A.IfExp({test, then', else', pos})) = {exp=(), ty=T.NIL} (* TODO *)
          | trexp (A.WhileExp({test, body, pos})) = {exp=(), ty=T.NIL} (* TODO *)
          | trexp (A.ForExp({var, escape, lo, hi, body, pos})) = {exp=(), ty=T.NIL} (* TODO *)
          | trexp (A.BreakExp(pos)) = {exp=(), ty=T.NIL} (* TODO *)
          | trexp (A.LetExp({decs, body, pos})) = 
                let
                    val {venv=venv', tenv=tenv'} = transDec(venv, tenv, decs)
                in
                    transExp(venv', tenv', body)
                end
          | trexp (A.ArrayExp({typ, size, init, pos})) = {exp=(), ty=T.NIL} (* TODO *)
        and trvar (A.SimpleVar(id, pos)) = 
                (case S.look(venv, id) of
                    SOME(Env.VarEntry({ty})) => {exp=(), ty=ty}
                  | SOME(Env.FunEntry({formals, result})) => {exp=(), ty=result}
                  | NONE => (Err.error pos ("undefined variable " ^ S.name id); {exp=(), ty=T.INT})
                )
          | trvar (A.FieldVar(v, id, pos)) = 
                (case trvar v of
                    {exp=(), ty=T.RECORD(fieldList, unique)} => {exp=(), ty=getFieldType(fieldList, id, pos)}
                  | {exp=_, ty=_} => (Err.error pos ("requires record"); {exp=(), ty=T.INT})
                )
          | trvar (A.SubscriptVar(v, subExp, pos)) = 
                (case trvar v of
                    {exp=(), ty=T.ARRAY(arrTy, unique)} => (checkInt(trexp subExp, pos); {exp=(), ty=arrTy})
                  | {exp=_, ty=_} => (Err.error pos ("requires array"); {exp=(), ty=T.INT}) (* TODO add name to error? *)
                )
        and checkArgs (forTy::formalList, argExp::argList, pos) = if forTy = #ty (trexp argExp)
                                                                  then checkArgs(formalList, argList, pos)
                                                                  else Err.error pos "mismatched args"
          | checkArgs ([], argExp::argList, pos) = Err.error pos "mismatched args"
          | checkArgs (forTy::formalList, [], pos) = Err.error pos "insufficient args"
          | checkArgs ([], [], pos) = ()
        in
            trexp exp
        end
    and transDec(venv, tenv, decs) = 
        let fun
            trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos})) =
                (case typ of
                    SOME(symbol, pos) =>
                        (case S.look(tenv, symbol) of
                            SOME ty => if (#ty (transExp(venv, tenv, init)) = ty)
                                       then {venv=S.enter(venv, name, (Env.VarEntry{ty=ty})), tenv=tenv}
                                       else (Err.error pos "mismatched types in vardec";
                                            {venv=S.enter(venv, name, (Env.VarEntry{ty=ty})), tenv=tenv})
                          | NONE => (Err.error pos "type not recognized"; {venv=venv, tenv=tenv})
                        )
                  | NONE =>
                        let 
                            val {exp, ty} = transExp(venv, tenv, init)
                        in 
                            {venv=S.enter(venv, name, (Env.VarEntry{ty=ty})), tenv=tenv}
                        end
                )
          | trdec(venv, tenv, A.TypeDec(tydeclist)) =
            let
                fun foldtydec({name, ty, pos}, {venv, tenv}) = {venv=venv, tenv=S.enter(tenv, name, transTy(tenv, ty))}
            in
                foldl foldtydec {venv=venv, tenv=tenv} tydeclist
            end
          | trdec(venv, tenv, A.FunctionDec(fundeclist)) =
                let 
                    fun transrt rt =
                        (case S.look(tenv, rt) of 
                            SOME(rt') => rt'
                          | NONE => (Err.error 0 ("Return type unrecognized: " ^ S.name rt); T.NIL) (*Add position to error?*)
                        )
                    fun transparam {name, escape, typ, pos} = 
                        (case S.look(tenv, typ) of
                            SOME t => {name=name, ty=t}
                          | NONE => (Err.error 0 ("Parameter type unrecognized: " ^ S.name typ); {name=name, ty=T.NIL})
                        )
                    fun enterFuncs ({name, params, body, pos, result=SOME(rt, pos')}, venv) = 
                            S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=transrt rt})
                    val venv' = foldr enterFuncs venv fundeclist
                    fun checkfundec({name, params, body, pos, result=SOME(rt, pos')}) = 
                        let 
                            val result_ty = transrt rt
                            val params' = map transparam params
                            fun enterparam ({name, ty}, venv) = S.enter(venv, name, Env.VarEntry{ty=ty})
                            val venv'' = foldl enterparam venv' params'
                            val body' = transExp (venv'', tenv, body)
                        in
                            if #ty body' <> result_ty
                            then Err.error pos ("Function body type doesn't match return type in function " ^ S.name name)
                            else ()
                        end
                    fun foldfundec (fundec, ()) = checkfundec fundec
                in
                    (foldr foldfundec () fundeclist;
                    {venv=venv', tenv=tenv})
                end
            and folddec(dec, {venv, tenv}) = trdec(venv, tenv, dec)
        in
            foldl folddec {venv=venv, tenv=tenv} decs
        end
    and transTy(tenv, ty) =
        let fun
            trty(tenv, A.NameTy (name, _)) =
                (case S.look (tenv, name) of
                    NONE => T.NAME (name, ref NONE) (* Use T.NAME (name, ref (SOME ty))) instead? *)
                  | SOME ty => ty
                )
          | trty(tenv, A.RecordTy (fields)) =
                T.RECORD ((map (fn {name, escape=_, typ, pos=pos'} =>
                    (name, (transTy(tenv, A.NameTy (typ, pos'))))) fields), ref ())
          | trty(tenv, A.ArrayTy (sym, pos')) =
                T.ARRAY (transTy (tenv, A.NameTy (sym, pos')), ref ())
        in
            trty(tenv, ty)
        end

    fun transProg (my_exp : A.exp) = 
        (transExp (Env.base_venv, Env.base_tenv, my_exp); ())
end

structure Main = 
struct
    fun compile fileName = Semant.transProg (Parse.parse(fileName))
end
