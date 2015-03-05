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
      | getFieldType ([], id, pos) = (Err.error pos "no such field"; T.BOTTOM)

    fun checkTypesEqual (tyA, tyB, pos, errMsg) = if not (T.eq(tyA, tyB))
                                                  then Err.error pos errMsg
                                                  else ()

    (* Main recursive type-checking functions *)
    fun transExp (venv, tenv, exp) = 
        let fun
            trexp (A.VarExp(var)) = trvar var
          | trexp (A.NilExp) = {exp=(), ty=T.NIL}
          | trexp (A.IntExp(intvalue)) = {exp=(), ty=T.INT}
          | trexp (A.StringExp(stringvalue, pos)) = {exp=(), ty=T.STRING}
          | trexp (A.CallExp({func, args, pos})) = 
          let
            fun checkArgs (forTy::formalList, argExp::argList, pos) = if T.eq(forTy, #ty (trexp argExp))
                                                                      then checkArgs(formalList, argList, pos)
                                                                      else Err.error pos "mismatched args"
              | checkArgs ([], argExp::argList, pos) = Err.error pos "mismatched args"
              | checkArgs (forTy::formalList, [], pos) = Err.error pos "insufficient args"
              | checkArgs ([], [], pos) = ()
          in
            case S.look(venv, func) of
                SOME(Env.FunEntry({formals, result})) => (checkArgs(formals, args, pos); {exp=(), ty=result})
              | SOME(_) => (Err.error pos ("symbol not function " ^ S.name func); {exp=(), ty=T.BOTTOM})
              | NONE => (Err.error pos ("no such function " ^ S.name func); {exp=(), ty=T.BOTTOM})
          end
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
          | trexp (A.RecordExp({fields, typ, pos})) = 
                (case S.look(tenv, typ) of
                    SOME x => 
                        (case x of
                            T.RECORD(f, _) => 
                                let 
                                    val recFormal : (S.symbol * S.symbol) list = (f ())
                                    fun getFieldType (name: string, []) = T.BOTTOM
                                      | getFieldType (name: string, (sym, exp, pos)::l) =
                                            if String.compare (name, S.name sym) = EQUAL
                                            then #ty (trexp exp)
                                            else getFieldType(name, l)
                                    fun checkFormal (sym, ty) =
                                            if not (T.leq(getFieldType(S.name sym, fields), ty))
                                            then Err.error pos ("actual type doesn't match formal type: " ^ S.name sym)
                                            else ()
                                    fun iterator((fieldname, typeid), ()) = 
                                        case S.look(tenv, typeid) of
                                            SOME x => (checkFormal (fieldname, x); ())
                                          | NONE => (Err.error pos ("unknown type in record: " ^ S.name typ); ())
                                in
                                    if List.length(recFormal) <> List.length(fields)
                                    then (Err.error pos ("record list is wrong length: " ^ S.name typ); {exp=(), ty=x})
                                    else (foldr iterator () recFormal; {exp=(), ty=x})
                                end
                          | _ => (Err.error pos ("expected record type, not: " ^ S.name typ); {exp=(), ty=T.NIL})
                        )
                  | NONE => (Err.error pos ("invalid record type: " ^ S.name typ); {exp=(), ty=T.NIL})
                )
          | trexp (A.SeqExp(expList)) = 
                let
                  fun helper((seqExp, pos), {exp=_, ty=_}) = (trexp seqExp)
                  fun checkSequence sequence = foldl helper {exp=(), ty=T.UNIT} sequence
                in
                  checkSequence expList
                end
          | trexp (A.AssignExp({var, exp, pos})) = 
                (
                checkTypesEqual(#ty (trvar var), #ty (trexp exp), pos, "mismatched types in assignment");
                {exp=(), ty=T.UNIT}
                )
          | trexp (A.IfExp({test, then', else', pos})) = 
                (
                checkTypesEqual(#ty (trexp test), T.INT, pos, "test in if exp does not evaluate to an int");
                case else' of 
                      SOME(elseExp) => checkTypesEqual(#ty (trexp then'), #ty (trexp elseExp), pos, "mismatching types in if expression")
                    | NONE => checkTypesEqual(#ty (trexp then'), T.UNIT, pos, "then must be unit in if then expression");
                {exp=(), ty=(#ty (trexp then'))}
                )
          | trexp (A.WhileExp({test, body, pos})) = 
                (
                checkTypesEqual(#ty (trexp test), T.INT, pos, "test does not evaluate to an int");
                checkTypesEqual(#ty (trexp body), T.UNIT, pos, "while body must be no value");
                {exp=(), ty=T.UNIT}
                )
          | trexp (A.ForExp({var, escape, lo, hi, body, pos})) = {exp=(), ty=T.NIL} (* TODO *)
          | trexp (A.BreakExp(pos)) = {exp=(), ty=T.BOTTOM} (* TODO *)
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
                  | NONE => (Err.error pos ("undefined variable " ^ S.name id); {exp=(), ty=T.BOTTOM})
                )
          | trvar (A.FieldVar(v, id, pos)) = {exp=(), ty=T.INT} (* TODO *)
                 (*(case trvar v of
                    {exp=(), ty=T.RECORD(fieldList, unique)} => {exp=(), ty=getFieldType(fieldList, id, pos)}
                  | {exp=_, ty=_} => (Err.error pos ("requires record"); {exp=(), ty=T.BOTTOM})
                )*)
          | trvar (A.SubscriptVar(v, subExp, pos)) = 
                (case trvar v of
                    {exp=(), ty=T.ARRAY(arrTy, unique)} => (checkInt(trexp subExp, pos); {exp=(), ty=arrTy})
                  | {exp=_, ty=_} => (Err.error pos ("requires array"); {exp=(), ty=T.BOTTOM})
                )
        in
            trexp exp
        end
    and transDec(venv, tenv, decs) = 
        let fun
            trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos})) =
                (case typ of
                    SOME(symbol, pos) =>
                        (case S.look(tenv, symbol) of
                            SOME ty => if T.eq(#ty (transExp(venv, tenv, init)), ty)
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
                fun maketemptydec ({name, ty, pos}, tenv') = S.enter(tenv', name, T.BOTTOM)
                val temp_tenv = foldl maketemptydec tenv tydeclist
                fun foldtydec({name, ty, pos}, {venv, tenv}) = {venv=venv, tenv=S.enter(tenv, name, transTy(temp_tenv, ty))}
            in
                foldl foldtydec {venv=venv, tenv=tenv} tydeclist
            end
          | trdec(venv, tenv, A.FunctionDec(fundeclist)) =
                let 
                    fun transrt rt =
                        (case S.look(tenv, rt) of 
                            SOME(rt') => rt'
                          | NONE => (Err.error 0 ("Return type unrecognized: " ^ S.name rt); T.BOTTOM)
                        )
                    fun transparam {name, escape, typ, pos} = 
                        (case S.look(tenv, typ) of
                            SOME t => {name=name, ty=t}
                          | NONE => (Err.error 0 ("Parameter type unrecognized: " ^ S.name typ); {name=name, ty=T.BOTTOM})
                        )
                    fun enterFuncs ({name, params, body, pos, result=SOME(rt, pos')}, venv) = 
                            S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=transrt rt})
                      | enterFuncs ({name, params, body, pos, result=NONE}, venv) = 
                            S.enter(venv, name, Env.FunEntry{formals= map #ty (map transparam params), result=T.UNIT})
                    val venv' = foldr enterFuncs venv fundeclist
                    fun checkfundec({name, params, body, pos, result}) = 
                        let 
                            val result_ty = 
                                (case result of
                                    SOME(rt, pos') => transrt rt
                                  | NONE => T.UNIT
                                )
                            val params' = map transparam params
                            fun enterparam ({name, ty}, venv) = S.enter(venv, name, Env.VarEntry{ty=ty})
                            val venv'' = foldl enterparam venv' params'
                            val body' = transExp (venv'', tenv, body)
                        in
                            if not (T.eq((#ty body'), result_ty))
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
                    NONE => T.NAME (name, ref NONE) (* Use T.NAME (name, ref (SOME ty))) instead? *) (*TODO is this right *)
                  | SOME ty => ty
                )
          | trty(tenv, A.RecordTy (fields)) =
                let 
                    fun fieldProcess {name, escape, typ, pos} =
                        case S.look(tenv, typ) of
                            SOME _ => (name, typ)
                          | NONE => (Err.error pos ("undefined type in rec: " ^ S.name typ); (name, typ))    
                    fun listConcat(a, b) = fieldProcess(a)::b
                    fun recGen () = foldl listConcat [] fields
                in 
                    T.RECORD (recGen, ref ())
                end
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
