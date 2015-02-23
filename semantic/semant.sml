structure Semant =
struct
    type venv = Env.enventry Symbol.table
    type tenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}

    fun checkInt ({exp=_, ty=T.INT }, pos) = ()
      | checkInt ({exp=_, ty=_ }, pos) = Err.error pos "integer required"

    fun getFieldType ((fSymbol, fTy)::l, id, pos) = if S.name fSymbol = S.name id
                                                    then fTy
                                                    else getFieldType(l, id, pos)
      | getFieldType ([], id, pos) = (Err.error pos "no such field";
                                      T.INT)

    fun transExp (venv, tenv, exp) = 
        let fun trexp (A.VarExp(var)) = trvar var
              | trexp (A.NilExp) = {exp=(), ty=T.NIL}
              | trexp (A.IntExp(intvalue)) = {exp=(), ty=T.INT}
              | trexp (A.StringExp(stringvalue, pos)) = {exp=(), ty=T.STRING}
              | trexp (A.CallExp({func, args, pos})) = 
                    (case Symbol.look(venv, func) of
                          SOME(Env.FunEntry({formals, result})) => (checkArgs(formals, args, pos);
                                                                    {exp=(), ty=result})
                        | SOME(_) => (Err.error pos ("symbol not function " ^ S.name func);
                                      {exp=(), ty=T.INT})
                        | NONE => (Err.error pos ("no such function " ^ S.name func);
                                   {exp=(), ty=T.INT}))
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
                    | A.EqOp => {exp=(), ty=T.INT} (* TODO *)
                    | A.NeqOp => {exp=(), ty=T.INT} (* TODO *)
                    | A.LtOp => {exp=(), ty=T.INT} (* TODO *)
                    | A.LeOp => {exp=(), ty=T.INT} (* TODO *)
                    | A.GtOp => {exp=(), ty=T.INT} (* TODO *)
                    | A.GeOp => {exp=(), ty=T.INT} (* TODO *)
                    )
              | trexp (A.RecordExp({fields, typ, pos})) = {exp=(), ty=T.NIL} (* TODO *)
              | trexp (A.SeqExp(expList)) = 
                    (case expList of
                          [] => {exp=(), ty=T.UNIT}
                        | [(exp, pos)] => trexp(exp)
                        | a::l => trexp(#1 (List.last l)))
              | trexp (A.AssignExp({var, exp, pos})) = {exp=(), ty=T.NIL} (* TODO *)
              | trexp (A.IfExp({test, then', else', pos})) = {exp=(), ty=T.NIL} (* TODO *)
              | trexp (A.WhileExp({test, body, pos})) = {exp=(), ty=T.NIL} (* TODO *)
              | trexp (A.ForExp({var, escape, lo, hi, body, pos})) = {exp=(), ty=T.NIL} (* TODO *)
              | trexp (A.BreakExp(pos)) = {exp=(), ty=T.NIL} (* TODO *)
              | trexp (A.LetExp({decs, body, pos})) = 
                    let
                      val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
                    in
                      transExp(venv', tenv', body)
                    end
              | trexp (A.ArrayExp({typ, size, init, pos})) = {exp=(), ty=T.NIL} (* TODO *)
        and trvar (A.SimpleVar(id, pos)) = 
                (case Symbol.look(venv, id) of
                    SOME(Env.VarEntry({ty})) => {exp=(), ty=ty}
                  | SOME(Env.FunEntry({formals, result})) => {exp=(), ty=result}
                  | NONE => (Err.error pos ("undefined variable " ^ S.name id);
                             {exp=(), ty=T.INT}))
          | trvar (A.FieldVar(v, id, pos)) = 
                (case trvar v of
                      {exp=(), ty=T.RECORD(fieldList, unique)} => {exp=(), ty=getFieldType(fieldList, id, pos)}
                    | {exp=_, ty=_} => (Err.error pos ("requires record");
                                        {exp=(), ty=T.INT}))
          | trvar (A.SubscriptVar(v, subExp, pos)) = 
                (case trvar v of
                      {exp=(), ty=T.ARRAY(arrTy, unique)} => (checkInt(trexp subExp, pos);
                                                              {exp=(), ty=arrTy})
                    | {exp=_, ty=_} => (Err.error pos ("requires array"); (* TODO add name to error? *)
                                        {exp=(), ty=T.INT}))
        and checkArgs(forTy::formalList, argExp::argList, pos) = if forTy = #ty (trexp argExp)
                                                                 then checkArgs(formalList, argList, pos)
                                                                 else Err.error pos "mismatched args"
          | checkArgs([], argExp::argList, pos) = Err.error pos "mismatched args"
          | checkArgs(forTy::formalList, [], pos) = Err.error pos "insufficient args"
          | checkArgs([], [], pos) = ()
        in
            trexp exp
        end
    and transDecs(venv, tenv, decs) = 
        let fun trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos})) =
                    (case typ of
                          SOME(_) => {venv=venv, tenv=tenv}
                        | NONE => let val {exp, ty} = transExp(venv, tenv, init)
                                  in 
                                    {venv=S.enter(venv, name, (Env.VarEntry{ty=ty})),
                                     tenv=tenv}
                                  end
                    )
              | trdec(venv, tenv, A.TypeDec({name, ty, pos}::l)) =
                    let val tenv' = S.enter(tenv, name, transTy(tenv, ty))
                    in
                      trdec(venv, tenv', A.TypeDec(l))
                    end
              | trdec(venv, tenv, A.FunctionDec[{name, params, body, pos,
                                                result=SOME(rt, pos')}]) =
                    let val SOME(result_ty) = S.look(tenv, rt)
                        fun transparam {name, escape, typ, pos} = 
                          case S.look(tenv, typ)
                            of SOME t => {name=name, ty=t}
                        val params' = map transparam params
                        val venv' = S.enter(venv, name,
                                    Env.FunEntry{formals= map #ty params',
                                               result=result_ty})
                        fun enterparam ({name, ty}, venv) =
                                    S.enter(venv, name,
                                            Env.VarEntry{ty=ty})
                        val venv'' = foldl enterparam venv' params'
                      in transExp (venv'', tenv, body);
                        {venv=venv', tenv=tenv}
                      end

            and foldHelper(dec, {venv, tenv}) = trdec(venv, tenv, dec)
        in
          foldl foldHelper {venv=venv, tenv=tenv} decs
        end
      and transTy(tenv, ty) =
        let fun trty(tenv, A.NameTy (name, _)) = 
                    (case Symbol.look (tenv, name) of
                          NONE => T.NAME (name, ref NONE) (* Use T.NAME (name, ref (SOME ty))) instead? *)
                        | SOME ty => ty
                    )
              | trty(tenv, A.RecordTy (fields)) =
                    T.RECORD ((map (fn {name, escape=_, typ, pos=pos'} =>
                                       (name, (transTy(tenv, A.NameTy (typ, pos')))))
                                    fields),
                              ref ())
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
