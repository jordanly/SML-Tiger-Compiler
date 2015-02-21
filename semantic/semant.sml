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
              | trexp (A.CallExp({func, args, pos})) = {exp=(), ty=T.NIL} (* TODO *)
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
                    (* TODO do rest of Ops *)
                    )
              | trexp (anythingelse) = {exp=(), ty=T.NIL} (*Place holder to ensure exhaustive match*)
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
        in
            trexp exp
        end


    fun transProg (my_exp : A.exp) = 
        (transExp (Env.base_venv, Env.base_tenv, my_exp); ())
end

structure Main = 
struct
    fun compile fileName = Semant.transProg (Parse.parse(fileName))
end
