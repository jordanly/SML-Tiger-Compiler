structure Semant =
struct
    type venv = Env.enventry Symbol.table
    type tenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}

    fun checkInt ({exp=_, ty=T.INT }, pos) = ()
      | checkInt ({exp=_, ty=_ }, pos) = Err.error pos "integer required"

    fun transExp (venv, tenv, exp) = 
        let fun trexp (A.IntExp(intvalue)) = {exp=(), ty=T.INT}
              | trexp (A.OpExp{left, oper=A.PlusOp,right,pos}) = 
                      (checkInt(trexp left, pos);
                       checkInt(trexp right, pos);
                       {exp=(), ty=T.INT})
              | trexp (anythingelse) = {exp=(), ty=T.NIL} (*Place holder to ensure exhaustive match*)
        and trvar (A.SimpleVar(id,pos)) = 
                (case Symbol.look(venv, id) of
                    SOME(Env.VarEntry({ty=varty})) =>
                        {exp=(), ty=varty}
                  | SOME(Env.FunEntry({formals=formalsTyList, result=resultTy})) =>
                        {exp=(), ty=resultTy}
                  | NONE => (Err.error pos ("undefined variable " ^ S.name id);
                             {exp=(), ty=T.INT}))
          | trvar (anythingelse) = {exp=(), ty=T.INT} (*Place holder to ensure exhaustive match*)
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