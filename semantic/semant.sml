structure Semant =
struct
    type ty = T.ty
    type venv = Env.enventry Symbol.table
    type tenv = Env.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}

    fun checkInt ({exp=_, ty=T.INT }, pos) = ()
      | checkInt ({exp=_, ty=_ }, pos) = Err.error pos "integer required"

    fun transExp (venv, tenv, exp) = 
        let fun trexp (A.IntExp(intvalue)) = {exp=(), ty=T.INT}
              | trexp (A.OpExp{left, oper=A.PlusOp,right,pos}) = 
                      (checkInt(trexp left, pos);
                       checkInt(trexp right, pos);
                       {exp=(), ty=T.INT})
              | trexp (a) = {exp=(), ty=T.NIL} (*Place holder: delete me*)
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