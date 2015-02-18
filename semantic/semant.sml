structure A = Absyn
structure Err = ErrorMsg
structure T = Types
structure S = Symbol

structure Semant =
struct
    (*type ty = T.ty
    type venv = Env.enventry Symbol.table
    type tenv = ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}*)

    (*fun transExp(venv, tevn, Absyn.OpExp{left, oper=Absyn.PlusOp, right, pos}) = 
        let val {exp=_, ty=tyleft} = transExp(venv, tenv, left)
            val {exp=_, ty=tyright} = transExp(venv, tenv, right)
        in case tyleft of Types.INT => ()
                        | _ => error pos "integer required";
           case tyright of Types.INT => ()
                        | _ => error pos "integer required";
           {exp=(), ty=Types.INT}
        end*)

    val transProg: A.exp -> unit = fn ast => (Err.linePos := [3]; Err.error 1 "hi")
end

structure Main = 
struct
    fun compile fileName = Semant.transProg (Parse.parse(fileName))
end