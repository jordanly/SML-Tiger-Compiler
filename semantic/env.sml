structure Env :> ENV = 
struct
    type access = unit
    type ty = T.ty

    datatype enventry = VarEntry of {ty: ty}
                      | FunEntry of {formals: ty list, result : ty}

    val base_tenv = (* predefined types *)
        let
            val withint = S.enter(S.empty, S.symbol "int", T.INT)
            val withstring = S.enter(withint, S.symbol "string", T.STRING)
        in
            withstring
        end
    val base_venv = Symbol.empty (* predefined functions *)
end