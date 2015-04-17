structure A = Absyn
structure S = Symbol

structure Env :> ENV = 
struct
    type access = unit
    type ty = Types.ty

    datatype enventry = VarEntry of {access: Translate.access, 
                                     ty: ty, read_only: bool}
                      | FunEntry of {level: Translate.level,
                                     label: Temp.label,
                                     formals: ty list, result : ty}

    val base_tenv = (* predefined types *)
        let
            fun addtotable ((s, t), table) = S.enter(table, S.symbol s, t)
            val toadd = [("int", T.INT), ("string", T.STRING)]
        in
            foldr addtotable S.empty toadd
        end

    val base_venv = (* predefined functions *)
        let
            fun addtotable ((s, t), table) = S.enter(table, S.symbol s, t)
            val toadd = [
                            ("print", FunEntry ({level=Translate.outermost, label=Temp.namedlabel "tig_print", formals=[T.STRING], result=T.UNIT})),
                            ("flush", FunEntry ({level=Translate.outermost, label=Temp.namedlabel "tig_flush", formals=[], result=T.UNIT})),
                            ("getchar", FunEntry ({level=Translate.outermost, label=Temp.namedlabel "tig_getchar", formals=[], result=T.STRING})),
                            ("ord", FunEntry ({level=Translate.outermost, label=Temp.namedlabel "tig_ord", formals=[T.STRING], result=T.INT})),
                            ("chr", FunEntry ({level=Translate.outermost, label=Temp.namedlabel "tig_chr", formals=[T.INT], result=T.STRING})),
                            ("size", FunEntry ({level=Translate.outermost, label=Temp.namedlabel "tig_size", formals=[T.STRING], result=T.INT})),
                            ("substring", FunEntry ({level=Translate.outermost, label=Temp.namedlabel "tig_substring", formals=[T.STRING, T.INT, T.INT], result=T.STRING})),
                            ("concat", FunEntry ({level=Translate.outermost, label=Temp.namedlabel "tig_concat", formals=[T.STRING, T.STRING], result=T.STRING})),
                            ("not", FunEntry ({level=Translate.outermost, label=Temp.namedlabel "tig_not", formals=[T.INT], result=T.INT})),
                            ("exit", FunEntry ({level=Translate.outermost, label=Temp.namedlabel "tig_exit", formals=[T.INT], result=T.UNIT}))
                        ]
        in
            foldr addtotable S.empty toadd
        end
end
