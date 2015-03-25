structure F = MipsFrame
structure T = Types

signature TRANSLATE = 
sig
    type exp
    type level
    type access
    
    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access
    val NIL : exp

    val exp2loc : Tr.exp -> Tr.loc

    val simpleVarIR : access * level -> exp
    val binopIR : Tr.binop * exp * exp -> exp
    val relopIR : Tr.relop * exp * exp -> exp
    val ifIR : exp * exp * exp -> exp
    val assignIR : exp * exp -> exp
    val whileIR : exp * exp * Temp.label -> exp
    val breakIR : Temp.label -> exp
    val forIR : exp * bool ref * exp * exp * exp * Temp.label -> exp
    val arrayIR : exp * exp -> exp
    val subscriptIR : exp * exp -> exp
    val recordIR : exp list -> exp
    val fieldIR : exp * int -> exp
    val sequencingIR : exp list -> exp
    val nilIR : unit -> exp
    val intIR : int -> exp
    val stringIR : string -> exp

    val procEntryExit : {level: level, body: exp} -> unit
    val getResult : unit -> F.frag list
    val resetFragList : unit -> unit
end

structure Translate =
struct
    datatype level =
        TOPLEVEL
      | NONTOP of {uniq: unit ref, parent: level, frame: F.frame}
    type access = level * F.access
    datatype exp = 
        Ex of Tree.exp
      | Nx of Tree.stm
      | Cx of Temp.label * Temp.label -> Tree.stm

    val fragList = ref [] : F.frag list ref
    val outermost = TOPLEVEL
    val NIL = Ex(Tr.CONST 0)

    fun seq[] = Tr.EXP(Tr.CONST 0)
      | seq[stm] = stm
      | seq(stm::stms) = Tr.SEQ(stm,seq(stms))  

    fun newLevel {parent, name, formals} = 
        let
            val formals'= true::formals
        in
            NONTOP({uniq = ref (), parent=parent, frame=F.newFrame {name=name, formals=formals'}})
        end

    fun formals TOPLEVEL = []
      | formals (curlevel as NONTOP{uniq, parent, frame}) = 
            let
                fun addLevel (faccess, l) = (curlevel, faccess)::l
            in
                foldl addLevel [] (F.formals frame)
            end

    fun printLevel level' =
      case level' of
           TOPLEVEL => print "Current level = TOPLEVEL\n"
         | NONTOP({uniq=uniq', parent=parent', frame=frame'}) => (print ("Level is NONTOP: " ^ Symbol.name (F.name frame') ^ "\n"); F.printFrame frame')
    
    fun printAccess (level, fAccess) = F.printAccess fAccess

    fun allocLocal level' escape' = 
      case level' of
           NONTOP({uniq=uniq', parent=parent', frame=frame'}) => (NONTOP({uniq=uniq', parent=parent', frame=frame'}), F.allocLocal frame' escape')
         | TOPLEVEL => (Err.error 0 "impossible"; (outermost, F.allocLocal (F.newFrame {name=Temp.newlabel(), formals=[]}) escape'))

    fun unEx (Ex e) = e 
      | unEx (Cx genstm) = 
            let 
                val r = Temp.newtemp() 
                val t = Temp.newlabel() and f = Temp.newlabel() 
            in
                Tr.ESEQ(seq[Tr.MOVE(Tr.TEMPLOC r, Tr.CONST 1), 
                            genstm(t,f), 
                            Tr.LABEL f, 
                            Tr.MOVE(Tr.TEMPLOC r, Tr.CONST 0), 
                            Tr.LABEL t], 
                        Tr.TEMP r) 
            end 
      | unEx (Nx (s as Tr.EXP(e))) = e
      | unEx (Nx s) = Tr.ESEQ(s, Tr.CONST 37)

    fun unCx (Cx c) = c
      | unCx (Ex (Tr.CONST 0)) = (fn(tlabel, flabel) => Tr.JUMP(Tr.NAME(flabel), [flabel]))
      | unCx (Ex (Tr.CONST 1)) = (fn(tlabel, flabel) => Tr.JUMP(Tr.NAME(tlabel), [tlabel]))
      | unCx (Ex e) = (fn(tlabel, flabel) => Tr.CJUMP(Tr.EQ, Tr.CONST 1, e, tlabel, flabel))
      | unCx (Nx _) = (ErrorMsg.error 0 "Compiler error: unCx an Nx"; fn (a, b) => Tree.LABEL(Temp.newlabel()))

    fun unNx (Ex e) = Tr.EXP(e)
      | unNx (Nx n) = n
      | unNx (c) = unNx(Ex(unEx(c)))

    fun followSLs TOPLEVEL TOPLEVEL bestguess = (Err.error 0 "Following static links failed"; bestguess)
              | followSLs TOPLEVEL _ bestguess = (Err.error 0 "Following static links failed"; bestguess)
              | followSLs _ TOPLEVEL bestguess = (Err.error 0 "Following static links failed"; bestguess)
              | followSLs (declevel as NONTOP{uniq=uniqdec, parent=_, frame=_}) (uselevel as NONTOP{uniq=uniquse, parent=useparent, frame=_}) bestguess =
                    if uniqdec = uniquse
                    then bestguess
                    else followSLs declevel useparent (Tr.MEM bestguess)

    fun simpleVarIR ((declevel, fraccess), uselevel) =
        Ex(F.exp (fraccess, followSLs declevel uselevel (Tr.TEMP F.FP)))

    fun binopIR (binop, left, right) = Ex(Tr.BINOP(binop, unEx(left), unEx(right)))

    (* ty mostly for comparing if STRING *)
    fun relopIR (relop, left, right, ty) = 
        case (ty,relop) of
            (T.STRING, Tr.EQ) => Ex(F.externalCall("stringEqual", [unEx left, unEx right]))
          | (T.STRING, Tr.LE) => Ex(F.externalCall("stringLE", [unEx left, unEx right]))
          | (T.STRING, Tr.LT) => Ex(F.externalCall("stringLT", [unEx left, unEx right]))
          | (T.STRING, Tr.GE) => Ex(F.externalCall("stringGE", [unEx left, unEx right]))
          | (T.STRING, Tr.GT) => Ex(F.externalCall("stringGT", [unEx left, unEx right]))
          | _        => Cx(fn (t, f) => Tr.CJUMP(relop, unEx(left), unEx(right), t, f))

    fun ifIR (test, then', else') =
        let
            val genstm = unCx(test)
            val e2 = unEx(then')
            val e3 = unEx(else')
            val resulttemp = Temp.newtemp()
            val t = Temp.newlabel()
            val f = Temp.newlabel()
            val join = Temp.newlabel()
        in
            Ex(Tr.ESEQ(seq[
                genstm(t, f),
                Tr.LABEL(t), Tr.MOVE(Tr.TEMPLOC(resulttemp), e2), Tr.JUMP(Tr.NAME(join), [join]),
                Tr.LABEL(f), Tr.MOVE(Tr.TEMPLOC(resulttemp), e3), Tr.JUMP(Tr.NAME(join), [join])
            ], Tr.TEMP(resulttemp)))
        end

    fun exp2loc (Tr.MEM exp') = Tr.MEMLOC exp'
      | exp2loc (Tr.TEMP temp') = Tr.TEMPLOC temp'
      | exp2loc Tr.TODO = (Err.error 0 "todo"; Tr.TEMPLOC(Temp.newtemp()))
      | exp2loc (Tr.ESEQ (stm', exp' as Tr.MEM(_))) = Tr.ESEQLOC(stm', exp')
      | exp2loc (Tr.ESEQ (stm', exp' as Tr.TEMP(_))) = Tr.ESEQLOC(stm', exp')
      | exp2loc _ = (Err.error 0 "Can't convert exp to loc"; Tr.TEMPLOC(Temp.newtemp()))

    fun assignIR (left, right) = Nx (Tr.MOVE (exp2loc (unEx left), unEx right))

    fun whileIR (test, body, breaklabel) =
        let
            val testlabel = Temp.newlabel()
            val bodylabel = Temp.newlabel()
            val test = unCx test
            val body = unNx body
        in
            Nx(seq[Tr.LABEL testlabel,
                      test (bodylabel, breaklabel),
                      Tr.LABEL(bodylabel),
                      body,
                      Tr.JUMP (Tr.NAME testlabel, [testlabel]),
                      Tr.LABEL breaklabel])
        end

    fun breakIR breaklabel = Nx(Tr.JUMP (Tr.NAME breaklabel, [breaklabel]))

    fun forIR (varEx, escape, loEx, hiEx, bodyNx, breaklabel) = 
        let
            val var = unEx(varEx)
            val lo = unEx(loEx)
            val hi = unEx(hiEx)
            val body = unNx(bodyNx)
            val bodylabel = Temp.newlabel()
            val updatelabel = Temp.newlabel()
        in
            Nx(seq[Tr.MOVE(exp2loc var, lo),
                        Tr.CJUMP(Tr.LE, var, hi, bodylabel, breaklabel),
                        Tr.LABEL(bodylabel),
                        body,
                        Tr.CJUMP(Tr.LT, var, hi, updatelabel, breaklabel),
                        Tr.LABEL(updatelabel),
                        Tr.MOVE(exp2loc var, Tr.BINOP(Tr.PLUS, var, Tr.CONST 1)),
                        Tr.JUMP(Tr.NAME(bodylabel), [bodylabel]),
                        Tr.LABEL(breaklabel)])
        end

    fun arrayIR (sizeEx, initEx) =
        Ex(F.externalCall("initArray", [unEx sizeEx, unEx initEx]))

    fun subscriptIR (arrEx, indexEx) =
        let
            val addr = Temp.newtemp()
            val arr = unEx arrEx
            val index = unEx indexEx
        in
            Ex(Tr.ESEQ(
               Tr.MOVE(Tr.TEMPLOC(addr),
                       Tr.BINOP(Tr.PLUS, arr,
                                Tr.BINOP(Tr.MUL, index, Tr.CONST(F.wordSize)))),
               Tr.MEM(Tr.TEMP(addr))))
        end

    fun recordIR (exps) =
        let
            val n = length exps
            val r = Temp.newtemp()
            val recordInit = Tr.MOVE(Tr.TEMPLOC(r), F.externalCall("initRecord", [Tr.CONST n]))
            fun setField (exp, elem) = Tr.MOVE((Tr.MEMLOC(
                                                    Tr.BINOP(Tr.PLUS, Tr.TEMP(r), Tr.CONST(F.wordSize * elem)))), 
                                                    unEx exp)
            fun instantiateFields ([]) = [recordInit]
              | instantiateFields (head :: l) = (setField(head, n - 1 - length l)) :: (instantiateFields (l))
            fun convert ([]) = Tr.EXP(Tr.CONST 0)
              | convert ([s]) = s
              | convert (f::t) = seq([f, convert(t)])
        in
            Ex(Tr.ESEQ(
                convert(instantiateFields(exps)), 
                Tr.TEMP(r)))
        end

    fun fieldIR (nameEx, elem) =
        Ex(Tr.MEM(Tr.BINOP(
                    Tr.PLUS, unEx nameEx, 
                    Tr.BINOP(Tr.MUL, Tr.CONST(elem), Tr.CONST (F.wordSize)))))

    fun sequencingIR [] = Ex (Tr.CONST 0)
      | sequencingIR [exp] = exp
      | sequencingIR (head :: l) = Ex (Tr.ESEQ (unNx head, unEx (sequencingIR l)))

    fun nilIR () = Ex (Tr.CONST 0)

    fun intIR (n) = Ex (Tr.CONST n)

    fun stringIR(lit) = 
        let
          fun checkFragLit(frag) =
            case frag of 
                 F.PROC(_) => false
               | F.STRING(lab', lit') => String.compare(lit', lit) = EQUAL
          fun genFragLabel() =
            case List.find checkFragLit (!fragList) of
                 SOME(F.STRING(lab', lit')) => lab'
               | _ => 
                   let
                     val lab' = Temp.newlabel()
                   in
                      fragList := F.STRING(lab', lit)::(!fragList);
                      F.STRING(lab', lit)::(!fragList);
                      lab'
                   end
          val lab = genFragLabel()
        in
          Ex(Tree.NAME(lab))
        end

    fun callexpIR (TOPLEVEL, calllevel, label, args) = Ex (Tr.TEMP F.FP)
      | callexpIR (declevel as NONTOP{uniq, parent, frame}, calllevel, label, args) =
        let
            val sl = followSLs parent calllevel (Tr.TEMP F.FP)
            val unExedArgs = map unEx args
        in
            Ex (Tr.CALL (Tr.NAME label, sl :: unExedArgs))
        end

    fun procEntryExit({level=level', body=body'}) = 
        let
          val levelFrame =
            case level' of
                TOPLEVEL => (Err.error 0 "Fundec should not happen in outermost";   
                             F.newFrame {name=Temp.newlabel(), formals=[]})
              | NONTOP({uniq=_, parent=_, frame=frame'}) => frame'
          val treeBody = unNx body'
        in
          fragList := F.PROC({body=treeBody, frame=levelFrame})::(!fragList)
        end
        
    fun getResult() = !fragList

    fun resetFragList() = fragList := []

    fun concatExpList(expList, body as exp') =
        let
          fun createExpListStm(a::l) = unNx(a)::createExpListStm(l)
            | createExpListStm([]) = []
        in
          Ex(Tr.ESEQ(seq(createExpListStm expList), unEx(exp')))
        end
end
