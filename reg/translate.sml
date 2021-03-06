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

    (* Only returns perfect integers or NONE *)
    fun logBase2 0 = NONE
      | logBase2 1 = SOME 0
      | logBase2 n =
            if n mod 2 = 0
            then case logBase2 (n div 2) of
                    SOME x => SOME (x + 1)
                  | NONE => NONE
            else NONE

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
                foldr addLevel [] (F.formals frame)
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
      | unEx (Nx (Tr.SEQ(stm, Tr.SEQ stms))) = Tr.ESEQ(stm, unEx (Nx (Tr.SEQ stms)))
      | unEx (Nx (Tr.SEQ(stm, Tr.EXP laste))) = Tr.ESEQ(stm, laste)
      | unEx (Nx s) = Tr.ESEQ(s, Tr.CONST 0)

    fun unCx (Cx c) = c
      | unCx (Ex (Tr.CONST 0)) = (fn(tlabel, flabel) => Tr.JUMP(Tr.NAME(flabel), [flabel]))
      | unCx (Ex (Tr.CONST 1)) = (fn(tlabel, flabel) => Tr.JUMP(Tr.NAME(tlabel), [tlabel]))
      | unCx (Ex e) = (fn(tlabel, flabel) => Tr.CJUMP(Tr.EQ, Tr.CONST 1, e, tlabel, flabel))
      | unCx (Nx _) = (ErrorMsg.error 0 "Compiler error: unCx an Nx"; fn (a, b) => Tree.LABEL(Temp.newlabel()))

    fun unNx (Ex e) = Tr.EXP(e)
      | unNx (Nx n) = n
      | unNx (c) = unNx(Ex(unEx(c)))

    fun eseq[] = Tr.CONST 0
      | eseq[ex] = unEx ex
      | eseq(stm::stms) = Tr.ESEQ(unNx stm, eseq(stms))

    fun followSLs TOPLEVEL TOPLEVEL bestguess = (Err.error 0 "Following static links failed"; bestguess)
              | followSLs TOPLEVEL _ bestguess = (Err.error 0 "Following static links failed"; bestguess)
              | followSLs _ TOPLEVEL bestguess = (Err.error 0 "Following static links failed"; bestguess)
              | followSLs (declevel as NONTOP{uniq=uniqdec, parent=_, frame=_}) (uselevel as NONTOP{uniq=uniquse, parent=useparent, frame=_}) bestguess =
                    if uniqdec = uniquse
                    then bestguess
                    else followSLs declevel useparent (Tr.MEM bestguess)

    fun simpleVarIR ((declevel, fraccess), uselevel) =
        Ex(F.exp (fraccess, followSLs declevel uselevel (Tr.TEMP F.FP)))

    fun binopIR (Tr.PLUS, Ex(Tr.CONST a), Ex(Tr.CONST b)) = Ex(Tr.CONST (a + b))
      | binopIR (Tr.PLUS, Ex(Tr.CONST 0), right) = right
      | binopIR (Tr.PLUS, left, Ex(Tr.CONST 0)) = left
      | binopIR (Tr.MINUS, left, Ex(Tr.CONST 0)) = left
      | binopIR (Tr.MINUS, Ex(Tr.CONST a), Ex(Tr.CONST b)) = Ex(Tr.CONST (a - b))
      | binopIR (Tr.MUL, Ex(Tr.CONST 0), right) = Ex(Tr.CONST 0)
      | binopIR (Tr.MUL, left, Ex(Tr.CONST 0)) = Ex(Tr.CONST 0)
      | binopIR (Tr.MUL, Ex(Tr.CONST 1), right) = right
      | binopIR (Tr.MUL, left, Ex(Tr.CONST 1)) = left
      | binopIR (Tr.MUL, Ex(Tr.CONST a), Ex(Tr.CONST b)) = Ex(Tr.CONST (a * b))
      | binopIR (binop as Tr.MUL, left as Ex(Tr.CONST a), right) =
            (case logBase2 a of
                SOME lg_a => Ex(Tr.BINOP(Tr.LSHIFT, unEx(right), Tr.CONST(lg_a)))
              | NONE => Ex(Tr.BINOP(binop, unEx(left), unEx(right)))
            )
      | binopIR (binop as Tr.MUL, left, right as Ex(Tr.CONST b)) =
            (case logBase2 b of
                SOME lg_b => Ex(Tr.BINOP(Tr.LSHIFT, unEx(left), Tr.CONST(lg_b)))
              | NONE => Ex(Tr.BINOP(binop, unEx(left), unEx(right)))
            )
      | binopIR (Tr.DIV, Ex(Tr.CONST a), Ex(Tr.CONST b)) = Ex(Tr.CONST (a div b))
      | binopIR (binop, left, right) = Ex(Tr.BINOP(binop, unEx(left), unEx(right)))

    (* ty mostly for comparing if STRING *)
    fun relopIR (relop, left, right, ty) = 
        let 
            val leftExp = unEx left
            val rightExp = unEx right
        in
            case (ty,relop,leftExp,rightExp) of
                (T.STRING, Tr.EQ, _, _) => Ex(F.externalCall("tig_stringEqual", [leftExp, rightExp]))
              | (T.STRING, Tr.NE, _, _) => Ex(Tr.BINOP(Tr.MINUS, Tr.CONST 1, F.externalCall("tig_stringEqual", [leftExp, rightExp])))
              | (T.STRING, Tr.LE, _, _) => Ex(F.externalCall("stringLE", [leftExp, rightExp]))
              | (T.STRING, Tr.LT, _, _) => Ex(F.externalCall("stringLT", [leftExp, rightExp]))
              | (T.STRING, Tr.GE, _, _) => Ex(F.externalCall("stringGE", [leftExp, rightExp]))
              | (T.STRING, Tr.GT, _, _) => Ex(F.externalCall("stringGT", [leftExp, rightExp]))
              | (T.INT, Tr.EQ, Tr.CONST a, Tr.CONST b) => if a = b then Ex(Tr.CONST 1) else Ex(Tr.CONST 0)
              | (T.INT, Tr.NE, Tr.CONST a, Tr.CONST b) => if a <> b then Ex(Tr.CONST 1) else Ex(Tr.CONST 0)  
              | (T.INT, Tr.LE, Tr.CONST a, Tr.CONST b) => if a <= b then Ex(Tr.CONST 1) else Ex(Tr.CONST 0)
              | (T.INT, Tr.LT, Tr.CONST a, Tr.CONST b) => if a < b then Ex(Tr.CONST 1) else Ex(Tr.CONST 0)
              | (T.INT, Tr.GE, Tr.CONST a, Tr.CONST b) => if a >= b then Ex(Tr.CONST 1) else Ex(Tr.CONST 0)
              | (T.INT, Tr.GT, Tr.CONST a, Tr.CONST b) => if a > b then Ex(Tr.CONST 1) else Ex(Tr.CONST 0)
              | _        => Cx(fn (t, f) => Tr.CJUMP(relop, leftExp, rightExp, t, f))
        end          

    fun ifthenIR(test, then') =
        let
            val genstm = unCx(test)
            val thenExp = unEx(then')
            val t = Temp.newlabel()
            val join = Temp.newlabel()
        in
            Ex(Tr.ESEQ(seq[
                genstm(t, join),
                Tr.LABEL(t), Tr.EXP(thenExp),
                Tr.LABEL(join)
            ], Tr.CONST 0))
        end
    fun ifthenelseIR (Ex(Tr.CONST 0), then', else') = Ex(unEx(else'))
      | ifthenelseIR (Ex(Tr.CONST 1), then', else') = Ex(unEx(then'))
      | ifthenelseIR (test, then', else') =
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
                Tr.LABEL(f), Tr.MOVE(Tr.TEMPLOC(resulttemp), e3), Tr.JUMP(Tr.NAME(join), [join]),
                Tr.LABEL(join)
            ], Tr.TEMP(resulttemp)))
        end

    fun exp2loc (Tr.MEM exp') = Tr.MEMLOC exp'
      | exp2loc (Tr.TEMP temp') = Tr.TEMPLOC temp'
      | exp2loc (Tr.ESEQ (stm', exp' as Tr.MEM(_))) = Tr.ESEQLOC(stm', exp2loc exp')
      | exp2loc (Tr.ESEQ (stm', exp' as Tr.TEMP(_))) = Tr.ESEQLOC(stm', exp2loc exp')
      | exp2loc _ = (Err.error 0 "Can't convert exp to loc"; Tr.TEMPLOC(Temp.newtemp()))

    fun assignIR (left, right) = Nx (Tr.MOVE (exp2loc (unEx left), unEx right))

    fun whileIR (Ex (Tr.CONST 0), _, _) = Ex(Tr.CONST 0)
      | whileIR (test, body, breaklabel) =
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

    fun fullForIR (varEx, escape, loEx, hiEx, bodyNx, breaklabel) = 
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

    fun forIR (varEx, escape, loEx as Ex (Tr.CONST loval), hiEx as Ex (Tr.CONST hival), bodyNx, breaklabel) = 
            if loval > hival then Ex (Tr.CONST 0)
            else if loval = hival
                then
                    let
                        val var = unEx(varEx)
                        val lo = unEx(loEx)
                        val body = unNx(bodyNx)
                    in
                        Nx(seq[Tr.MOVE(exp2loc var, lo),
                                    body,
                                    Tr.LABEL(breaklabel)])
                    end
            else 
                fullForIR (varEx, escape, loEx, hiEx, bodyNx, breaklabel)
      | forIR (varEx, escape, loEx, hiEx, bodyNx, breaklabel) = fullForIR (varEx, escape, loEx, hiEx, bodyNx, breaklabel)

    fun arrayIR (sizeEx, initEx) =
        Ex(F.externalCall("tig_initArray", [unEx sizeEx, unEx initEx]))

    fun subscriptIR (arrEx, Ex (Tr.CONST 0)) = Ex(Tr.MEM(unEx arrEx))
      | subscriptIR (arrEx, indexEx) =
            let
                val addr = Temp.newtemp()
                val arr = unEx arrEx
                val index = unEx indexEx
                (* Add one to index since index 0 is size of array*)
                val accessVal = Tr.BINOP(Tr.MUL, Tr.BINOP(Tr.PLUS, index, Tr.CONST 1), Tr.CONST(F.wordSize))
            in
                Ex(Tr.ESEQ(
                   Tr.MOVE(Tr.TEMPLOC(addr), Tr.BINOP(Tr.PLUS, arr, accessVal)),
                   Tr.MEM(Tr.TEMP(addr))))
            end

    fun recordIR (exps) =
        let
            val n = length exps
            val r = Temp.newtemp()
            (* times 4 in allocrecord since appel only allocs n bytes in runtime *)
            val recordInit = Tr.MOVE(Tr.TEMPLOC(r), F.externalCall("tig_allocRecord", [Tr.CONST (n * 4)]))
            fun setField (exp, elem) = Tr.MOVE((Tr.MEMLOC(
                                                    Tr.BINOP(Tr.PLUS, Tr.TEMP(r), Tr.CONST(F.wordSize * elem)))), 
                                                    unEx exp)
            fun instantiateFields ([]) = [recordInit]
              | instantiateFields (head :: l) = (setField(head, length l)) :: (instantiateFields (l))
            fun convert ([]) = Tr.EXP(Tr.CONST 0)
              | convert ([s]) = s
              | convert (f::t) = seq([f, convert(t)])
        in
            Ex(Tr.ESEQ(
                convert(rev(instantiateFields(exps))),  
                Tr.TEMP(r)))
        end

    fun fieldIR (nameEx, elem) =
        Ex(Tr.MEM(Tr.BINOP(
                    Tr.PLUS, unEx nameEx, 
                    Tr.CONST(F.wordSize * elem))))

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

    fun callexpIR (TOPLEVEL, calllevel, label, args) = Ex (Tr.CALL (Tr.NAME label, map unEx args))
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
          val treeBody = Tr.MOVE(Tr.TEMPLOC(F.RV), unEx body')
          val treeBody' = F.procEntryExit1(levelFrame, treeBody)
        in
          fragList := F.PROC({body=treeBody', frame=levelFrame})::(!fragList)
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
