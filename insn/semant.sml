structure R = Translate

structure Semant =
struct

    type venv = Env.enventry Symbol.table
    type tenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}

    (* Global helper functions *)
    fun checkInt ({exp=_, ty=T.INT}, pos) = ()
      | checkInt ({exp=_, ty=_ }, pos) = Err.error pos "error : integer required"

    fun checkEqualityOp ({exp=_, ty=T.INT}, {exp=_, ty=T.INT}, pos) = ()
      | checkEqualityOp ({exp=_, ty=T.STRING}, {exp=_, ty=T.STRING}, pos) = ()
      | checkEqualityOp ({exp=_, ty=T.RECORD(_, ref1)}, {exp=_, ty=T.RECORD(_, ref2)}, pos) = if ref1 = ref2 then () else Err.error pos "can't compare different record types"
      | checkEqualityOp ({exp=_, ty=T.NIL}, {exp=_, ty=T.RECORD(_, _)}, pos) = ()
      | checkEqualityOp ({exp=_, ty=T.RECORD(_, _)}, {exp=_, ty=T.NIL}, pos) = ()
      | checkEqualityOp ({exp=_, ty=T.ARRAY(_, ref1)}, {exp=_, ty=T.ARRAY(_, ref2)}, pos) = if ref1 = ref2 then () else Err.error pos "can't compare different array types"
      | checkEqualityOp ({exp=_, ty=_}, {exp=_, ty=_}, pos) = Err.error pos "error : comparison expected both int, string, record, or array"

    fun checkComparisonOp ({exp=_, ty=T.INT}, {exp=_, ty=T.INT}, pos) = ()
      | checkComparisonOp ({exp=_, ty=T.STRING}, {exp=_, ty=T.STRING}, pos) = ()
      | checkComparisonOp ({exp=_, ty=_ }, {exp=_, ty=_ }, pos) = Err.error pos "error: comparison of incompatible types"

    fun checkTypesEqual (tyA, tyB, pos, errMsg) = if T.eq(tyA, tyB)
                                                  then ()
                                                  else Err.error pos errMsg

    fun checkTypesAssignable (var, value, pos, errMsg) = if T.comp(var, value) = T.EQ orelse T.comp(var, value) = T.GT
                                                         then ()
                                                         else Err.error pos errMsg 

    val loopDepth : int ref = ref 0
    fun incrementLoopDepth () = loopDepth := !loopDepth + 1
    fun decrementLoopDepth () = loopDepth := !loopDepth - 1
    fun getLoopDepth () = !loopDepth
    fun setLoopDepth (n) = loopDepth := n
    fun checkInLoop (pos, errorMsg) =
      if !loopDepth = 0
      then Err.error pos errorMsg
      else ()

    (* Main recursive type-checking functions *)
    fun transExp (venv, tenv, exp, level: Translate.level, break) : expty = 
        let fun
            trexp (A.VarExp(var)) = trvar var
          | trexp (A.NilExp) = {exp=R.NIL, ty=T.NIL}
          | trexp (A.IntExp(intvalue)) = {exp=R.Ex(Tr.CONST intvalue), ty=T.INT}
          | trexp (A.StringExp(stringvalue, pos)) = {exp=R.stringIR(stringvalue), ty=T.STRING}
          | trexp (A.CallExp({func, args, pos})) = 
                let
                    fun checkArgs (forTy::formalList, argExp::argList, pos) = if T.eq(forTy, #ty (trexp argExp))
                                                                              then checkArgs(formalList, argList, pos)
                                                                              else Err.error pos "error : formals and actuals have different types"
                      | checkArgs ([], argExp::argList, pos) = Err.error pos "error : formals are fewer then actuals"
                      | checkArgs (forTy::formalList, [], pos) = Err.error pos "error : formals are more then actuals"
                      | checkArgs ([], [], pos) = ()
                    fun makearglist (a, b) = (#exp (trexp a))::b
                    val argExpList = foldr makearglist [] args
                in
                    case S.look(venv, func) of
                        SOME(Env.FunEntry({level=declevel, label, formals, result})) => (checkArgs(formals, args, pos); {exp=R.callexpIR(declevel, level, label, argExpList), ty=result})
                      | SOME(_) => (Err.error pos ("symbol not function " ^ S.name func); {exp=R.Ex(Tr.CONST 0), ty=T.BOTTOM})
                      | NONE => (Err.error pos ("no such function " ^ S.name func); {exp=R.Ex(Tr.CONST 0), ty=T.BOTTOM})
                end
          | trexp (A.OpExp{left, oper, right, pos}) = 
                (case oper of
                    A.PlusOp => (checkInt(trexp left, pos);
                                 checkInt(trexp right, pos);
                                 {exp=R.binopIR(Tr.PLUS, #exp (trexp left), #exp (trexp right)), ty=T.INT})
                  | A.MinusOp => (checkInt(trexp left, pos); 
                                  checkInt(trexp right, pos);
                                  {exp=R.binopIR(Tr.MINUS, #exp (trexp left), #exp (trexp right)), ty=T.INT})
                  | A.TimesOp => (checkInt(trexp left, pos); 
                                  checkInt(trexp right, pos);
                                  {exp=R.binopIR(Tr.MUL, #exp (trexp left), #exp (trexp right)), ty=T.INT})
                  | A.DivideOp => (checkInt(trexp left, pos); 
                                   checkInt(trexp right, pos);
                                   {exp=R.binopIR(Tr.DIV, #exp (trexp left), #exp (trexp right)), ty=T.INT})
                  | A.EqOp => (checkEqualityOp(trexp left, trexp right, pos);
                                let val {exp=exp', ty=ty'} = trexp left
                                in {exp=R.relopIR(Tr.EQ, exp', #exp (trexp right), ty'), ty=T.INT}
                                end
                              ) 
                  | A.NeqOp => (checkEqualityOp(trexp left, trexp right, pos);
                                let val {exp=exp', ty=ty'} = trexp left
                                in {exp=R.relopIR(Tr.NE, exp', #exp (trexp right), ty'), ty=T.INT}
                                end
                              ) 
                  | A.LtOp => (checkEqualityOp(trexp left, trexp right, pos);
                                let val {exp=exp', ty=ty'} = trexp left
                                in {exp=R.relopIR(Tr.LT, exp', #exp (trexp right), ty'), ty=T.INT}
                                end
                              ) 
                  | A.LeOp => (checkEqualityOp(trexp left, trexp right, pos);
                                let val {exp=exp', ty=ty'} = trexp left
                                in {exp=R.relopIR(Tr.LE, exp', #exp (trexp right), ty'), ty=T.INT}
                                end
                              ) 
                  | A.GtOp => (checkEqualityOp(trexp left, trexp right, pos);
                                let val {exp=exp', ty=ty'} = trexp left
                                in {exp=R.relopIR(Tr.GT, exp', #exp (trexp right), ty'), ty=T.INT}
                                end
                              ) 
                  | A.GeOp => (checkEqualityOp(trexp left, trexp right, pos);
                                let val {exp=exp', ty=ty'} = trexp left
                                in {exp=R.relopIR(Tr.GE, exp', #exp (trexp right), ty'), ty=T.INT}
                                end
                              ) 
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
                                    then (Err.error pos ("record list is wrong length: " ^ S.name typ); {exp=R.Ex(Tr.CONST 0), ty=x})
                                    else (foldr iterator () recFormal; {exp=R.recordIR(map #exp (map trexp (map #2 fields))), ty=x})
                                end
                          | _ => (Err.error pos ("error : expected record type, not: " ^ S.name typ); {exp=R.Ex(Tr.CONST 0), ty=T.NIL})
                        )
                  | NONE => (Err.error pos ("error : invalid record type: " ^ S.name typ); {exp=R.Ex(Tr.CONST 0), ty=T.NIL})
                )
          | trexp (A.SeqExp(expList)) = 
                let
                    fun helper((seqExp, pos), {stmlist=stmlist', ty=ty'}) =
                        let val {exp=exp'', ty=ty''} = trexp seqExp
                        in {stmlist=stmlist'@[(R.unNx exp'')], ty=ty''}
                        end
                    fun checkSequence sequence = foldl helper {stmlist=[], ty=T.UNIT} sequence
                in
                    {exp=R.Nx(R.seq(#stmlist (checkSequence expList))), ty= (#ty (checkSequence expList))}
                end
          | trexp (A.AssignExp({var, exp, pos})) = 
                let
                  fun getVarSymbol var' =
                    case var' of
                         A.SimpleVar(sym, _) => S.look(venv, sym)
                       | A.FieldVar(var', _, _) => getVarSymbol var'
                       | A.SubscriptVar(var', _, _) => getVarSymbol var'
                  fun canAssign var' =
                    case getVarSymbol var' of 
                         SOME(Env.VarEntry({access, ty, read_only})) => 
                              if read_only 
                              then Err.error pos "error : index variable erroneously assigned to"
                              else ()
                       | _ => Err.error pos "cannot assign to a function"
                in
                  canAssign var;
                  checkTypesAssignable(#ty (trvar var), #ty (trexp exp), pos, "error : mismatched types in assignment");

                  {exp=R.assignIR(#exp (trvar var), #exp (trexp exp)), ty=T.UNIT}
                end
          | trexp (A.IfExp({test, then', else', pos})) = 
                (
                checkTypesEqual(#ty (trexp test), T.INT, pos, "test in if exp does not evaluate to an int");
                case else' of 
                      SOME(elseExp) => 
                          (
                          case (#ty (trexp then'), #ty (trexp elseExp)) of
                                (T.RECORD(_), NIL) => {exp=R.ifIR(#exp (trexp test), #exp (trexp then'), #exp (trexp elseExp)), ty=(#ty (trexp then'))}
                              | (NIL, T.RECORD(_)) => {exp=R.ifIR(#exp (trexp test), #exp (trexp then'), #exp (trexp elseExp)), ty=(#ty (trexp elseExp))}
                              | (tyA, tyB) => (checkTypesEqual(tyA, tyB, pos, "error : types of then - else differ");
                                                {exp=R.ifIR(#exp (trexp test), #exp (trexp then'), #exp (trexp elseExp)),
                                                ty=(#ty (trexp then'))})
                          )
                    | NONE => (checkTypesEqual(#ty (trexp then'), T.UNIT, pos, "error : if-then returns non unit");
                                {exp=R.Ex(Tr.CONST 0), ty=(#ty (trexp then'))})
                )
          | trexp (A.WhileExp({test, body, pos})) = 
                (
                checkTypesEqual(#ty (trexp test), T.INT, pos, "test does not evaluate to an int");
                incrementLoopDepth();
                checkTypesEqual(#ty (trexp body), T.UNIT, pos, "error : body of while not unit");
                let 
                    val breakpoint = Temp.newlabel()
                    val answer = {exp=R.whileIR(#exp (transExp(venv, tenv, test, level, break)), #exp (transExp(venv, tenv, body, level, breakpoint)), breakpoint),
                    ty=T.UNIT}
                in
                    (decrementLoopDepth(); answer)
                end
                )
          | trexp (A.ForExp({var, escape, lo, hi, body, pos})) = 
                let
                    val venv' = S.enter(venv, var, Env.VarEntry({access=Translate.allocLocal level true,
                                                               ty=T.INT, read_only=true}))
                    val breakpoint = Temp.newlabel()
                    val _ = checkTypesEqual(#ty (trexp lo), T.INT, pos, "error : lo expr is not int")
                    val _ = checkTypesEqual(#ty (trexp hi), T.INT, pos, "error : hi expr is not int");
                    val {exp=bodyexp, ty=bodytype} = (incrementLoopDepth(); transExp(venv', tenv, body, level, breakpoint))
                    val _ = decrementLoopDepth()
                    val _ = checkTypesEqual(bodytype, T.UNIT, pos, "for body must be no value")
                in
                    case S.look(venv', var) of
                        SOME x =>
                            (case x of
                                Env.VarEntry{access, ty, read_only} => {exp=R.forIR(R.simpleVarIR(access, level),
                                                                                    escape,
                                                                                    #exp (trexp lo),
                                                                                    #exp (trexp hi),
                                                                                    bodyexp,
                                                                                    breakpoint),
                                                                    ty=T.UNIT}
                              | _ => (Err.error 0 "Compiler bug: ForExp var isn't VarEntry"; {exp=R.Ex(Tr.CONST 0), ty=T.UNIT})
                            )
                      | _ => (Err.error 0 "couldnt find forexp var"; {exp=R.Ex(Tr.CONST 0), ty=T.UNIT})
                end
          | trexp (A.BreakExp(pos)) =
                ( 
                checkInLoop(pos, "incorrect break");
                {exp=R.breakIR(break), ty=T.UNIT}
                )
          | trexp (A.LetExp({decs, body, pos})) = 
                let
                    val curDepth = !loopDepth
                    val _ = setLoopDepth(0)
                    val {venv=venv', tenv=tenv', expList=expList'} = transDec(venv, tenv, decs, level, break)
                    val _ = setLoopDepth(curDepth)
                    val bodyExpty = transExp(venv', tenv', body, level, break)
                    val newBody = R.concatExpList(expList', #exp bodyExpty)
                in
                    {exp=newBody, ty=(#ty bodyExpty)}
                end
          | trexp (A.ArrayExp({typ, size, init, pos})) = 
                let
                  fun getType(SOME(ty)) = ty
                    | getType(NONE) = T.BOTTOM
                  fun actualTy ty = 
                    case ty of
                        T.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
                      | someTy => someTy
                in
                  (
                  case S.look(tenv, typ) of
                      SOME(x) => 
                      (
                      case actualTy x of
                            T.ARRAY(ty, unique) => 
                            (
                            let
                              fun getType(SOME(ty)) = ty
                                | getType(NONE) = T.BOTTOM
                              fun actualTy ty = 
                                case ty of
                                    T.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
                                  | someTy => someTy
                            in
                              checkInt(trexp size, pos);
                              checkTypesEqual(#ty (trexp init), actualTy ty, pos, "error : initializing exp and array type differ");
                              {exp=R.arrayIR(#exp (trexp size), #exp (trexp init)), ty=T.ARRAY(ty, unique)}
                            end
                            )
                          | _ => (Err.error pos "Not of ARRAY type in array creation"; {exp=R.Ex(Tr.CONST 0), ty=T.BOTTOM})
                      )
                    | NONE => (Err.error pos "No such type"; {exp=R.Ex(Tr.CONST 0), ty=T.BOTTOM})
                  )
                end
        and trvar (A.SimpleVar(id, pos)) = 
                (case S.look(venv, id) of
                    SOME(Env.VarEntry({access, ty, read_only=_})) => {exp=R.simpleVarIR(access, level), ty=ty}
                  (*| SOME(Env.FunEntry({level, label, formals, result})) => {exp=R.Ex(Tr.TODO), ty=result}*) (* Testing to see if this is unnecessary *)
                  | _ => (Err.error pos ("error: undeclared variable " ^ S.name id); {exp=R.Ex(Tr.CONST 0), ty=T.BOTTOM})
                )
          | trvar (A.FieldVar(v, id, pos)) =
                 (case trvar v of
                    {exp=_, ty=T.RECORD(recGen, unique)} => 
                    let
                      val fields = recGen()
                      fun getFieldType ((fSymbol, fTy)::l, id, pos) = 
                            if String.compare(S.name fSymbol, S.name id) = EQUAL
                            then 
                              case S.look(tenv, fTy) of
                                    SOME(ty) => ty
                                  | NONE => (Err.error pos "type error in record"; T.BOTTOM)
                            else getFieldType(l, id, pos)
                        | getFieldType ([], id, pos) = (Err.error pos "no such field"; T.BOTTOM)
                      fun getIndex ([], id, cur) = cur
                        | getIndex (h :: t, id, cur) = if h = id then cur else getIndex(t, id, cur + 1)
                    in
                      {exp=R.fieldIR(#exp (trvar v), getIndex(map #1 fields, id, 0)), ty=getFieldType(fields, id, pos)}
                    end
                  | {exp=_, ty=_} => (Err.error pos ("error : variable not record"); {exp=R.Ex(Tr.CONST 0), ty=T.BOTTOM})
                )
          | trvar (A.SubscriptVar(v, subExp, pos)) = 
                let
                  fun getType(SOME(ty)) = ty
                    | getType(NONE) = T.BOTTOM
                  fun actualTy ty = 
                    case ty of
                        T.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
                      | someTy => someTy
                in
                  (case trvar v of
                      {exp=_, ty=T.ARRAY(arrTy, unique)} => (checkInt(trexp subExp, pos); {exp=R.subscriptIR(#exp (trvar v), #exp (trexp subExp)), ty=actualTy arrTy})
                    | {exp=_, ty=_} => (Err.error pos ("requires array"); {exp=R.Ex(Tr.CONST 0), ty=T.BOTTOM})
                  )
                end
        in
            trexp exp
        end
    and transDec(venv, tenv, decs, level, break) = 
        let fun
            trdec(venv, tenv, A.VarDec({name, escape, typ, init, pos}), expList) =
                let
                  fun getType(SOME(ty)) = ty
                    | getType(NONE) = T.BOTTOM
                  fun actualTy ty = 
                    case ty of
                        T.NAME(name, tyRef) => actualTy(getType(S.look(tenv, name)))
                      | someTy => someTy
                  val access' = Translate.allocLocal level (!escape)
                  fun createAssignExp() =
                      let
                        val left = R.simpleVarIR(access', level)
                        val right = #exp (transExp(venv, tenv, init, level, break))
                      in
                        R.assignIR(left, right)
                      end
                in
                    (
                    case typ of
                        SOME(symbol, pos) =>
                            (case S.look(tenv, symbol) of
                                SOME ty => (checkTypesAssignable(actualTy ty, #ty (transExp(venv, tenv, init, level, break)), pos, "error : mismatched types in vardec");
                                           {venv=S.enter(venv, name, (Env.VarEntry{access=access', ty=actualTy ty, read_only=false})),
                                            tenv=tenv, expList=createAssignExp()::expList})
                              | NONE => (Err.error pos "type not recognized"; {venv=venv, tenv=tenv, expList=createAssignExp()::expList})
                            )
                      | NONE =>
                            let 
                              val {exp, ty} = transExp(venv, tenv, init, level, break)
                            in 
                              if T.eq(ty, T.NIL)
                              then Err.error pos "error: initializing nil expressions not constrained by record type"
                              else ();
                              {venv=S.enter(venv, name, (Env.VarEntry{access=access', ty=ty, read_only=false})),
                                                                      tenv=tenv, expList=createAssignExp()::expList}
                            end
                    )
                end
          | trdec(venv, tenv, A.TypeDec(tydeclist), expList) =
                let
                  fun maketemptydec ({name, ty, pos}, tenv') = S.enter(tenv', name, T.BOTTOM)
                  val temp_tenv = foldl maketemptydec tenv tydeclist
                  fun foldtydec({name, ty, pos}, {venv, tenv, expList}) = {venv=venv, tenv=S.enter(tenv, name, transTy(temp_tenv, ty)), expList=expList}
                  val new_env = foldl foldtydec {venv=venv, tenv=tenv, expList=expList} tydeclist

                  fun checkIllegalCycle({name, ty, pos}, ()) = 
                  let
                    fun checkHelper(seenList, name) =
                      (
                      case S.look(#tenv new_env, name) of
                           SOME(T.NAME(symb, _)) => if List.exists (fn y => String.compare(S.name symb, S.name y) = EQUAL) seenList
                                                    then Err.error pos "error: mutually recursive types thet do not pass through record or array - cycle"
                                                    else checkHelper(name::seenList, symb)
                         | _ => ()
                      )
                  in
                    checkHelper([], name)
                  end

                  fun checkDuplicates({name, ty, pos}, seenList) = 
                      if List.exists (fn y => String.compare(S.name name, y) = EQUAL) seenList
                      then (Err.error pos "error : two types of same name in mutually recursive tydec"; seenList)
                      else (S.name name)::seenList

                in
                  foldl checkDuplicates [] tydeclist;
                  foldl checkIllegalCycle () tydeclist;
                  new_env
                end
          | trdec(venv, tenv, A.FunctionDec(fundeclist), expList) =
                let 
                    fun transrt rt =
                        (case S.look(tenv, rt) of 
                            SOME(rt') => rt'
                          | NONE => (Err.error 0 ("Return type unrecognized: " ^ S.name rt); T.BOTTOM)
                        )
                    fun transparam {name, escape, typ, pos} = 
                        (case S.look(tenv, typ) of
                            SOME t => {name=name, escape=escape, ty=t, pos=pos}
                          | NONE => (Err.error 0 ("Parameter type unrecognized: " ^ S.name typ);
                                     {name=name, escape=escape, ty=T.BOTTOM, pos=pos})
                        )
                    fun enterFuncs (func, venv) = 
                        let
                          val newlabel = Temp.newlabel()
                          fun getEscape {name=name', escape=escape', typ=typ', pos=pos'} = !escape'
                          fun genEscapeList params' = map getEscape params'
                        in
                          case func of 
                              {name, params, body, pos, result=SOME(rt, pos')} =>
                                    S.enter(venv, name, Env.FunEntry{level=Translate.newLevel {parent=level, name=newlabel, formals=genEscapeList params},
                                                                     label=newlabel, formals= map #ty (map transparam params), result=transrt rt})
                            | {name, params, body, pos, result=NONE} =>
                                    S.enter(venv, name, Env.FunEntry{level=Translate.newLevel {parent=level, name=newlabel, formals=genEscapeList params},
                                                                     label=newlabel, formals= map #ty (map transparam params), result=T.UNIT})
                        end
                    val venv' = foldr enterFuncs venv fundeclist
                    fun checkfundec({name, params, body, pos, result}) = 
                        let 
                            val newLevel = 
                                (case S.look(venv', name) of
                                     SOME(Env.FunEntry({level=level', label=_, formals=_, result=_})) => level'
                                   | _ => Translate.newLevel {parent=Translate.outermost, name=Temp.newlabel(), formals=[]}
                                   )
                            val result_ty = 
                                (case result of
                                    SOME(rt, pos') => transrt rt
                                  | NONE => T.UNIT
                                )
                            val params' = map transparam params
                            val allocatedFormals = R.formals newLevel
                            (* Hacky workaround since we already alloced formals we want to get
                            * their access so we use the index as we traverse. Starts at one
                            * since 0th index is static link by default
                            *)
                            fun enterparam ({name, escape, ty, pos}, (venv, curIndex)) = 
                              (S.enter(venv, name, Env.VarEntry{access=List.nth(allocatedFormals, curIndex),
                                                               ty=ty, read_only=false}), curIndex + 1)
                            val venv'' = #1 (foldl enterparam (venv', 1) params')
                            val body' = transExp (venv'', tenv, body, newLevel, break)
                        in
                            R.procEntryExit {level=newLevel, body=(#exp body')};
                            if not (T.eq((#ty body'), result_ty))
                            then Err.error pos ("Function body type doesn't match return type in function " ^ S.name name)
                            else ()
                        end 
                    fun foldfundec (fundec, ()) = checkfundec fundec
                    fun checkDuplicates({name, params, body, pos, result}, seenList) = 
                        if List.exists (fn y => String.compare(S.name name, y) = EQUAL) seenList
                        then (Err.error pos "error : two types of same name in mutually recursive fundec"; seenList)
                        else (S.name name)::seenList
                in
                    foldl checkDuplicates [] fundeclist;
                    foldr foldfundec () fundeclist;
                    {venv=venv', tenv=tenv, expList=expList}
                end
            and folddec(dec, {venv, tenv, expList}) = trdec(venv, tenv, dec, expList)
        in
            foldl folddec {venv=venv, tenv=tenv, expList=[]} decs
        end
    and transTy(tenv, ty) =
        let fun
            trty(tenv, A.NameTy (name, _)) =
                (case S.look(tenv, name) of
                    SOME _ => T.NAME(name, ref(NONE))
                  | NONE => (Err.error 0 ("Unrecognized name type: " ^ S.name name); T.NAME(name, ref(NONE)))
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
                    recGen();
                    T.RECORD (recGen, ref ())
                end
          | trty(tenv, A.ArrayTy (sym, pos')) =
                T.ARRAY (transTy (tenv, A.NameTy (sym, pos')), ref ())
        in
            trty(tenv, ty)
        end

    fun transProg (my_exp : A.exp) = 
    let
      val mainlabel = Temp.newlabel()
      val mainlevel = Translate.newLevel {parent=Translate.outermost, name=mainlabel, formals=[]}
      val _ = FindEscape.findEscape my_exp
      val mainexp = #exp (transExp (Env.base_venv, Env.base_tenv, my_exp, mainlevel, mainlabel))
    in
      R.procEntryExit {level=mainlevel, body=mainexp};
      R.getResult()
    end
end

structure Main = 
struct
    fun compile fileName = Semant.transProg (Parse.parse(fileName))
end
