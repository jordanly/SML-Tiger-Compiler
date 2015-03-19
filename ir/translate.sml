structure F = MipsFrame

signature TRANSLATE = 
sig
	type exp
	type level
	type access
	
	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal : level -> bool -> access

	val simpleVarIR : access * level -> exp
	val binopIR : Tr.binop * exp * exp -> exp
	val relopIR : Tr.relop * exp * exp -> exp
	val ifIR : exp * exp * exp -> exp
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

	val outermost = TOPLEVEL
	fun newLevel {parent, name, formals} = 
	    let
	    	val formals'= true::formals (* Add static link *)
	    in
	    	NONTOP({uniq = ref (), parent=parent, frame=F.newFrame {name=name, formals=formals'}})
	    end

	fun formals {parent=parent', frame=frame'} = 
	    let
	    	fun addLevel(f, l) = ({parent=parent', frame=frame'}, f)::l
	    in
	    	foldl addLevel [] (F.formals frame')
	    end

    fun printLevel level' =
      case level' of
           TOPLEVEL => print "Current level = TOPLEVEL\n"
         | NONTOP({uniq=uniq', parent=parent', frame=frame'}) => print ("Level is NONTOP: " ^ Symbol.name (F.name frame') ^ "\n")

	fun allocLocal level' escape' = 
      (printLevel level';
      case level' of
           NONTOP({uniq=uniq', parent=parent', frame=frame'}) => (NONTOP({uniq=uniq', parent=parent', frame=frame'}), F.allocLocal frame' escape')
         | TOPLEVEL => (outermost, F.allocLocal (F.newFrame {name=Temp.newlabel(), formals=[]}) escape') (* TODO error? *)
      )

    fun unEx (Ex e) = e 
      | unEx (Cx genstm) = 
			let 
				val r = Temp.newtemp() 
				val t = Temp.newlabel() and f = Temp.newlabel() 
			in
				Tr.ESEQ(Tr.SEQ[Tr.MOVE(Tr.TEMPLOC r, Tr.CONST 1), 
							genstm(t,f), 
							Tr.LABEL f, 
							Tr.MOVE(Tr.TEMPLOC r, Tr.CONST 0), 
							Tr.LABEL t], 
						Tr.TEMP r) 
			end 
      | unEx (Nx s) = Tr.ESEQ(s, Tr.CONST 0)

    fun unCx (Cx c) = c
      | unCx (Ex (Tr.CONST 0)) = (fn(tlabel, flabel) => Tr.JUMP(Tr.NAME(flabel), [flabel]))
      | unCx (Ex (Tr.CONST 1)) = (fn(tlabel, flabel) => Tr.JUMP(Tr.NAME(tlabel), [tlabel]))
      | unCx (Ex e) = (fn(tlabel, flabel) => Tr.CJUMP(Tr.EQ, Tr.CONST 1, e, tlabel, flabel))
      | unCx (Nx _) = (ErrorMsg.error 0 "Compiler error"; fn (a, b) => Tree.LABEL(Temp.newlabel()))

    fun unNx (Ex e) = Tr.EXP(e)
      | unNx (Nx n) = n
      | unNx (c) = unNx(Ex(unEx(c)))

    (* Only handles calllevel = funlevel right now; doesn't calculate static links *)
    fun simpleVar ((funlevel, fraccess), calllevel) = Ex(F.exp fraccess (Tr.TEMP(F.FP)))

    fun binopIR (binop, left, right) = Ex(Tr.BINOP(binop, unEx(left), unEx(right)))

    fun relopIR (relop, left, right) = Cx(fn (t, f) => Tr.CJUMP(relop, unEx(left), unEx(right), t, f))

    fun ifIR (test, then', else') =
    	let
    		val genstm = unCx(test)
    		val e2 = unEx(then')
    		val e3 = unEx(else')
    		val t = Temp.newlabel()
    		val f = Temp.newlabel()
    		val join = Temp.newlabel()
    	in
    		Nx(Tr.SEQ [
    			genstm(t, f),
    			Tr.LABEL(t), Tr.EXP(e2), Tr.JUMP(Tr.NAME(join), [join]),
    			Tr.LABEL(f), Tr.EXP(e3), Tr.JUMP(Tr.NAME(join), [join])
    		])
    	end
end
