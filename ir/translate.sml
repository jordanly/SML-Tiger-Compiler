structure F = MipsFrame
structure Tr = Tree

signature TRANSLATE = 
sig
	type exp
	type level
	type access
	
	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal : level -> bool -> access
end

structure Translate =
struct
	datatype level =
	    TOPLEVEL
	  | NONTOP of {parent: level, frame: F.frame}
	type access = level * F.access
	datatype exp = 
		Ex of Tree.exp
	  | Nx of Tree.stm
	  | Cx of Temp.label * Temp.label -> Tree.stm

	val outermost = {parent=nil, frame=nil}
	fun newLevel {parent, name, formals} = 
	    let
	    	val formals'= true::formals (* Add static link *)
	    in
	    	{parent=parent, frame=F.newFrame {name=name, formals=formals'}}
	    end

	fun formals {parent=parent', frame=frame'} = 
	    let
	    	fun addLevel(f, l) = ({parent=parent', frame=frame'}, f)::l
	    in
	    	foldl addLevel [] (F.formals frame')
	    end

	fun allocLocal {parent=parent', frame=frame'} escape' = 
    	({parent=parent', frame=frame'}, F.allocLocal frame' escape')

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
      | unCx (Ex e) = (fn(tlabel, flabel) => Tr.CJUMP(Tr.EQ, Tr.CONST 1, e, tlabel, flabel))
      | unCx (Nx _) = (Err.error 0 "Compiler error"; fn (a, b) => Tree.LABEL(Temp.newlabel()))
end
