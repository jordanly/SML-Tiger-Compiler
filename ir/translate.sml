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

structure F = MipsFrame
structure Translate =
struct
	type exp = unit (* TODO i think this is more ch7 stuff *)
	datatype level = level of {parent: level, frame: F.frame}
	type access = level * F.access

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
end
