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
	type exp = unit (* TODO *)
	type level = int (* TODO *)
	type access = int (* TODO *)

	val outermost = 0 (* TODO *)
	fun newLevel {parent, name, formals} = 0 (* TODO *)
	fun formals _ = [] (* TODO *)
	fun allocLocal _ _ = 0 (* TODO *)
end