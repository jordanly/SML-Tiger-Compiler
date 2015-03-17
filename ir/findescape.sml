signature FINDESCAPE = 
sig
	val findEscape : Absyn.exp -> unit
end

structure FindEscape : FINDESCAPE = 
struct
	type depth = int
	type escEnv = (depth * bool ref) Symbol.table

	fun traverseVar(env:escEnv, d:depth, s:Absyn.var) : unit = () (* TODO *)
	and traverseExp(env:escEnv, d:depth, s:Absyn.exp): unit = () (* TODO *)
	and traverseDecs(env:escEnv, d:depth, s:Absyn.dec list): escEnv = S.empty (* TODO *)

	fun findEscape(prog: Absyn.exp): unit = () (* TODO *)
end