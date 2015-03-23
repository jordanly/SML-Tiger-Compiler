structure Printtree : 
     sig val printtree : TextIO.outstream * Tree.stm -> unit end =
struct

fun printtree (outstream, s0) =
 let fun say s =  TextIO.output(outstream,s)
  fun sayln s= (say s; say "\n") 

  fun indent 0 = ()
    | indent i = (say " "; indent(i-1))

  fun stm(Tree.SEQ(stmlist),d) =
          (indent d;
          sayln "SEQ(";
          foldl (fn (a, b) => (stm(a,d+1); sayln ","; ())) () stmlist;
          say ")")
    | stm(Tree.LABEL lab, d) = (indent d; say "LABEL "; say (Symbol.name lab))
    | stm(Tree.JUMP (e,_), d) =  (indent d; sayln "JUMP("; exp(e,d+1); say ")")
    | stm(Tree.CJUMP(r,a,b,t,f),d) = (indent d; say "CJUMP(";
				relop r; sayln ",";
				exp(a,d+1); sayln ","; exp(b,d+1); sayln ",";
				indent(d+1); say(Symbol.name t); 
				say ","; say (Symbol.name f); say ")")
    | stm(Tree.MOVE(a,b),d) = (indent d; sayln "MOVE("; loc(a,d+1); sayln ",";
			    exp(b,d+1); say ")")
    | stm(Tree.EXP e, d) = (indent d; sayln "EXP("; exp(e,d+1); say ")")

  and exp(Tree.BINOP(p,a,b),d) = (indent d; say "BINOP("; binop p; sayln ",";
			       exp(a,d+1); sayln ","; exp(b,d+1); say ")")
    | exp(Tree.MEM(e),d) = (indent d; sayln "MEM("; exp(e,d+1); say ")")
    | exp(Tree.TEMP t, d) = (indent d; say "TEMP t"; say(Int.toString t))
    | exp(Tree.ESEQ(s,e),d) = (indent d; sayln "ESEQ("; stm(s,d+1); sayln ",";
			  exp(e,d+1); say ")")
    | exp(Tree.NAME lab, d) = (indent d; say "NAME "; say (Symbol.name lab))
    | exp(Tree.CONST i, d) = (indent d; say "CONST "; say(Int.toString i))
    | exp(Tree.CALL(e,el),d) = (indent d; sayln "CALL("; exp(e,d+1);
			   app (fn a => (sayln ","; exp(a,d+2))) el;
			   say ")")
    | exp(Tree.TODO, d) = (indent d; say "TODO")

  and loc(Tree.TEMPLOC t, d) = (indent d; say "TEMP t"; say(Int.toString t))
    | loc(Tree.MEMLOC(e),d) = (indent d; sayln "MEM("; exp(e,d+1); say ")")
    | loc(Tree.ESEQLOC(s,e),d) = (indent d; sayln "ESEQ("; stm(s,d+1); sayln ",";
        exp(e,d+1); say ")")

  and binop Tree.PLUS = say "PLUS"
    | binop Tree.MINUS = say "MINUS"
    | binop Tree.MUL = say "MUL"
    | binop Tree.DIV = say "DIV"
    | binop Tree.AND = say "AND"
    | binop Tree.OR = say "OR"
    | binop Tree.LSHIFT = say "LSHIFT"
    | binop Tree.RSHIFT = say "RSHIFT"
    | binop Tree.ARSHIFT = say "ARSHIFT"
    | binop Tree.XOR = say "XOR"

  and relop Tree.EQ = say "EQ"
    | relop Tree.NE = say "NE"
    | relop Tree.LT = say "LT"
    | relop Tree.GT = say "GT"
    | relop Tree.LE = say "LE"
    | relop Tree.GE = say "GE"
    | relop Tree.ULT = say "ULT"
    | relop Tree.ULE = say "ULE"
    | relop Tree.UGT = say "UGT"
    | relop Tree.UGE = say "UGE"

 in  stm(s0,0); sayln ""; TextIO.flushOut outstream
end

end

