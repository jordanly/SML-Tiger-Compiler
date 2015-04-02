Extra Credit README

===== Type-checker =====
Purely functional records (already received 25% or 5/20 bonus on type checker for this)

===== IR =====
Dead-code elimination / Code simplification
	if 0 then _ else _ => JUMP not CJUMP (test101.tig)
	if 1 then _ else _ => JUMP not CJUMP (test102.tig)
	WHILE 0 => no code or labels (test113.tig)
	WHILE 1 => JUMP not CJUMP at end (test114.tig)
	FOR with low > hi => (test115.tig)
	FOR with low = hi => no jumps (test116.tig, test117.tig)
	ARRAY[0] => doesn't generate mult instruction NOT DONE
	mult by power of 2 => LSHIFT NOT DONE
	divide by power of 2 => ARSHIFT NOT DONE
	UMINUS UMINUS exp => exp NOT DONE
	
Constant Folding
	CONST + CONST => CONST not BINOP (test103.tig)
	CONST - CONST => CONST not BINOP (test104.tig)
	CONST * CONST => CONST not BINOP (test105.tig)
	CONST / CONST => CONST not BINOP (test106.tig)
	CONST = CONST => JUMP not CJUMP (test107.tig) NOT DONE
	CONST <> CONST => JUMP not CJUMP (test108.tig) NOT DONE
	CONST < CONST => JUMP not CJUMP (test109.tig) NOT DONE
	CONST <= CONST => JUMP not CJUMP (test110.tig) NOT DONE
	CONST > CONST => JUMP not CJUMP (test111.tig) NOT DONE
	CONST >= CONST => JUMP not CJUMP (test112.tig) NOT DONE

Miscellaneous
	String literals get allocated only once per unique string (test100.tig)
	Common subexpression elimination NOT DONE
	Induction variable elimination NOT DONE
	Strength reduction NOT DONE
	Jump threading NOT DONE

===== Register allocation =====
heuristics for smart spilling NOT DONE
live range spilling NOT DONE
graph coloring spill spot allocation NOT DONE
coalesce NOT DONE
interprocedural register allocation (never has been done before) NOT DONE