Extra Credit README

===== Type-checker =====
Purely functional records (already received 25% or 5/20 bonus on type checker for this)

===== IR =====
Dead-code elimination / Code simplification
	if 0 then _ else _ => JUMP not CJUMP (test101.tig)
	if 1 then _ else _ => JUMP not CJUMP (test102.tig)
	WHILE 0 => nothing (test113.tig) NOT DONE
	WHILE 1 => JUMP not CJUMP at end
	FOR with low > hi => Nothing
	FOR with low = hi => no jumps
	
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
	ARRAY[0] => doesn't generate mult instruction

Miscellaneous
	String literals get allocated only once per unique string (test100.tig)

===== Register allocation =====
heuristics for smart spilling
live range spilling
graph coloring spill spot allocation
coalesce
interprocedural register allocation (never has been done before)