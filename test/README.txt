List of Optimizations

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

	ARRAY[0] => doesn't generate mult instruction (test118.tig)

	add by 0 => no add instruction (test120.tig)
	subtract by 0 => no sub instruction (test121.tig)
	
	mult by 0 => 0 (test122.tig)
	mult by 1 => no mult (test123.tig)

Strength reduction
	mult by power of 2 => LSHIFT not MULT (test119.tig)
	
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

===== Instruction Selection =====
Tr.CONST 0 uses $zero, not li into some register (test124.tig)

===== Register allocation =====
Temps are colored with registers in the same order each time. This maximizes register reuse, so that we save the "higher-numbered" registers for when there is a lot of register pressure.
heuristics for smart spilling NOT DONE
coalesce NOT DONE