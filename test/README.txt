Extra Credit (We need to generate test cases for these)
IR
	if 0 then _ else _ => JUMP not CJUMP (test101.tig)
	if 1 then _ else _ => JUMP not CJUMP (test102.tig)
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
	WHILE 0 => nothing (test113.tig) NOT DONE
	WHILE 1 => JUMP not CJUMP at end
	ARRAY[0] => doesn't generate mult instruction
	FOR with low > hi => Nothing
	FOR with low = hi => no loop, just body once
	
	String literals get allocated only once per unique string (test100.tig)
	Purely functional records (already received 25% bonus on IR for this)
