Extra Credit (We need to generate test cases for these)
IR
	if 0 then _ else _ => JUMP not CJUMP (test101.tig)
	if 1 then _ else _ => JUMP not CJUMP (test102.tig)
	CONST + CONST => CONST not BINOP (test103.tig)
	CONST - CONST => CONST not BINOP (test104.tig)
	CONST * CONST => CONST not BINOP (test105.tig)
	CONST / CONST => CONST not BINOP (test105.tig)
	CONST = CONST => JUMP not CJUMP
	CONST <> CONST => JUMP not CJUMP
	CONST < CONST => JUMP not CJUMP
	CONST <= CONST => JUMP not CJUMP
	CONST > CONST => JUMP not CJUMP
	CONST >= CONST => JUMP not CJUMP
	WHILE 0 => nothing
	WHILE 1 => JUMP not CJUMP at end
	ARRAY[0] => doesn't generate mult instruction
	FOR with low > hi => Nothing
	FOR with low = hi => no loop, just body once
	
	String literals get allocated only once per unique string (test100.tig)
	Purely functional records (already received 25% bonus on IR for this)
