Kevin Do (kkd10), Cody Lieu (cal53), Jordan Ly (jl455)
ECE 553 Frame analysis and IR
March 22, 2015

To test, run "sml < a" at the shell. This test script prints the AST,
type-checking errors, and the IR produced.

NOTE: We used a late day on this assignment so that although it was previously
due on March 20, we are turning it in on March 22.

We have completed this portion of the assignment, and to our knowledge it fulfills
all requirements laid out in Appel and in class.

The main files we have edited are semant.sml, translate.sml, and mipsframe.sml.

Some explanatory notes:
+ As instructed on Piazza, we do not implement procEntryExit in this phase

+ For efficiency, we specifically check the case of unCx (Ex (Tr.CONST 0)) and
	unCx (Ex (Tr.CONST 1)) and produce JUMP instead of CJUMPs

+ In translate.sml, we have a followSLs function that contains the static link logic

+ For record creation, we make an external call to "initRecord".
	This will probably end up being a thin wrapper around malloc, depending on future requirements

+ In tree.sml, we added loc = TEMPLOC of Temp.temp | MEMLOC of exp, which is used as the
	first argument to the MOVE type constructor. This is superior to Appel's method,
	since it allows us to use the SML type-checker to ensure we always have legal MOVE expressions.
	
+ In tree.sml, we changed SEQ of stm * stm to SEQ of stm list, as Professor Hilton mentioned in class.