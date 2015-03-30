Kevin Do (kkd10), Cody Lieu (cal53), Jordan Ly (jl455)
Instruction Selection

To test, run "sml < a" at the shell. This test script prints the AST,
type-checking errors, pre-canonicalization IR, post-canonicalization IR,
and emitted MIPS for each fragment.

We have completed this portion of the assignment, and to our knowledge it fulfills
all requirements laid out in Appel and in class.

Notes:
+ Appel's given canon.sml produces numerous "match nonexhaustive" warnings.
	To our knowledge, these do not affect the correctness of our compiler.

+ We lost points on our last phase because "It seems that your function
	definitions are missing the first part where you take the arguments
	and put them into temps." However, this is item 4 of procEntryExit1
	described in Appel (p. 168). Professor Hilton specifically stated on
	Piazza that we did not have to implement this function for that phase:
	https://piazza.com/class/i4ab28jtiig3jq?cid=98. We therefore believe that
	those points were incorrectly deducted.