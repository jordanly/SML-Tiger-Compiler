Kevin Do (kkd10), Cody Lieu (cal53), Jordan Ly (jl455)
ECE 553 Type-Checker
March 6, 2015

To test, run "sml < a" at the shell.

NOTE: We used a late day on this assignment so that although it was previously
due on March 6, the late day and spring break extended the due date until 
March 16.

NOTE: We have completed the "purely functional types" extra credit.

We have completed the type-checker and it works correctly for all test cases
to the best of our knowledge.

The main files we have edited are semant.sml and types.sml with some small
changes to env.sig/env.sml.

Our type-checker is fairly straight forward. Below is list of some of the less
intuitive or standard implementation details:

- EXTRA CREDIT: We have implemented purely functional types (as discussed in
piazza and in class). We generate and pass around record fields through a
unit function. This allows us to avoid using refs mutually recursive 
declerations. We never assign the ref portion of T.NAME. Instead of using
a unit -> (symbol * ty) list, we use a unit -> (symbol * symbol) list, 
but the reasoning is similar. We made this change to pass in the correct
type environment for type checking recursive declarations.

- Break statements: We implement the checking of break statements through
creating a nestDepth variable that tracks if we are in a loop. In cases like when
we enter a let statement, we hold the current count and set the nestDepth to 0,
resetting to the original value after we exit.

- For-Loop Index Variables: We prevent assigning to for-loop index variables
by adding a "read_only" field to the Env.VarEntry.

- We implemented a basic type lattice in types.sml. It supports a comparison operator
that returns EQ, LT, GT, or INCOMP.

Overall, this portion of the compiler was very challenging but very rewarding.
