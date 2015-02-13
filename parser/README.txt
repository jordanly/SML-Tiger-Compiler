Jordan Ly (jl455), Cody Lieu (cal53), Kevin Do (kkd10)
ECE 553 Lexer
February 13, 2015

Our parser is fairly straight-forward.

Important note: The supplied lexer causes errors when parsing comments

On lines 51 to 62 we have our precedence directives

On lines 3-5 and 7-9, we declared functions to handle adding new type and function declarations

On line 77 we pretty print the AST

Lines 79-108 is the grammar for expressions. For the ambiguous productions like the operator expressions, we use precedence directives to specify which productions to use.
For other ambiguous productions like let expressions, function calls, and record expressions, we introduce new nonterminals using left refactoring to eliminate ambiguity.
An example of this is introducing the funarg and funargtail rules which eliminate ambiguity by recursively representing function arguments in function calls. By making the first symbol of the rule a COMMA, the ambiguity is reduced.

Our semantic actions are also fairly intuitive. We use the corresponding types in the absyn.sml module to build the AST.
Of special note are the semantic actions for negative integers and boolean "AND" and "OR" expressions.
Negative integers are represented by using the A.OpExp to subtract the expression from 0
"AND" and "OR" expressions are represented using A.IFExp.
Most of the other semantic actions have a one to one correspondence with datatypes in the absyn.sml module

