Jordan Ly (jl455), Cody Lieu (cal53), Kevin Do (kkd10)
ECE 553 Lexer
January 30, 2015

Our lexer is fairly straightforward.

Lines 32 and 33 ignore whitespace and increment counters on newlines.

We allow integers to begin with a 0, per Professor Hilton's post on Piazza.

Our identifier format is a direct implementation of the description from Appendix A.

To deal with nested comments, we use starting states. Parsing the "/*" string takes the lexer into the COMMENT state, in which it ignores all characters, except for "/*" which increments the comment depth, and "*/" which decrements it.

To handle strings, we use a similar strategy. Parsing a double quotes takes the lexer into the STRING state, in which we build up a string buffer ref variable. Seeing the end of string causes us to create the STRING token with the contents of the string buffer.

The first rule in the STRING state allows us to match any printable character or spaces, except backslashes and double quotes. We handle all of the escape sequences next, by appending the proper escaped character to the string buffer. \f____f\ is ignored through use of the continue() command.

At EOF we check for two cases, an unclosed comment and an unclosed string. To do this, we modified the eof function to check two variables, commentDepth and inString. if commentDepth is greater than 0 (an unclosed comment) or inString is 1 (binary var for unclosed string) then an appropriate ErrorMsg is returned.