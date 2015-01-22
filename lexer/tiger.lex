type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
[\ \t\n]+ => (lex());

type => (Tokens.TYPE(yypos,yypos+4));
var  => (Tokens.VAR(yypos,yypos+3));
function  => (Tokens.FUNCTION(yypos,yypos+8));
break => (Tokens.BREAK(yypos,yypos+5));
of => (Tokens.OF(yypos,yypos+2));
end => (Tokens.END(yypos,yypos+3));
in => (Tokens.IN(yypos,yypos+2));
nil => (Tokens.NIL(yypos,yypos+3));
let => (Tokens.LET(yypos,yypos+3));
do => (Tokens.DO(yypos,yypos+2));
to => (Tokens.TO(yypos,yypos+2));
for => (Tokens.FOR(yypos,yypos+3));
while => (Tokens.WHILE(yypos,yypos+5));
else => (Tokens.ELSE(yypos,yypos+4));
then => (Tokens.THEN(yypos,yypos+4));
if => (Tokens.IF(yypos,yypos+2));





"," => (Tokens.COMMA(yypos,yypos+1));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());