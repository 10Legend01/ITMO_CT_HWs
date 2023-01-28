grammar Var4;

start   :   lines EOF ;
lines     :   line* lastline ;

line    :   ( SPS* | expr | if | assign | while ) newline  ;
lastline    :   ( SPS* | expr | if | assign | while) newline?  ;

expr:   func
    |   expr op expr
    |   int | name
    |   '(' expr ')'
    ;

block   :   '{' lines '}' ;

assign  :   name EQ expr
        |   name MOD expr
        |   name MODONE
        ;

func    :   name '(' WSP* (expr ( WSP* ',' WSP* expr)*)? WSP* ')' ;

name    :   NAME ;
int     :   INT ;

if      :   'if' expr ':' block (WSP* elif)? (WSP* else)? ;
elif    :   'elif' expr ':' block (WSP* elif)? ;
else    :   'else' ':' block ;
while   :   'while' expr ':' block ;

op      :   OP | COMP ;

newline :   ALT | WSP ;

SPS     :   [ \t]+ -> skip ;
ALT     :   [;] ;
WSP     :   [\r]?[\n] ;
NAME    :   [a-zA-Z][0-9a-zA-Z]* ;
INT     :   [0-9]+ ;
EQ      :   '=' ;
MOD     :   '+=' | '-=' | '*=' | '/=' ;
MODONE  :   '++' | '--' ;
OP      :   '*' | '/' | '+' | '-' ;
COMP    :   '==' | '!=' | '<' | '<=' | '>' | '>=' ;
