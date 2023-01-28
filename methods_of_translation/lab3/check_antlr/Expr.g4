grammar Expr;
prog:   (expr NEWLINE)* ;
expr:   expr ('*'|'/') expr
    |   expr ('+'|'-') expr
    |   SPACES* INT SPACES*
    |   '(' expr ')'
    ;

SPACES  : [ \t]+ -> skip ;
NEWLINE : [\r\n]+ ;
INT     : [0-9]+ ;