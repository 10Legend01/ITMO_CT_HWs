
grammar Calc_v3;

stat:   e NEWLINE           {print($e.v);}
    |   NEWLINE
    ;

e returns [int v]
    : a=e op=('*'|'/') b=e  {$v = self.eval($a.v, $op, $b.v)}
    | a=e op=('+'|'-') b=e  {$v = self.eval($a.v, $op, $b.v)}
    | INT                   {$v = $INT.int}
    | '(' e ')'             {$v = $e.v}
    ;

MUL : '*' ;
DIV : '/' ;
ADD : '+' ;
SUB : '-' ;
TEST : 'asd'? ;

INT :   [0-9]+ ;         // match integers
NEWLINE:'\r'? '\n' ;     // return newlines to parser (is end-statement signal)
WS  :   [ \t]+ -> skip ; // toss out whitespace