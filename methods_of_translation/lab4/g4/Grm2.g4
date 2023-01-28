grammar Grm2;

start : (choose_rule ';')* '_start_' NONTERM ';' (choose_rule ';')* ;

choose_rule
        : TERM ':' STRING skip_rule?
        | non_term_rule
        ;

skip_rule : '->' 'skip' ;

non_term_rule : NONTERM args? non_term_returns? ':' rightPart ('|' rightPart)* ;

args    : '[' arg (',' arg)* ']' ;

non_term_returns  : 'returns' args ;

arg
    : TERM
    | NONTERM
    ;

rightPart : (ruleToken)* ;

ruleToken   : TERM
            | NONTERM ('[' param (',' param)* ']')?
            | CODE
            ;


param   : CODE
        | arg
        ;


TERM   : [A-Z][a-zA-Z0-9_]*;
NONTERM  : [a-z][a-zA-Z0-9_]*;

STRING : '\'' ((('\\\'')|~('\'')))* '\'';
//STRING : '"' (~('"'))* '"';
CODE   : '{' ((('\\{')|('\\}')|~([{}])))* '}';
//CODE   : '{' (~[{}]+ CODE?)* '}';

WS : [ \t\r\n] -> skip;