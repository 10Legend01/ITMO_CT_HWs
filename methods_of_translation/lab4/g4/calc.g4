grammar calc;

expr returns [int val]
            : addSub {$val = addSub.val;}
              END
            ;

addSub returns [int val]
            : mulDiv addSubP[{mulDiv.val}]
              {$val = addSubP.val;}
            ;
addSubP [int left] returns [int val]
             : ADD mulDiv    { next = $left + $mulDiv.val;}
               addSubP[{next}] { val = addSubP.val;}
             | SUB mulDiv    { next = $left - $mulDiv.val;}
               addSubP[{next}] { val = $addSubP.val;}
             | {$val = $left;}
             ;
mulDiv returns [int val]
            : log mulDivP[{$log.val}]
              {$val = $mulDivP.val;}
            ;
mulDivP [int left] returns [int val]
             : MUL log { next = $left * $log.val;}
               mulDivP[{next}] {$val = $mulDivP.val;}
             | DIV log { next = $left / $log.val;}
               mulDivP[{next}] {$val = $mulDivP.val;}
             | {$val = $left;}
             ;
log returns [int val]
            : unary log_[{unary.val}] {$val = $log_.val;}
            ;

log_ [int left] returns [int val]
            : LOG log {$val = (int) (Math.log($left)/Math.log($log.val));}
            | {$val = $left;}
            ;

unary returns [int val]
            : SUB unary { val = -$unary.val;}
            | LPAREN addSub RPAREN { val = $addSub.val;}
            | NUM          { val = Integer.valueOf($NUM.text);}
            ;

ADD : '+';
SUB : '-';
MUL : '*';
LOG : 'lg';
DIV : '/';
LPAREN : '(';
RPAREN : ')';
NUM : ([1-9][0-9]*)|([0]);
END : '$';