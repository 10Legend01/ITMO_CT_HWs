grammar Gramm;

@header{import gramm.*; }

start returns [Grammar v]
        : NONTERM              { $v = new Grammar($NONTERM.text); }
          (choose_rule[$v] ';')+
        ;

choose_rule [Grammar gr]
        : TERM '=' STRING    { $gr.addTermRule(new TermRule(false, $TERM.text, $STRING.text)); }
        | TERM ':' STRING    { $gr.addTermRule(new TermRule(true, $TERM.text, $STRING.text)); }
        | non_term_rule            { $gr.addNonTermRule($non_term_rule.v); }
        ;

non_term_rule returns [NonTermRule v]
        : NONTERM args non_term_returns '=' { $v = new NonTermRule($NONTERM.text, $args.v, $non_term_returns.v); }
          rightPart                 { $v.addRule($rightPart.v); }
          ('|' rightPart            { $v.addRule($rightPart.v); })*
        ;

args returns [List<Argument> v]
        : '('               { $v = new ArrayList<>(); }
          arg               { $v.add($arg.v); }
          (',' arg          { $v.add($arg.v); })*
          ')'
        |                   { $v = new ArrayList<>(); }
        ;

non_term_returns returns [List<Argument> v]
        : 'returns' '['        { $v = new ArrayList<>(); }
          arg               { $v.add($arg.v); }
          (',' arg          { $v.add($arg.v); })*
          ']'
        |                   { $v = new ArrayList<>(); }
        ;

arg returns [Argument v]
        : l = var_or_type ':' r = var_or_type    { $v = new Argument($l.v, $r.v); }
        ;

var_or_type returns [String v]
        : TERM          { $v = $TERM.text; }
        | NONTERM         { $v = $NONTERM.text; }
        ;

rightPart returns [List<Token> v]
        :               { $v = new ArrayList<>(); }
        (ruleToken      { $v.add($ruleToken.v); })+
        ;

ruleToken returns [Token v]
        : TERM          { $v = new Term($TERM.text); }
        | NONTERM         { NonTerm t = new NonTerm($NONTERM.text); }
          ('(' param    { t.addParameter($param.v); }
          (',' param    { t.addParameter($param.v); })*
          ')')?         { $v = t; }
        | CODE          { $v = new Code($CODE.text); }
        ;


param returns [String v]
        : CODE  { $v = $CODE.text.substring(1, $CODE.text.length() - 1); }
        | var_or_type   { $v = $var_or_type.v; }
        ;


TERM   : [A-Z][a-zA-Z0-9_]*;
NONTERM  : [a-z][a-zA-Z0-9_]*;

STRING : '"' (~('"'))* '"';
CODE   : '{' (~[{}]+ CODE?)* '}';

WS : [ \t\r\n] -> skip;