grammar Grm;

@header{from grm import *}

start returns [Object v]
        : NONTERM              {$v = new Grammar($NONTERM.text)}
          (choose_rule[$v] ';')+
        ;

choose_rule [gr]
        : TERM '=' STRING    {$gr.addTermRule(TermRule(false, $TERM.text, $STRING.text))}
        | TERM ':' STRING    {$gr.addTermRule(TermRule(true, $TERM.text, $STRING.text))}
        | non_term_rule            {$gr.addNonTermRule($non_term_rule.v)}
        ;

non_term_rule returns [Object v]
        : NONTERM args non_term_returns '=' {$v = NonTermRule($NONTERM.text, $args.v, $non_term_returns.v)}
          rightPart                 {$v.addRule($rightPart.v)}
          ('|' rightPart            {$v.addRule($rightPart.v)})*
        ;

args returns [Object v]
        : '('               {$v = list()}
          arg               {$v.add($arg.v)}
          (',' arg          {$v.add($arg.v)})*
          ')'
        |                   {$v = list()}
        ;

non_term_returns returns [Object v]
        : 'returns' '['        {$v = list()}
          arg               {$v.append($arg.v)}
          (',' arg          {$v.append($arg.v)})*
          ']'
        |                   {$v = list()}
        ;

arg returns [Object v]
        : l = var_or_type ':' r = var_or_type    {$v = Argument($l.v, $r.v)}
        ;

var_or_type returns [Object v]
        : TERM          {$v = $TERM.text}
        | NONTERM         {$v = $NONTERM.text}
        ;

rightPart returns [Object v]
        :               {$v = list()}
        (ruleToken      {$v.add($ruleToken.v)})+
        ;

ruleToken returns [Object v]
        : TERM          {$v = Term($TERM.text)}
        | NONTERM         {t = NonTerm($NONTERM.text)}
          ('(' param    {t.addParameter($param.v)}
          (',' param    {t.addParameter($param.v)})*
          ')')?         {$v = t}
        | CODE          {$v = Code($CODE.text)}
        ;


param returns [Object v]
        : CODE  {$v = $CODE.text[1, len($CODE.text) - 1]}
        | var_or_type   {$v = $var_or_type.v}
        ;


TERM   : [A-Z][a-zA-Z0-9_]*;
NONTERM  : [a-z][a-zA-Z0-9_]*;

STRING : '"' (~('"'))* '"';
CODE   : '{' (~[{}]+ CODE?)* '}';

WS : [ \t\r\n] -> skip;