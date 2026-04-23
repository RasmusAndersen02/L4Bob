%{
#include "encoding.h"
#include "parser.tab.h"
%}

%option yylineno
%option noyywrap
%%
"ADD" {yylval.bit16 = OP_ADD; return ARITH_BIN;}
"SUB" {yylval.bit16 = OP_SUB; return ARITH_BIN;}
"ADD1" {yylval.bit16 = OP_ADD1; return ARITH_UNI;}
"SUB1" {yylval.bit16 = OP_SUB1; return ARITH_UNI;}
"NEG" {yylval.bit16 = OP_NEG; return ARITH_UNI;}
"XOR" {yylval.bit16 = OP_XOR; return ARITH_BIN;}

"XORI" {yylval.bit16 = OP_XORI; return ARITH_XORI;}
"MUL2" {yylval.bit16 = OP_MUL2; return ARITH_MULDIV;}
"DIV2" {yylval.bit16 = OP_DIV2; return ARITH_MULDIV;}

"BGEZ" {yylval.bit16 = OP_BGEZ; return BRANCH_REGOFF;}
"BLZ" {yylval.bit16 = OP_BLZ; return BRANCH_REGOFF;}
"BEVN" {yylval.bit16 = OP_BEVN; return BRANCH_REGOFF;}
"BODD" {yylval.bit16 = OP_BODD; return BRANCH_REGOFF;}
"BRA" {yylval.bit16 = OP_BRA; return BRANCH_OFF;}
"SWB" {yylval.bit16 = OP_SWB; return BRANCH_REG;}
"RSWB" {yylval.bit16 = OP_RSWB; return BRANCH_REG;}

"EXCH" {yylval.bit16 = OP_EXCH; return EXCHANGE;}

"reg"([0-9]|1[0-5]) {yylval.bit16 = atoi(yytext+3); return REGISTER; }

-?[0-9]+ {yylval.bit16 = atoi(yytext); return IMMEDIATE; }
_[a-zA-Z][a-zA-Z0-9]*_ { yylval.label = yytext; return LABEL;}
[a-zA-Z][a-zA-Z0-9]* { yylval.label = yytext; return REFERENCE;}

"," {return COMMA;}
"\n" {return EOL;}

[ \t\r]+ {}

. {fprintf(stderr, "Lexical Error [l %d]: Character (%s) is undefined \n", yylineno, yytext);}




%%
