%{
/*
	Benjamin Tan Wei Hao
	U077129N
	
	I used this series of commands to generate the resulting program:

	lex clex.l
	yacc -d cgram.y
	gcc -c lex.yy.c
	gcc -c y.tab.c
	gcc -o cgram lex.yy.o y.tab.o
	
*/
	
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "y.tab.h"

void count();
int check_type();
%}

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*

%%
"/*"					{ comment(); }
"auto"					{ count(); printf("Unimplemented: %s\n", yytext); }
"break"					{ count(); printf("Unimplemented: %s\n", yytext); }
"case"					{ count(); printf("Unimplemented: %s\n", yytext); }
"char"					{ count(); printf("Unimplemented: %s\n", yytext); }
"const"					{ count(); printf("Unimplemented: %s\n", yytext); }
"continue"				{ count(); yylval.s = (char *)strdup(yytext); return(CONTINUE); }
"default"				{ count(); printf("Unimplemented: %s\n", yytext); }
"do"					{ count(); yylval.s = (char *)strdup(yytext); return(DO); }
"double"				{ count(); printf("Unimplemented: %s\n", yytext); }
"else"					{ count(); yylval.s = (char *)strdup(yytext); return(ELSE); }
"enum"					{ count(); printf("Unimplemented: %s\n", yytext); }
"extern"				{ count(); printf("Unimplemented: %s\n", yytext); }
"float"					{ count(); printf("Unimplemented: %s\n", yytext); }
"for"					{ count(); printf("Unimplemented: %s\n", yytext); }
"goto"					{ count(); yylval.s = strdup(yytext); return(GOTO); }
"if"					{ count(); yylval.s = strdup(yytext); return(IF); }
"int"					{ count(); yylval.s = strdup(yytext); return(INT); }
"long"					{ count(); printf("Unimplemented: %s\n", yytext); }
"register"				{ count(); printf("Unimplemented: %s\n", yytext); }
"return"				{ count(); yylval.s = strdup(yytext); return(RETURN); }
"short"					{ count(); printf("Unimplemented: %s\n", yytext); }
"signed"				{ count(); printf("Unimplemented: %s\n", yytext); }
"sizeof"				{ count(); printf("Unimplemented: %s\n", yytext); }
"static"				{ count(); printf("Unimplemented: %s\n", yytext); }
"struct"				{ count(); printf("Unimplemented: %s\n", yytext); }
"switch"				{ count(); printf("Unimplemented: %s\n", yytext); }
"typedef"				{ count(); printf("Unimplemented: %s\n", yytext); }
"union"					{ count(); printf("Unimplemented: %s\n", yytext); }
"unsigned"				{ count(); printf("Unimplemented: %s\n", yytext); }
"void"					{ count(); yylval.s = strdup(yytext); return(VOID); }
"volatile"				{ count(); printf("Unimplemented: %s\n", yytext); }
"while"					{ count(); yylval.s = (char *)strdup(yytext); return(WHILE); }
	
{L}({L}|{D})*			{ count(); yylval.s = (char *)strdup(yytext); return(IDENTIFIER); }
0[xX]{H}+{IS}?			{ count(); printf("Unimplemented: %s\n", yytext); }
0{D}+{IS}?				{ count(); yylval.s = (char *)strdup(yytext);  return(CONSTANT); }
{D}+{IS}?				{ count(); yylval.s = (char *)strdup(yytext);  return(CONSTANT); }
L?'(\\.|[^\\'])+'		{ count(); printf("Unimplemented: %s\n", yytext); }
{D}+{E}{FS}?			{ count(); printf("Unimplemented: %s\n", yytext); }
{D}*"."{D}+({E})?{FS}?	{ count(); printf("Unimplemented: %s\n", yytext); }
{D}+"."{D}*({E})?{FS}?	{ count(); printf("Unimplemented: %s\n", yytext); }

L?\"(\\.|[^\\"])*\"		{ count(); printf("Unimplemented: %s\n", yytext); }

"..."					{ count(); printf("Unimplemented: %s\n", yytext); }
">>="					{ count(); printf("Unimplemented: %s\n", yytext); }
"<<="					{ count(); printf("Unimplemented: %s\n", yytext); }
"+="					{ count(); printf("Unimplemented: %s\n", yytext); }
"-="					{ count(); printf("Unimplemented: %s\n", yytext); }
"*="					{ count(); printf("Unimplemented: %s\n", yytext); }
"/="					{ count(); printf("Unimplemented: %s\n", yytext); }
"%="					{ count(); printf("Unimplemented: %s\n", yytext); }
"&="					{ count(); printf("Unimplemented: %s\n", yytext); }
"^="					{ count(); printf("Unimplemented: %s\n", yytext); }
"|="					{ count(); printf("Unimplemented: %s\n", yytext); }
">>"					{ count(); printf("Unimplemented: %s\n", yytext); }
"<<"					{ count(); printf("Unimplemented: %s\n", yytext); }
"++"					{ count(); printf("Unimplemented: %s\n", yytext); }
"--"					{ count(); printf("Unimplemented: %s\n", yytext); }
"->"					{ count(); printf("Unimplemented: %s\n", yytext); }
"&&"					{ count(); return(AND_OP); }
"||"					{ count(); return(OR_OP); }
"<="					{ count(); return(LE_OP); }
">="					{ count(); return(GE_OP); }
"=="					{ count(); return(EQ_OP); }
"!="					{ count(); return(NE_OP); }
";"						{ count(); return(';'); }
("{"|"<%")				{ count(); return('{'); }
("}"|"%>")				{ count(); return('}'); }
","						{ count(); return(','); }
":"						{ count(); return(':'); }
"="						{ count(); return('='); }
"("						{ count(); return('('); }
")"						{ count(); return(')'); }
("["|"<:")				{ count(); return('['); }
("]"|":>")				{ count(); return(']'); }
"."						{ count(); printf("Unimplemented: %s\n", yytext); }
"&"						{ count(); return('&'); }
"!"						{ count(); return('!'); }
"~"						{ count(); printf("Unimplemented: %s\n", yytext); }
"-"						{ count(); return('-'); }
"+"						{ count(); return('+'); }
"*"						{ count(); return('*'); }
"/"						{ count(); return('/'); }
"%"						{ count(); return('%'); }
"<"						{ count(); return('<'); }
">"						{ count(); return('>'); }
"^"						{ count(); printf("Unimplemented: %s\n", yytext); }
"|"						{ count(); printf("Unimplemented: %s\n", yytext); }
"?"						{ count(); return('?'); }

[ \t\v\n\f]				{ count(); }
.						{ /* ignore bad characters */ }
%%

yywrap()
{
	return(1);
}


comment()
{
	char c, c1;

loop:
	while ((c = input()) != '*' && c != 0)
		putchar(c);

	if ((c1 = input()) != '/' && c != 0)
	{
		unput(c1);
		goto loop;
	}

	if (c != 0)
		putchar(c1);
}


int column = 0;

void count()
{
	int i;

	for (i = 0; yytext[i] != '\0'; i++)
		if (yytext[i] == '\n')
			column = 0;
		else if (yytext[i] == '\t')
			column += 8 - (column % 8);
		else
			column++;

	ECHO;
}


int check_type()
{
	return(IDENTIFIER);
}
