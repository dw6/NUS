%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>	
%}

%union{
	char* s;	
}

%token<s> INT CONSTANT IDENTIFIER GOTO CONTINUE BREAK RETURN
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP STRING_LITERAL SIZEOF
%token AND_OP OR_OP 
%token<s> SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token<s> XOR_ASSIGN OR_ASSIGN TYPE_NAME
%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS
%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR 
%type<s> primary_expression expression assignment_expression unary_expression postfix_expression
%type<s> conditional_expression logical_or_expression logical_and_expression
%type<s> inclusive_or_expression exclusive_or_expression and_expression equality_expression
%type<s> relational_expression shift_expression additive_expression multiplicative_expression
%type<s> cast_expression compound_statement expression_statement assignment_operator
%type<s> selection_statement iteration_statement jump_statement statement 
%type<s> declaration_list statement_list declaration declaration_specifiers init_declarator_list
%type<s> type_specifier init_declarator declarator initializer direct_declarator
%type<s> parameter_type_list parameter_list identifier_list parameter_declaration initializer_list
%type<s> external_declaration translation_unit function_definition argument_expression_list


%start translation_unit  
%%

primary_expression
	: IDENTIFIER {$$=$1;}
	| CONSTANT 	 {$$=$1;}
	| '(' expression ')' 
	  {char buf[128]; snprintf(buf, sizeof(buf), "(%s)", $2); $$ = buf;}
	;

postfix_expression
	: primary_expression {$$=$1;}
	| postfix_expression '(' ')' 
     {char buf[128]; snprintf(buf, sizeof(buf), "%s()", $1); $$ = buf;}
	| postfix_expression '(' argument_expression_list ')'
     {char buf[128]; snprintf(buf, sizeof(buf), "%s(%s)", $1, $3); $$ = buf;}
	;

argument_expression_list
	: assignment_expression {$$=$1;}
	| argument_expression_list ',' assignment_expression
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s, %s", $1, $3); $$ = buf;}
	;

unary_expression
	: postfix_expression {$$=$1;}
	;

cast_expression
	: unary_expression {$$=$1;}
	;

multiplicative_expression
	: cast_expression {$$=$1;}
	| multiplicative_expression '*' cast_expression
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s * %s", $1, $3); $$ = buf;}
	| multiplicative_expression '/' cast_expression
     {char buf[128]; snprintf(buf, sizeof(buf), "%s / %s", $1, $3); $$ = buf;}
	;

additive_expression
	: multiplicative_expression {$$=$1;}
	| additive_expression '+' multiplicative_expression
      {char buf[128]; snprintf(buf, sizeof(buf), "%s + %s", $1, $3); $$ = buf;}
	| additive_expression '-' multiplicative_expression
	  {char buf[128]; snprintf(buf, sizeof(buf), "%s - %s", $1, $3); $$ = buf;}
	;

shift_expression
	: additive_expression {$$=$1;}
	;

relational_expression
	: shift_expression {$$=$1;}
	| relational_expression '<' shift_expression 
      {char buf[128]; snprintf(buf, sizeof(buf), "%s < %s", $1, $3); $$ = buf;}
	| relational_expression '>' shift_expression 
	  {char buf[128]; snprintf(buf, sizeof(buf), "%s > %s", $1, $3); $$ = buf;}
	| relational_expression LE_OP shift_expression
      {char buf[128]; snprintf(buf, sizeof(buf), "%s <= %s", $1, $3); $$ = buf;}
	| relational_expression GE_OP shift_expression
	  {char buf[128]; snprintf(buf, sizeof(buf), "%s >= %s", $1, $3); $$ = buf;}
	;

equality_expression
	: relational_expression {$$=$1;}
	| equality_expression EQ_OP relational_expression
	  {char buf[128]; snprintf(buf, sizeof(buf), "%s == %s", $1, $3); $$ = buf;}
	| equality_expression NE_OP relational_expression
	  {char buf[128]; snprintf(buf, sizeof(buf), "%s != %s", $1, $3); $$ = buf;}
	;

and_expression
	: equality_expression {$$=$1;}
	;

exclusive_or_expression
	: and_expression {$$=$1;}
	;

inclusive_or_expression
	: exclusive_or_expression {$$=$1;}
	| inclusive_or_expression '|' exclusive_or_expression
	{char buf[128]; snprintf(buf, sizeof(buf), "%s | %s", $1, $3); $$ = buf;}
	;

logical_and_expression
	: inclusive_or_expression {$$=$1;}
	| logical_and_expression AND_OP inclusive_or_expression
	;

logical_or_expression
	: logical_and_expression {$$=$1;}
	| logical_or_expression OR_OP logical_and_expression
	  {char buf[128]; snprintf(buf, sizeof(buf), "(%s) || (%s)", $1, $3); $$ = buf;}
	;

conditional_expression
	: logical_or_expression {$$=$1;}
	| logical_or_expression '?' expression ':' conditional_expression
	  {char buf[128]; snprintf(buf, sizeof(buf), "(%s) ? (%s) : (%s)", $1, $3, $5); $$ = buf;}
	;

assignment_expression
	: conditional_expression {$$=$1;}
	| unary_expression assignment_operator assignment_expression  
	  {char buf[128]; snprintf(buf, sizeof(buf), "%s = %s", $1, $3); $$ = buf;}
	;

assignment_operator
	: '=' 	 {char buf[128]; snprintf(buf, sizeof(buf), "="); $$ = buf;}		
	| MUL_ASSIGN {$$=$1;}
	| DIV_ASSIGN {$$=$1;}
	| MOD_ASSIGN {$$=$1;}
	| ADD_ASSIGN {$$=$1;}
	| SUB_ASSIGN  {$$=$1;}
	| LEFT_ASSIGN {$$=$1;}
	| RIGHT_ASSIGN {$$=$1;}
	| AND_ASSIGN {$$=$1;}
	| XOR_ASSIGN {$$=$1;}
	| OR_ASSIGN {$$=$1;}
	;

expression
	: assignment_expression {$$=$1;}
	| expression ',' assignment_expression {$$;}
	;

declaration
	: declaration_specifiers ';'
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s;", $1); $$ = buf;}
	| declaration_specifiers init_declarator_list ';'
     {char buf[128]; snprintf(buf, sizeof(buf), "%s %s;", $1, $2); $$ = buf;}
	;

declaration_specifiers
	: type_specifier {$$=$1;}
	| type_specifier declaration_specifiers
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s %s", $1, $2); $$ = buf;}
	;

init_declarator_list
	: init_declarator {$$=$1;}
	| init_declarator_list ',' init_declarator
	  {char buf[128]; snprintf(buf, sizeof(buf), "%s, %s", $1, $3); $$ = buf;}
	;

init_declarator
	: declarator {$$=$1;}
	| declarator '=' initializer
      {char buf[128]; snprintf(buf, sizeof(buf), "%s = %s", $1, $3); $$ = buf;}
	;

type_specifier
	: INT {$$=$1;}
	;

declarator
	: direct_declarator {$$=$1;}
	;

direct_declarator
	: IDENTIFIER {$$=$1;}
	| '(' declarator ')'
	  {char buf[128]; snprintf(buf, sizeof(buf), "(%s)", $2); $$ = buf;}
	| direct_declarator '(' parameter_type_list ')'
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s ( %s )", $1, $3); $$ = buf;}
	| direct_declarator '(' identifier_list ')'
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s ( %s )", $1, $3); $$ = buf;}
	| direct_declarator '(' ')'
	 {char buf[128]; snprintf(buf, sizeof(buf), "()"); $$ = buf;}
	;

parameter_type_list
	: parameter_list {$$=$1;}
	;

parameter_list
	: parameter_declaration {$$=$1;}
	| parameter_list ',' parameter_declaration
     {char buf[128]; snprintf(buf, sizeof(buf), "%s, %s", $1, $3); $$ = buf;}
	;

parameter_declaration
	: declaration_specifiers declarator
     {char buf[128]; snprintf(buf, sizeof(buf), "%s, %s", $1, $2); $$ = buf;}
	| declaration_specifiers {$$=$1;}
	;

identifier_list
	: IDENTIFIER {$$=$1;}
	| identifier_list ',' IDENTIFIER
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s , %s", $1, $3); $$ = buf;}
	;

initializer
	: assignment_expression {$$=$1;}
	| '{' initializer_list '}'
     {char buf[128]; snprintf(buf, sizeof(buf), "{ %s }", $2); $$ = buf;}

	;

initializer_list
	: initializer {$$=$1;}
	| initializer_list ',' initializer
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s, %s", $1, $3); $$ = buf;}
	;

statement
	: compound_statement {$$=$1;}	
	| expression_statement {$$=$1;}	
	| selection_statement {$$=$1;}	
	| iteration_statement {$$=$1;}	
	| jump_statement {$$=$1;}		
	;

compound_statement
	: '{' '}'
	 {char buf[128]; snprintf(buf, sizeof(buf), "{ }"); $$ = buf;}
	| '{' statement_list '}'
     {char buf[128]; snprintf(buf, sizeof(buf), "{ %s };", $2); $$ = buf;}
	| '{' declaration_list '}'
	  {char buf[128]; snprintf(buf, sizeof(buf), "{ %s };", $2); $$ = buf;}
	| '{' declaration_list statement_list '}'
	  {char buf[128]; snprintf(buf, sizeof(buf), "{ %s %s };", $2, $3); $$ = buf;}
	;

declaration_list
	: declaration {$$=$1;}
	| declaration_list declaration
     {char buf[128]; snprintf(buf, sizeof(buf), "%s %s", $1, $2); $$ = buf;}
	;

statement_list
	: statement {$$=$1;}
	| statement_list statement 
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s %s", $1, $2); $$ = buf;}
	;

expression_statement
	: ';'
	{char buf[128]; snprintf(buf, sizeof(buf), ";"); $$ = buf;}

	| expression ';' 	 
	  {char buf[128]; snprintf(buf, sizeof(buf), "%s;", $1); $$ = buf;}

	;

selection_statement
	: IF '(' expression ')' statement 
     {char buf[128]; snprintf(buf, sizeof(buf), "if ( %s ) then\n{\t %s \n};", $3, $5); $$ = buf; }
	| IF '(' expression ')' statement ELSE statement
	 {char buf[128]; snprintf(buf, sizeof(buf), "if ( %s ) then { %s } else { %s };", $3, $5, $7); $$ = buf;}
	;

iteration_statement
	: WHILE '(' expression ')' statement 
	 {char buf[128]; snprintf(buf, sizeof(buf), "while ( %s ) do { %s };", $3, $5); $$ = buf;}
	;

jump_statement
	: GOTO IDENTIFIER ';' 
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s %s;", $1, $2); $$ = buf;}
	| CONTINUE ';'
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s;", $1); $$ = buf;}
	| BREAK ';'
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s;", $1); $$ = buf;}
	| RETURN ';'
     {char buf[128]; snprintf(buf, sizeof(buf), "%s;", $1); $$ = buf;}
	| RETURN expression ';'
     {char buf[128]; snprintf(buf, sizeof(buf), "%s %s;", $1, $2); $$ = buf;}
	;

translation_unit
	: external_declaration {$$=$1; }
	| translation_unit external_declaration
	  {char buf[128]; snprintf(buf, sizeof(buf), "%s %s", $1, $2); $$ = buf; printf("\n%s\n", $$);}
	;

external_declaration
	: function_definition {$$=$1;}
	| declaration {$$=$1;}
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s %s %s %s", $1, $2, $3, $4); $$ = buf;}
	| declaration_specifiers declarator compound_statement
     {char buf[128]; snprintf(buf, sizeof(buf), "%s %s %s", $1, $2, $3); $$ = buf;}
	| declarator declaration_list compound_statement
	 {char buf[128]; snprintf(buf, sizeof(buf), "%s %s %s", $1, $2, $3); $$ = buf;}
	| declarator compound_statement
     {char buf[128]; snprintf(buf, sizeof(buf), "%s %s", $1, $2); $$ = buf;}

	;

%%
extern char yytext[];
extern int column;

yyerror(s)
char *s;
{
	fflush(stdout);
	printf("\n%*s\n%*s\n", column, "^", column, s);
}

main()
{
  if ( yyparse() ) printf("Rejected\n") ;
  else printf("Accepted\n") ;
}
