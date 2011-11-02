/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     INT = 258,
     VOID = 259,
     CONSTANT = 260,
     IDENTIFIER = 261,
     GOTO = 262,
     CONTINUE = 263,
     BREAK = 264,
     RETURN = 265,
     PTR_OP = 266,
     INC_OP = 267,
     DEC_OP = 268,
     LEFT_OP = 269,
     RIGHT_OP = 270,
     LE_OP = 271,
     GE_OP = 272,
     EQ_OP = 273,
     NE_OP = 274,
     STRING_LITERAL = 275,
     SIZEOF = 276,
     AND_OP = 277,
     OR_OP = 278,
     SUB_ASSIGN = 279,
     LEFT_ASSIGN = 280,
     RIGHT_ASSIGN = 281,
     AND_ASSIGN = 282,
     MUL_ASSIGN = 283,
     DIV_ASSIGN = 284,
     MOD_ASSIGN = 285,
     ADD_ASSIGN = 286,
     XOR_ASSIGN = 287,
     OR_ASSIGN = 288,
     TYPE_NAME = 289,
     TYPEDEF = 290,
     EXTERN = 291,
     STATIC = 292,
     AUTO = 293,
     REGISTER = 294,
     CASE = 295,
     DEFAULT = 296,
     IF = 297,
     ELSE = 298,
     SWITCH = 299,
     WHILE = 300,
     DO = 301,
     FOR = 302
   };
#endif
/* Tokens.  */
#define INT 258
#define VOID 259
#define CONSTANT 260
#define IDENTIFIER 261
#define GOTO 262
#define CONTINUE 263
#define BREAK 264
#define RETURN 265
#define PTR_OP 266
#define INC_OP 267
#define DEC_OP 268
#define LEFT_OP 269
#define RIGHT_OP 270
#define LE_OP 271
#define GE_OP 272
#define EQ_OP 273
#define NE_OP 274
#define STRING_LITERAL 275
#define SIZEOF 276
#define AND_OP 277
#define OR_OP 278
#define SUB_ASSIGN 279
#define LEFT_ASSIGN 280
#define RIGHT_ASSIGN 281
#define AND_ASSIGN 282
#define MUL_ASSIGN 283
#define DIV_ASSIGN 284
#define MOD_ASSIGN 285
#define ADD_ASSIGN 286
#define XOR_ASSIGN 287
#define OR_ASSIGN 288
#define TYPE_NAME 289
#define TYPEDEF 290
#define EXTERN 291
#define STATIC 292
#define AUTO 293
#define REGISTER 294
#define CASE 295
#define DEFAULT 296
#define IF 297
#define ELSE 298
#define SWITCH 299
#define WHILE 300
#define DO 301
#define FOR 302




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 7 "cgram.y"
{
	char* s;	
}
/* Line 1529 of yacc.c.  */
#line 147 "y.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

