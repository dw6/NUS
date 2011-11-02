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
     STRING_LITERAL = 271,
     SIZEOF = 272,
     AND_OP = 273,
     OR_OP = 274,
     SUB_ASSIGN = 275,
     LEFT_ASSIGN = 276,
     RIGHT_ASSIGN = 277,
     AND_ASSIGN = 278,
     MUL_ASSIGN = 279,
     DIV_ASSIGN = 280,
     MOD_ASSIGN = 281,
     ADD_ASSIGN = 282,
     XOR_ASSIGN = 283,
     OR_ASSIGN = 284,
     TYPE_NAME = 285,
     LE_OP = 286,
     GE_OP = 287,
     EQ_OP = 288,
     NE_OP = 289,
     CASE = 290,
     DEFAULT = 291,
     IF = 292,
     ELSE = 293,
     SWITCH = 294,
     WHILE = 295,
     DO = 296,
     FOR = 297
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
#define STRING_LITERAL 271
#define SIZEOF 272
#define AND_OP 273
#define OR_OP 274
#define SUB_ASSIGN 275
#define LEFT_ASSIGN 276
#define RIGHT_ASSIGN 277
#define AND_ASSIGN 278
#define MUL_ASSIGN 279
#define DIV_ASSIGN 280
#define MOD_ASSIGN 281
#define ADD_ASSIGN 282
#define XOR_ASSIGN 283
#define OR_ASSIGN 284
#define TYPE_NAME 285
#define LE_OP 286
#define GE_OP 287
#define EQ_OP 288
#define NE_OP 289
#define CASE 290
#define DEFAULT 291
#define IF 292
#define ELSE 293
#define SWITCH 294
#define WHILE 295
#define DO 296
#define FOR 297




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 20 "cgram.y"
{
	char* s;	
}
/* Line 1529 of yacc.c.  */
#line 137 "y.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

