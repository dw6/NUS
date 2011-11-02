/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



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




/* Copy the first part of user declarations.  */
#line 1 "cgram.y"

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


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 20 "cgram.y"
{
	char* s;	
}
/* Line 193 of yacc.c.  */
#line 203 "y.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 216 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  25
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   413

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  59
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  45
/* YYNRULES -- Number of rules.  */
#define YYNRULES  115
/* YYNRULES -- Number of states.  */
#define YYNSTATES  192

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   297

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      43,    44,    46,    48,    45,    49,     2,    47,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    54,    56,
      50,    55,    51,    53,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    57,    52,    58,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     7,    11,    13,    17,    22,    24,
      28,    30,    32,    34,    38,    42,    44,    48,    52,    54,
      56,    60,    64,    68,    72,    74,    78,    82,    84,    86,
      88,    92,    94,    98,   100,   104,   106,   112,   114,   118,
     120,   122,   124,   126,   128,   130,   132,   134,   136,   138,
     140,   142,   146,   149,   153,   155,   158,   160,   164,   166,
     170,   172,   174,   176,   178,   182,   187,   192,   196,   198,
     200,   204,   207,   209,   211,   215,   217,   221,   223,   227,
     229,   231,   233,   235,   237,   241,   245,   250,   253,   257,
     261,   266,   268,   271,   273,   276,   278,   281,   287,   299,
     309,   319,   327,   333,   337,   340,   343,   346,   350,   352,
     355,   357,   359,   364,   368,   372
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
     101,     0,    -1,     6,    -1,     5,    -1,    43,    78,    44,
      -1,    60,    -1,    61,    43,    44,    -1,    61,    43,    62,
      44,    -1,    76,    -1,    62,    45,    76,    -1,    61,    -1,
      63,    -1,    64,    -1,    65,    46,    64,    -1,    65,    47,
      64,    -1,    65,    -1,    66,    48,    65,    -1,    66,    49,
      65,    -1,    66,    -1,    67,    -1,    68,    50,    67,    -1,
      68,    51,    67,    -1,    68,    31,    67,    -1,    68,    32,
      67,    -1,    68,    -1,    69,    33,    68,    -1,    69,    34,
      68,    -1,    69,    -1,    70,    -1,    71,    -1,    72,    52,
      71,    -1,    72,    -1,    73,    18,    72,    -1,    73,    -1,
      74,    19,    73,    -1,    74,    -1,    74,    53,    78,    54,
      75,    -1,    75,    -1,    63,    77,    76,    -1,    55,    -1,
      24,    -1,    25,    -1,    26,    -1,    27,    -1,    20,    -1,
      21,    -1,    22,    -1,    23,    -1,    28,    -1,    29,    -1,
      76,    -1,    78,    45,    76,    -1,    80,    56,    -1,    80,
      81,    56,    -1,    83,    -1,    83,    80,    -1,    82,    -1,
      81,    45,    82,    -1,    84,    -1,    84,    55,    90,    -1,
       3,    -1,     4,    -1,    85,    -1,     6,    -1,    43,    84,
      44,    -1,    85,    43,    86,    44,    -1,    85,    43,    89,
      44,    -1,    85,    43,    44,    -1,    87,    -1,    88,    -1,
      87,    45,    88,    -1,    80,    84,    -1,    80,    -1,     6,
      -1,    89,    45,     6,    -1,    76,    -1,    57,    91,    58,
      -1,    90,    -1,    91,    45,    90,    -1,    94,    -1,    97,
      -1,    98,    -1,    99,    -1,   100,    -1,    57,    96,    58,
      -1,    57,    95,    58,    -1,    57,    95,    96,    58,    -1,
      57,    58,    -1,    57,    96,    58,    -1,    57,    95,    58,
      -1,    57,    95,    96,    58,    -1,    79,    -1,    95,    79,
      -1,    92,    -1,    96,    92,    -1,    56,    -1,    78,    56,
      -1,    37,    43,    78,    44,    92,    -1,    37,    43,    78,
      44,    57,    92,    58,    38,    57,    92,    58,    -1,    37,
      43,    78,    44,    57,    92,    58,    38,    92,    -1,    37,
      43,    78,    44,    92,    38,    57,    92,    58,    -1,    37,
      43,    78,    44,    92,    38,    92,    -1,    40,    43,    78,
      44,    92,    -1,     7,     6,    56,    -1,     8,    56,    -1,
       9,    56,    -1,    10,    56,    -1,    10,    78,    56,    -1,
     102,    -1,   101,   102,    -1,   103,    -1,    79,    -1,    80,
      84,    95,    93,    -1,    80,    84,    93,    -1,    84,    95,
      93,    -1,    84,    93,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    48,    48,    49,    50,    55,    56,    58,    63,    64,
      69,    73,    77,    78,    80,    85,    86,    88,    93,    97,
      98,   100,   102,   104,   109,   110,   112,   117,   121,   125,
     126,   131,   132,   136,   137,   142,   143,   148,   149,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     168,   169,   174,   176,   181,   182,   187,   188,   193,   194,
     199,   200,   204,   208,   209,   211,   213,   215,   220,   224,
     225,   230,   232,   236,   237,   242,   243,   249,   250,   255,
     256,   257,   258,   259,   264,   266,   268,   273,   275,   277,
     279,   284,   285,   290,   291,   296,   298,   303,   305,   307,
     309,   311,   316,   321,   323,   325,   327,   329,   335,   336,
     341,   342,   346,   348,   350,   352
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INT", "VOID", "CONSTANT", "IDENTIFIER",
  "GOTO", "CONTINUE", "BREAK", "RETURN", "PTR_OP", "INC_OP", "DEC_OP",
  "LEFT_OP", "RIGHT_OP", "STRING_LITERAL", "SIZEOF", "AND_OP", "OR_OP",
  "SUB_ASSIGN", "LEFT_ASSIGN", "RIGHT_ASSIGN", "AND_ASSIGN", "MUL_ASSIGN",
  "DIV_ASSIGN", "MOD_ASSIGN", "ADD_ASSIGN", "XOR_ASSIGN", "OR_ASSIGN",
  "TYPE_NAME", "LE_OP", "GE_OP", "EQ_OP", "NE_OP", "CASE", "DEFAULT", "IF",
  "ELSE", "SWITCH", "WHILE", "DO", "FOR", "'('", "')'", "','", "'*'",
  "'/'", "'+'", "'-'", "'<'", "'>'", "'|'", "'?'", "':'", "'='", "';'",
  "'{'", "'}'", "$accept", "primary_expression", "postfix_expression",
  "argument_expression_list", "unary_expression", "cast_expression",
  "multiplicative_expression", "additive_expression", "shift_expression",
  "relational_expression", "equality_expression", "and_expression",
  "exclusive_or_expression", "inclusive_or_expression",
  "logical_and_expression", "logical_or_expression",
  "conditional_expression", "assignment_expression", "assignment_operator",
  "expression", "declaration", "declaration_specifiers",
  "init_declarator_list", "init_declarator", "type_specifier",
  "declarator", "direct_declarator", "parameter_type_list",
  "parameter_list", "parameter_declaration", "identifier_list",
  "initializer", "initializer_list", "statement", "function_statement",
  "compound_statement", "declaration_list", "statement_list",
  "expression_statement", "selection_statement", "iteration_statement",
  "jump_statement", "translation_unit", "external_declaration",
  "function_definition", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,    40,    41,    44,    42,    47,    43,    45,
      60,    62,   124,    63,    58,    61,    59,   123,   125
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    59,    60,    60,    60,    61,    61,    61,    62,    62,
      63,    64,    65,    65,    65,    66,    66,    66,    67,    68,
      68,    68,    68,    68,    69,    69,    69,    70,    71,    72,
      72,    73,    73,    74,    74,    75,    75,    76,    76,    77,
      77,    77,    77,    77,    77,    77,    77,    77,    77,    77,
      78,    78,    79,    79,    80,    80,    81,    81,    82,    82,
      83,    83,    84,    85,    85,    85,    85,    85,    86,    87,
      87,    88,    88,    89,    89,    90,    90,    91,    91,    92,
      92,    92,    92,    92,    93,    93,    93,    94,    94,    94,
      94,    95,    95,    96,    96,    97,    97,    98,    98,    98,
      98,    98,    99,   100,   100,   100,   100,   100,   101,   101,
     102,   102,   103,   103,   103,   103
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     3,     1,     3,     4,     1,     3,
       1,     1,     1,     3,     3,     1,     3,     3,     1,     1,
       3,     3,     3,     3,     1,     3,     3,     1,     1,     1,
       3,     1,     3,     1,     3,     1,     5,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     2,     3,     1,     2,     1,     3,     1,     3,
       1,     1,     1,     1,     3,     4,     4,     3,     1,     1,
       3,     2,     1,     1,     3,     1,     3,     1,     3,     1,
       1,     1,     1,     1,     3,     3,     4,     2,     3,     3,
       4,     1,     2,     1,     2,     1,     2,     5,    11,     9,
       9,     7,     5,     3,     2,     2,     2,     3,     1,     2,
       1,     1,     4,     3,     3,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    60,    61,    63,     0,   111,     0,    54,     0,    62,
       0,   108,   110,     0,    52,     0,    56,    58,    55,     0,
      91,     0,   115,     0,     0,     1,   109,    64,     0,    53,
       0,   113,     0,     3,     2,     0,     0,     0,     0,     0,
       0,     0,    95,     0,     5,    10,    11,    12,    15,    18,
      19,    24,    27,    28,    29,    31,    33,    35,    37,    50,
       0,    93,    79,     0,     0,    80,    81,    82,    83,    58,
      92,   114,    73,    67,    72,     0,    68,    69,     0,    57,
       0,    75,    59,   112,     0,   104,   105,   106,     0,     0,
       0,     0,    87,     0,     0,     0,    44,    45,    46,    47,
      40,    41,    42,    43,    48,    49,    39,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    96,    85,     0,    84,    94,    71,    65,
       0,    66,     0,    77,     0,   103,   107,     0,     0,     4,
      89,     0,    88,     6,     0,     8,    38,    11,    13,    14,
      16,    17,    22,    23,    20,    21,    25,    26,    30,    32,
      34,     0,    51,    86,    70,    74,     0,    76,     0,     0,
      90,     7,     0,     0,    78,     0,    97,   102,     9,    36,
      93,     0,     0,     0,   101,     0,    93,     0,    99,   100,
      93,    98
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    44,    45,   144,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,   107,    60,
      20,    21,    15,    16,     7,     8,     9,    75,    76,    77,
      78,    82,   134,    61,    22,    62,    93,    94,    65,    66,
      67,    68,    10,    11,    12
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -77
static const yytype_int16 yypact[] =
{
     123,   -77,   -77,   -77,    50,   -77,    56,    20,    57,    16,
      94,   -77,   -77,    39,   -77,    83,   -77,    27,   -77,   204,
     -77,    56,   -77,    57,    72,   -77,   -77,   -77,    50,   -77,
      48,   -77,    57,   -77,   -77,    58,    52,    54,    66,    77,
      87,    68,   -77,    12,   -77,    89,   147,   -77,   -18,    86,
     -77,   128,   129,   -77,   -77,    43,   143,    14,   -77,   -77,
     137,   -77,   -77,   140,   211,   -77,   -77,   -77,   -77,    60,
     -77,   -77,   -77,   -77,    50,   145,    93,   -77,   120,   -77,
      48,   -77,   -77,   -77,   138,   -77,   -77,   -77,   139,    68,
      68,   142,   -77,   148,   219,    97,   -77,   -77,   -77,   -77,
     -77,   -77,   -77,   -77,   -77,   -77,   -77,    68,    68,    68,
      68,    68,    68,    68,    68,    68,    68,    68,    68,    68,
      68,    68,    68,   -77,   -77,   273,   -77,   -77,   -77,   -77,
      20,   -77,   195,   -77,    59,   -77,   -77,   155,   178,   -77,
     -77,   280,   -77,   -77,   186,   -77,   -77,   -77,   -77,   -77,
     -18,   -18,   -77,   -77,   -77,   -77,   128,   128,   -77,    43,
     143,   -40,   -77,   -77,   -77,   -77,    48,   -77,   334,   341,
     -77,   -77,    68,    68,   -77,    12,   165,   -77,   -77,   -77,
     157,   349,   196,    12,   -77,   356,   181,    12,   -77,   -77,
     182,   -77
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -77,   -77,   -77,   -77,   -72,   124,   125,   -77,   -27,   121,
     -77,   -77,   127,   130,   122,   -77,    70,   -30,   -77,   -32,
       3,     1,   -77,   218,   -77,     6,   -77,   -77,   -77,   133,
     -77,   -76,   -77,   -62,   101,   -77,   173,   -12,   -77,   -77,
     -77,   -77,   -77,   240,   -77
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      81,     6,   127,     5,   133,   122,    88,    64,    18,    91,
      13,     6,    17,     5,   173,     1,     2,    33,    34,    35,
      36,    37,    38,     1,     2,    74,    70,    69,   108,   109,
       1,     2,   127,   120,    69,    70,   147,   147,   147,   147,
     147,   147,   147,   147,   147,   147,   147,   147,   147,    39,
      81,   125,    40,    33,    34,    41,     3,   137,   138,    24,
       1,     2,     3,   127,    84,   145,    70,   121,    42,    43,
      92,    33,    34,    33,    34,     1,     2,   146,    72,   127,
     128,   141,    30,    27,    19,   152,   153,   154,   155,   161,
     174,    41,   162,     4,    25,   118,    70,     1,     2,     4,
       3,   147,    33,    34,   166,    80,   176,   177,    85,    41,
      86,    41,    14,   180,    19,    30,    73,   167,    31,   184,
      89,   186,    87,   188,    71,   190,     1,     2,    28,     3,
      90,    74,    95,    83,   110,   111,    81,     4,   130,    29,
      41,   143,   178,     1,     2,    33,    34,    35,    36,    37,
      38,     1,     2,    33,    34,    35,    36,    37,    38,   112,
     113,   119,   116,   117,   131,   132,     4,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,    39,   114,   115,
      40,    23,   122,    41,   122,    39,   139,   122,    40,   129,
      32,    41,    63,   123,   135,   136,    42,    43,   124,   168,
     122,   165,   106,   181,    42,    43,   140,     1,     2,    33,
      34,    35,    36,    37,    38,   182,    33,    34,    35,    36,
      37,    38,   169,   122,    33,    34,    35,    36,    37,    38,
     171,   172,   148,   149,   185,   150,   151,   156,   157,   189,
     191,    39,   160,   179,    40,   158,    79,    41,    39,   159,
      26,    40,     0,     0,    41,     0,    39,     0,     0,    40,
      42,    43,    41,   164,     0,     0,     0,    42,    43,   126,
       0,     0,     0,     0,     0,    42,    43,   142,    33,    34,
      35,    36,    37,    38,     0,    33,    34,    35,    36,    37,
      38,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      39,     0,     0,    40,     0,     0,    41,    39,     0,     0,
      40,     0,     0,    41,     0,     0,     0,     0,     0,    42,
      43,   163,     0,     0,     0,     0,    42,    43,   170,    33,
      34,    35,    36,    37,    38,     0,    33,    34,    35,    36,
      37,    38,     0,     0,    33,    34,    35,    36,    37,    38,
       0,    33,    34,    35,    36,    37,    38,     0,     0,     0,
       0,    39,     0,     0,    40,     0,     0,    41,    39,     0,
       0,    40,     0,     0,    41,     0,    39,     0,     0,    40,
      42,   175,    41,    39,     0,     0,    40,    42,    43,    41,
       0,     0,     0,     0,     0,    42,   183,     0,     0,     0,
       0,     0,    42,   187
};

static const yytype_int16 yycheck[] =
{
      30,     0,    64,     0,    80,    45,    38,    19,     7,    41,
       4,    10,     6,    10,    54,     3,     4,     5,     6,     7,
       8,     9,    10,     3,     4,    24,    23,    21,    46,    47,
       3,     4,    94,    19,    28,    32,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    37,
      80,    63,    40,     5,     6,    43,     6,    89,    90,    43,
       3,     4,     6,   125,     6,    95,    63,    53,    56,    57,
      58,     5,     6,     5,     6,     3,     4,   107,     6,   141,
      74,    93,    55,    44,    57,   112,   113,   114,   115,   121,
     166,    43,   122,    43,     0,    52,    93,     3,     4,    43,
       6,   173,     5,     6,    45,    57,   168,   169,    56,    43,
      56,    43,    56,   175,    57,    55,    44,    58,    17,   181,
      43,   183,    56,   185,    23,   187,     3,     4,    45,     6,
      43,   130,    43,    32,    48,    49,   166,    43,    45,    56,
      43,    44,   172,     3,     4,     5,     6,     7,     8,     9,
      10,     3,     4,     5,     6,     7,     8,     9,    10,    31,
      32,    18,    33,    34,    44,    45,    43,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    37,    50,    51,
      40,     8,    45,    43,    45,    37,    44,    45,    40,    44,
      17,    43,    19,    56,    56,    56,    56,    57,    58,    44,
      45,     6,    55,    38,    56,    57,    58,     3,     4,     5,
       6,     7,     8,     9,    10,    58,     5,     6,     7,     8,
       9,    10,    44,    45,     5,     6,     7,     8,     9,    10,
      44,    45,   108,   109,    38,   110,   111,   116,   117,    58,
      58,    37,   120,   173,    40,   118,    28,    43,    37,   119,
      10,    40,    -1,    -1,    43,    -1,    37,    -1,    -1,    40,
      56,    57,    43,   130,    -1,    -1,    -1,    56,    57,    58,
      -1,    -1,    -1,    -1,    -1,    56,    57,    58,     5,     6,
       7,     8,     9,    10,    -1,     5,     6,     7,     8,     9,
      10,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    -1,    -1,    40,    -1,    -1,    43,    37,    -1,    -1,
      40,    -1,    -1,    43,    -1,    -1,    -1,    -1,    -1,    56,
      57,    58,    -1,    -1,    -1,    -1,    56,    57,    58,     5,
       6,     7,     8,     9,    10,    -1,     5,     6,     7,     8,
       9,    10,    -1,    -1,     5,     6,     7,     8,     9,    10,
      -1,     5,     6,     7,     8,     9,    10,    -1,    -1,    -1,
      -1,    37,    -1,    -1,    40,    -1,    -1,    43,    37,    -1,
      -1,    40,    -1,    -1,    43,    -1,    37,    -1,    -1,    40,
      56,    57,    43,    37,    -1,    -1,    40,    56,    57,    43,
      -1,    -1,    -1,    -1,    -1,    56,    57,    -1,    -1,    -1,
      -1,    -1,    56,    57
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     6,    43,    79,    80,    83,    84,    85,
     101,   102,   103,    84,    56,    81,    82,    84,    80,    57,
      79,    80,    93,    95,    43,     0,   102,    44,    45,    56,
      55,    93,    95,     5,     6,     7,     8,     9,    10,    37,
      40,    43,    56,    57,    60,    61,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
      78,    92,    94,    95,    96,    97,    98,    99,   100,    84,
      79,    93,     6,    44,    80,    86,    87,    88,    89,    82,
      57,    76,    90,    93,     6,    56,    56,    56,    78,    43,
      43,    78,    58,    95,    96,    43,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    55,    77,    46,    47,
      48,    49,    31,    32,    50,    51,    33,    34,    52,    18,
      19,    53,    45,    56,    58,    96,    58,    92,    84,    44,
      45,    44,    45,    90,    91,    56,    56,    78,    78,    44,
      58,    96,    58,    44,    62,    76,    76,    63,    64,    64,
      65,    65,    67,    67,    67,    67,    68,    68,    71,    72,
      73,    78,    76,    58,    88,     6,    45,    58,    44,    44,
      58,    44,    45,    54,    90,    57,    92,    92,    76,    75,
      92,    38,    58,    57,    92,    38,    92,    57,    92,    58,
      92,    58
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 48 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 3:
#line 49 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 4:
#line 51 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "(%s)", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 5:
#line 55 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 6:
#line 57 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s()", (yyvsp[(1) - (3)].s)); (yyval.s) = buf;}
    break;

  case 7:
#line 59 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s(%s)", (yyvsp[(1) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 8:
#line 63 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 9:
#line 65 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 10:
#line 69 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 11:
#line 73 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 12:
#line 77 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 13:
#line 79 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s * %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 14:
#line 81 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s / %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 15:
#line 85 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 16:
#line 87 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s + %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 17:
#line 89 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s - %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 18:
#line 93 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 19:
#line 97 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 20:
#line 99 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s < %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 21:
#line 101 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s > %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 22:
#line 103 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s <= %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 23:
#line 105 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s >= %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 24:
#line 109 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 25:
#line 111 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s == %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 26:
#line 113 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s != %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 27:
#line 117 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 28:
#line 121 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 29:
#line 125 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 30:
#line 127 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s | %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 31:
#line 131 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 33:
#line 136 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 34:
#line 138 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "(%s) || (%s)", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 35:
#line 142 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 36:
#line 144 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "(%s) ? (%s) : (%s)", (yyvsp[(1) - (5)].s), (yyvsp[(3) - (5)].s), (yyvsp[(5) - (5)].s)); (yyval.s) = buf;}
    break;

  case 37:
#line 148 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 38:
#line 150 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s = %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 39:
#line 154 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "="); (yyval.s) = buf;}
    break;

  case 40:
#line 155 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 41:
#line 156 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 42:
#line 157 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 43:
#line 158 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 44:
#line 159 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 45:
#line 160 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 46:
#line 161 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 47:
#line 162 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 48:
#line 163 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 49:
#line 164 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 50:
#line 168 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 51:
#line 170 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 52:
#line 175 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 53:
#line 177 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s;", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 54:
#line 181 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 55:
#line 183 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 56:
#line 187 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 57:
#line 189 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 58:
#line 193 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 59:
#line 195 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s = %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 60:
#line 199 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 61:
#line 200 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 62:
#line 204 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 63:
#line 208 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 64:
#line 210 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "(%s)", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 65:
#line 212 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s ( %s )", (yyvsp[(1) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 66:
#line 214 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s ( %s )", (yyvsp[(1) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 67:
#line 216 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s()", (yyval.s)); (yyval.s) = buf;}
    break;

  case 68:
#line 220 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 69:
#line 224 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 70:
#line 226 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 71:
#line 231 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 72:
#line 232 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 73:
#line 236 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 74:
#line 238 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s , %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 75:
#line 242 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 76:
#line 244 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "{ %s }", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 77:
#line 249 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 78:
#line 251 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 79:
#line 255 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 80:
#line 256 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 81:
#line 257 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 82:
#line 258 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 83:
#line 259 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 84:
#line 265 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 85:
#line 267 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 86:
#line 269 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(2) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 87:
#line 274 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "{ }"); (yyval.s) = buf;}
    break;

  case 88:
#line 276 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "{ %s }", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 89:
#line 278 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "{ %s }", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 90:
#line 280 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "{ %s %s }", (yyvsp[(2) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 91:
#line 284 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 92:
#line 286 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 93:
#line 290 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 94:
#line 292 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 95:
#line 297 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), ";"); (yyval.s) = buf;}
    break;

  case 96:
#line 299 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 97:
#line 304 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "if ( %s ) then %s;", (yyvsp[(3) - (5)].s), (yyvsp[(5) - (5)].s)); (yyval.s) = buf;}
    break;

  case 98:
#line 306 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "if ( %s ) then {%s} else {%s};", (yyvsp[(3) - (11)].s), (yyvsp[(6) - (11)].s), (yyvsp[(10) - (11)].s)); (yyval.s) = buf; }
    break;

  case 99:
#line 308 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "if ( %s ) then %s else {%s};", (yyvsp[(3) - (9)].s), (yyvsp[(6) - (9)].s), (yyvsp[(9) - (9)].s)); (yyval.s) = buf;}
    break;

  case 100:
#line 310 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "if ( %s ) then {%s} else %s;", (yyvsp[(3) - (9)].s), (yyvsp[(5) - (9)].s), (yyvsp[(8) - (9)].s)); (yyval.s) = buf;}
    break;

  case 101:
#line 312 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "if ( %s ) then {%s} else {%s};", (yyvsp[(3) - (7)].s), (yyvsp[(5) - (7)].s), (yyvsp[(7) - (7)].s)); (yyval.s) = buf; }
    break;

  case 102:
#line 317 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "while ( %s ) do %s;", (yyvsp[(3) - (5)].s), (yyvsp[(5) - (5)].s)); (yyval.s) = buf;}
    break;

  case 103:
#line 322 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s;", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 104:
#line 324 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 105:
#line 326 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 106:
#line 328 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 107:
#line 330 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s;", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 108:
#line 335 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s); printf("\n%s\n", (yyval.s));}
    break;

  case 109:
#line 337 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 110:
#line 341 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 111:
#line 342 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 112:
#line 347 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s %s %s", (yyvsp[(1) - (4)].s), (yyvsp[(2) - (4)].s), (yyvsp[(3) - (4)].s), (yyvsp[(4) - (4)].s)); (yyval.s) = buf;}
    break;

  case 113:
#line 349 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s", (yyvsp[(3) - (3)].s)); (yyval.s) = buf; }
    break;

  case 114:
#line 351 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s %s", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 115:
#line 353 "cgram.y"
    {char buf[1024]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;


/* Line 1267 of yacc.c.  */
#line 2219 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 356 "cgram.y"

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

