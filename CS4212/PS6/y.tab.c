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
     CONSTANT = 259,
     IDENTIFIER = 260,
     GOTO = 261,
     CONTINUE = 262,
     BREAK = 263,
     RETURN = 264,
     PTR_OP = 265,
     INC_OP = 266,
     DEC_OP = 267,
     LEFT_OP = 268,
     RIGHT_OP = 269,
     LE_OP = 270,
     GE_OP = 271,
     EQ_OP = 272,
     NE_OP = 273,
     STRING_LITERAL = 274,
     SIZEOF = 275,
     AND_OP = 276,
     OR_OP = 277,
     SUB_ASSIGN = 278,
     LEFT_ASSIGN = 279,
     RIGHT_ASSIGN = 280,
     AND_ASSIGN = 281,
     MUL_ASSIGN = 282,
     DIV_ASSIGN = 283,
     MOD_ASSIGN = 284,
     ADD_ASSIGN = 285,
     XOR_ASSIGN = 286,
     OR_ASSIGN = 287,
     TYPE_NAME = 288,
     TYPEDEF = 289,
     EXTERN = 290,
     STATIC = 291,
     AUTO = 292,
     REGISTER = 293,
     CHAR = 294,
     SHORT = 295,
     LONG = 296,
     SIGNED = 297,
     UNSIGNED = 298,
     FLOAT = 299,
     DOUBLE = 300,
     CONST = 301,
     VOLATILE = 302,
     VOID = 303,
     STRUCT = 304,
     UNION = 305,
     ENUM = 306,
     ELLIPSIS = 307,
     CASE = 308,
     DEFAULT = 309,
     IF = 310,
     ELSE = 311,
     SWITCH = 312,
     WHILE = 313,
     DO = 314,
     FOR = 315
   };
#endif
/* Tokens.  */
#define INT 258
#define CONSTANT 259
#define IDENTIFIER 260
#define GOTO 261
#define CONTINUE 262
#define BREAK 263
#define RETURN 264
#define PTR_OP 265
#define INC_OP 266
#define DEC_OP 267
#define LEFT_OP 268
#define RIGHT_OP 269
#define LE_OP 270
#define GE_OP 271
#define EQ_OP 272
#define NE_OP 273
#define STRING_LITERAL 274
#define SIZEOF 275
#define AND_OP 276
#define OR_OP 277
#define SUB_ASSIGN 278
#define LEFT_ASSIGN 279
#define RIGHT_ASSIGN 280
#define AND_ASSIGN 281
#define MUL_ASSIGN 282
#define DIV_ASSIGN 283
#define MOD_ASSIGN 284
#define ADD_ASSIGN 285
#define XOR_ASSIGN 286
#define OR_ASSIGN 287
#define TYPE_NAME 288
#define TYPEDEF 289
#define EXTERN 290
#define STATIC 291
#define AUTO 292
#define REGISTER 293
#define CHAR 294
#define SHORT 295
#define LONG 296
#define SIGNED 297
#define UNSIGNED 298
#define FLOAT 299
#define DOUBLE 300
#define CONST 301
#define VOLATILE 302
#define VOID 303
#define STRUCT 304
#define UNION 305
#define ENUM 306
#define ELLIPSIS 307
#define CASE 308
#define DEFAULT 309
#define IF 310
#define ELSE 311
#define SWITCH 312
#define WHILE 313
#define DO 314
#define FOR 315




/* Copy the first part of user declarations.  */
#line 1 "cgram.y"

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
#line 7 "cgram.y"
{
	char* s;	
}
/* Line 193 of yacc.c.  */
#line 226 "y.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 239 "y.tab.c"

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
#define YYFINAL  24
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   257

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  77
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  44
/* YYNRULES -- Number of rules.  */
#define YYNRULES  108
/* YYNRULES -- Number of states.  */
#define YYNSTATES  173

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   315

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      61,    62,    64,    66,    63,    67,     2,    65,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    72,    74,
      68,    73,    69,    71,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    75,    70,    76,     2,     2,     2,     2,
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
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60
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
     170,   172,   174,   176,   180,   185,   190,   194,   196,   198,
     202,   205,   207,   209,   213,   215,   219,   221,   225,   227,
     229,   231,   233,   235,   238,   242,   246,   251,   253,   256,
     258,   261,   263,   266,   272,   280,   286,   290,   293,   296,
     299,   303,   305,   308,   310,   312,   317,   321,   325
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
     118,     0,    -1,     5,    -1,     4,    -1,    61,    96,    62,
      -1,    78,    -1,    79,    61,    62,    -1,    79,    61,    80,
      62,    -1,    94,    -1,    80,    63,    94,    -1,    79,    -1,
      81,    -1,    82,    -1,    83,    64,    82,    -1,    83,    65,
      82,    -1,    83,    -1,    84,    66,    83,    -1,    84,    67,
      83,    -1,    84,    -1,    85,    -1,    86,    68,    85,    -1,
      86,    69,    85,    -1,    86,    15,    85,    -1,    86,    16,
      85,    -1,    86,    -1,    87,    17,    86,    -1,    87,    18,
      86,    -1,    87,    -1,    88,    -1,    89,    -1,    90,    70,
      89,    -1,    90,    -1,    91,    21,    90,    -1,    91,    -1,
      92,    22,    91,    -1,    92,    -1,    92,    71,    96,    72,
      93,    -1,    93,    -1,    81,    95,    94,    -1,    73,    -1,
      27,    -1,    28,    -1,    29,    -1,    30,    -1,    23,    -1,
      24,    -1,    25,    -1,    26,    -1,    31,    -1,    32,    -1,
      94,    -1,    96,    63,    94,    -1,    98,    74,    -1,    98,
      99,    74,    -1,   101,    -1,   101,    98,    -1,   100,    -1,
      99,    63,   100,    -1,   102,    -1,   102,    73,   108,    -1,
       3,    -1,   103,    -1,     5,    -1,    61,   102,    62,    -1,
     103,    61,   104,    62,    -1,   103,    61,   107,    62,    -1,
     103,    61,    62,    -1,   105,    -1,   106,    -1,   105,    63,
     106,    -1,    98,   102,    -1,    98,    -1,     5,    -1,   107,
      63,     5,    -1,    94,    -1,    75,   109,    76,    -1,   108,
      -1,   109,    63,   108,    -1,   111,    -1,   114,    -1,   115,
      -1,   116,    -1,   117,    -1,    75,    76,    -1,    75,   113,
      76,    -1,    75,   112,    76,    -1,    75,   112,   113,    76,
      -1,    97,    -1,   112,    97,    -1,   110,    -1,   113,   110,
      -1,    74,    -1,    96,    74,    -1,    55,    61,    96,    62,
     110,    -1,    55,    61,    96,    62,   110,    56,   110,    -1,
      58,    61,    96,    62,   110,    -1,     6,     5,    74,    -1,
       7,    74,    -1,     8,    74,    -1,     9,    74,    -1,     9,
      96,    74,    -1,   119,    -1,   118,   119,    -1,   120,    -1,
      97,    -1,    98,   102,   112,   111,    -1,    98,   102,   111,
      -1,   102,   112,   111,    -1,   102,   111,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    36,    36,    37,    38,    43,    44,    46,    51,    52,
      57,    61,    65,    66,    68,    73,    74,    76,    81,    85,
      86,    88,    90,    92,    97,    98,   100,   105,   109,   113,
     114,   119,   120,   124,   125,   130,   131,   136,   137,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
     156,   157,   161,   163,   168,   169,   174,   175,   180,   181,
     186,   190,   194,   195,   197,   199,   201,   206,   210,   211,
     216,   218,   222,   223,   228,   229,   235,   236,   241,   242,
     243,   244,   245,   249,   251,   253,   255,   260,   261,   266,
     267,   272,   275,   281,   283,   288,   293,   295,   297,   299,
     301,   306,   307,   312,   313,   317,   319,   321,   323
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INT", "CONSTANT", "IDENTIFIER", "GOTO",
  "CONTINUE", "BREAK", "RETURN", "PTR_OP", "INC_OP", "DEC_OP", "LEFT_OP",
  "RIGHT_OP", "LE_OP", "GE_OP", "EQ_OP", "NE_OP", "STRING_LITERAL",
  "SIZEOF", "AND_OP", "OR_OP", "SUB_ASSIGN", "LEFT_ASSIGN", "RIGHT_ASSIGN",
  "AND_ASSIGN", "MUL_ASSIGN", "DIV_ASSIGN", "MOD_ASSIGN", "ADD_ASSIGN",
  "XOR_ASSIGN", "OR_ASSIGN", "TYPE_NAME", "TYPEDEF", "EXTERN", "STATIC",
  "AUTO", "REGISTER", "CHAR", "SHORT", "LONG", "SIGNED", "UNSIGNED",
  "FLOAT", "DOUBLE", "CONST", "VOLATILE", "VOID", "STRUCT", "UNION",
  "ENUM", "ELLIPSIS", "CASE", "DEFAULT", "IF", "ELSE", "SWITCH", "WHILE",
  "DO", "FOR", "'('", "')'", "','", "'*'", "'/'", "'+'", "'-'", "'<'",
  "'>'", "'|'", "'?'", "':'", "'='", "';'", "'{'", "'}'", "$accept",
  "primary_expression", "postfix_expression", "argument_expression_list",
  "unary_expression", "cast_expression", "multiplicative_expression",
  "additive_expression", "shift_expression", "relational_expression",
  "equality_expression", "and_expression", "exclusive_or_expression",
  "inclusive_or_expression", "logical_and_expression",
  "logical_or_expression", "conditional_expression",
  "assignment_expression", "assignment_operator", "expression",
  "declaration", "declaration_specifiers", "init_declarator_list",
  "init_declarator", "type_specifier", "declarator", "direct_declarator",
  "parameter_type_list", "parameter_list", "parameter_declaration",
  "identifier_list", "initializer", "initializer_list", "statement",
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
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,    40,    41,    44,    42,    47,    43,    45,    60,    62,
     124,    63,    58,    61,    59,   123,   125
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    77,    78,    78,    78,    79,    79,    79,    80,    80,
      81,    82,    83,    83,    83,    84,    84,    84,    85,    86,
      86,    86,    86,    86,    87,    87,    87,    88,    89,    90,
      90,    91,    91,    92,    92,    93,    93,    94,    94,    95,
      95,    95,    95,    95,    95,    95,    95,    95,    95,    95,
      96,    96,    97,    97,    98,    98,    99,    99,   100,   100,
     101,   102,   103,   103,   103,   103,   103,   104,   105,   105,
     106,   106,   107,   107,   108,   108,   109,   109,   110,   110,
     110,   110,   110,   111,   111,   111,   111,   112,   112,   113,
     113,   114,   114,   115,   115,   116,   117,   117,   117,   117,
     117,   118,   118,   119,   119,   120,   120,   120,   120
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
       1,     1,     1,     3,     4,     4,     3,     1,     1,     3,
       2,     1,     1,     3,     1,     3,     1,     3,     1,     1,
       1,     1,     1,     2,     3,     3,     4,     1,     2,     1,
       2,     1,     2,     5,     7,     5,     3,     2,     2,     2,
       3,     1,     2,     1,     1,     4,     3,     3,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    60,    62,     0,   104,     0,    54,     0,    61,     0,
     101,   103,     0,    52,     0,    56,    58,    55,     0,    87,
       0,   108,     0,     0,     1,   102,    63,     0,    53,     0,
     106,     0,     3,     2,     0,     0,     0,     0,     0,     0,
       0,    91,    83,     5,    10,    11,    12,    15,    18,    19,
      24,    27,    28,    29,    31,    33,    35,    37,    50,     0,
      89,    78,     0,     0,    79,    80,    81,    82,    58,    88,
     107,    72,    66,    71,     0,    67,    68,     0,    57,     0,
      74,    59,   105,     0,    97,    98,    99,     0,     0,     0,
       0,     0,    44,    45,    46,    47,    40,    41,    42,    43,
      48,    49,    39,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    92,
      85,     0,    84,    90,    70,    64,     0,    65,     0,    76,
       0,    96,   100,     0,     0,     4,     6,     0,     8,    38,
      11,    13,    14,    16,    17,    22,    23,    20,    21,    25,
      26,    30,    32,    34,     0,    51,    86,    69,    73,     0,
      75,     0,     0,     7,     0,     0,    77,    93,    95,     9,
      36,     0,    94
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    43,    44,   137,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,   103,    59,
      19,    20,    14,    15,     6,     7,     8,    74,    75,    76,
      77,    81,   130,    60,    61,    22,    63,    64,    65,    66,
      67,     9,    10,    11
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -76
static const yytype_int16 yypact[] =
{
      42,   -76,   -76,     4,   -76,    43,    10,     5,   -38,   126,
     -76,   -76,   -27,   -76,    62,   -76,     2,   -76,    11,   -76,
      43,   -76,     5,    28,   -76,   -76,   -76,     4,   -76,    17,
     -76,     5,   -76,   -76,    38,   -23,   -10,     7,    -4,    12,
     145,   -76,   -76,   -76,    83,   184,   -76,     6,    32,   -76,
      37,   136,   -76,   -76,   108,   111,    22,   -76,   -76,    71,
     -76,   -76,    33,    21,   -76,   -76,   -76,   -76,   106,   -76,
     -76,   -76,   -76,     4,   118,   122,   -76,    76,   -76,    17,
     -76,   -76,   -76,   110,   -76,   -76,   -76,    77,   145,   145,
      79,   143,   -76,   -76,   -76,   -76,   -76,   -76,   -76,   -76,
     -76,   -76,   -76,   145,   145,   145,   145,   145,   145,   145,
     145,   145,   145,   145,   145,   145,   145,   145,   145,   -76,
     -76,   107,   -76,   -76,   -76,   -76,    10,   -76,   198,   -76,
     -17,   -76,   -76,    98,   101,   -76,   -76,   104,   -76,   -76,
     -76,   -76,   -76,     6,     6,   -76,   -76,   -76,   -76,    37,
      37,   -76,   108,   111,    -5,   -76,   -76,   -76,   -76,    17,
     -76,   114,   114,   -76,   145,   145,   -76,   161,   -76,   -76,
     -76,   114,   -76
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -76,   -76,   -76,   -76,    86,    66,    67,   -76,    48,    64,
     -76,   -76,   105,   103,   109,   -76,    55,   -29,   -76,   -34,
     124,     1,   -76,   194,   -76,    29,   -76,   -76,   -76,    96,
     -76,   -75,   -76,   -61,   121,    45,   162,   -76,   -76,   -76,
     -76,   -76,   214,   -76
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      80,     5,   123,    87,   129,     1,    90,    17,     1,     2,
       5,    32,    33,     1,     1,    32,    33,    34,    35,    36,
      37,    32,    33,    23,    73,    32,    33,    34,    35,    36,
      37,     1,    12,    71,    16,    26,     1,    32,    33,    34,
      35,    36,    37,    83,   116,     1,   159,     2,     2,    68,
      80,    84,   108,   109,   133,   134,    68,    88,   118,   160,
     123,    31,   138,    62,    85,     3,    38,   165,    40,    39,
     104,   105,    40,    89,   139,    29,    38,    18,    40,    39,
      18,    86,    40,   154,   166,    41,    18,    42,    38,   155,
      72,    39,    79,   117,    40,    41,    18,   122,   106,   107,
     167,   168,   124,     3,     3,   110,   111,    41,    18,   120,
     172,    32,    33,    34,    35,    36,    37,    13,    32,    33,
      34,    35,    36,    37,     4,    27,    24,    73,    21,     1,
      80,     2,   115,     4,   118,   169,    28,    30,   127,   128,
     118,   135,   118,    70,    91,   119,    69,    32,    33,    32,
      33,   132,    82,   112,   113,    69,   145,   146,   147,   148,
     161,   118,    38,   162,   118,    39,   163,   164,    40,    38,
     141,   142,    39,   143,   144,    40,   149,   150,   114,    29,
     125,    41,    18,   156,   131,   126,    69,     3,    41,    18,
     140,   140,   140,   140,   140,   140,   140,   140,   140,   140,
     140,   140,   140,   158,    40,   136,    40,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   171,   152,   151,
     170,    78,   157,    25,   121,   153,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   140,     0,     0,     0,     0,     0,   102
};

static const yytype_int16 yycheck[] =
{
      29,     0,    63,    37,    79,     3,    40,     6,     3,     5,
       9,     4,     5,     3,     3,     4,     5,     6,     7,     8,
       9,     4,     5,    61,    23,     4,     5,     6,     7,     8,
       9,     3,     3,     5,     5,    62,     3,     4,     5,     6,
       7,     8,     9,     5,    22,     3,    63,     5,     5,    20,
      79,    74,    15,    16,    88,    89,    27,    61,    63,    76,
     121,    16,    91,    18,    74,    61,    55,    72,    61,    58,
      64,    65,    61,    61,   103,    73,    55,    75,    61,    58,
      75,    74,    61,   117,   159,    74,    75,    76,    55,   118,
      62,    58,    75,    71,    61,    74,    75,    76,    66,    67,
     161,   162,    73,    61,    61,    68,    69,    74,    75,    76,
     171,     4,     5,     6,     7,     8,     9,    74,     4,     5,
       6,     7,     8,     9,     0,    63,     0,   126,     7,     3,
     159,     5,    21,     9,    63,   164,    74,    16,    62,    63,
      63,    62,    63,    22,    61,    74,    22,     4,     5,     4,
       5,    74,    31,    17,    18,    31,   108,   109,   110,   111,
      62,    63,    55,    62,    63,    58,    62,    63,    61,    55,
     104,   105,    58,   106,   107,    61,   112,   113,    70,    73,
      62,    74,    75,    76,    74,    63,    62,    61,    74,    75,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,     5,    61,    62,    61,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    56,   115,   114,
     165,    27,   126,     9,    62,   116,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   165,    -1,    -1,    -1,    -1,    -1,    73
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     5,    61,    97,    98,   101,   102,   103,   118,
     119,   120,   102,    74,    99,   100,   102,    98,    75,    97,
      98,   111,   112,    61,     0,   119,    62,    63,    74,    73,
     111,   112,     4,     5,     6,     7,     8,     9,    55,    58,
      61,    74,    76,    78,    79,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    96,
     110,   111,   112,   113,   114,   115,   116,   117,   102,    97,
     111,     5,    62,    98,   104,   105,   106,   107,   100,    75,
      94,   108,   111,     5,    74,    74,    74,    96,    61,    61,
      96,    61,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    73,    95,    64,    65,    66,    67,    15,    16,
      68,    69,    17,    18,    70,    21,    22,    71,    63,    74,
      76,   113,    76,   110,   102,    62,    63,    62,    63,   108,
     109,    74,    74,    96,    96,    62,    62,    80,    94,    94,
      81,    82,    82,    83,    83,    85,    85,    85,    85,    86,
      86,    89,    90,    91,    96,    94,    76,   106,     5,    63,
      76,    62,    62,    62,    63,    72,   108,   110,   110,    94,
      93,    56,   110
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
#line 36 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 3:
#line 37 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 4:
#line 39 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "(%s)", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 5:
#line 43 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 6:
#line 45 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s()", (yyvsp[(1) - (3)].s)); (yyval.s) = buf;}
    break;

  case 7:
#line 47 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s(%s)", (yyvsp[(1) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 8:
#line 51 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 9:
#line 53 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 10:
#line 57 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 11:
#line 61 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 12:
#line 65 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 13:
#line 67 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s * %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 14:
#line 69 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s / %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 15:
#line 73 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 16:
#line 75 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s + %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 17:
#line 77 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s - %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 18:
#line 81 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 19:
#line 85 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 20:
#line 87 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s < %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 21:
#line 89 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s > %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 22:
#line 91 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s <= %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 23:
#line 93 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s >= %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 24:
#line 97 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 25:
#line 99 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s == %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 26:
#line 101 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s != %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 27:
#line 105 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 28:
#line 109 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 29:
#line 113 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 30:
#line 115 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s | %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 31:
#line 119 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 33:
#line 124 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 34:
#line 126 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "(%s) || (%s)", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 35:
#line 130 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 36:
#line 132 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "(%s) ? (%s) : (%s)", (yyvsp[(1) - (5)].s), (yyvsp[(3) - (5)].s), (yyvsp[(5) - (5)].s)); (yyval.s) = buf;}
    break;

  case 37:
#line 136 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 38:
#line 138 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s = %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 39:
#line 142 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "="); (yyval.s) = buf;}
    break;

  case 40:
#line 143 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 41:
#line 144 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 42:
#line 145 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 43:
#line 146 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 44:
#line 147 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 45:
#line 148 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 46:
#line 149 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 47:
#line 150 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 48:
#line 151 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 49:
#line 152 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 50:
#line 156 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 51:
#line 157 "cgram.y"
    {(yyval.s);}
    break;

  case 52:
#line 162 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 53:
#line 164 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s;", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 54:
#line 168 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 55:
#line 170 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 56:
#line 174 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 57:
#line 176 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 58:
#line 180 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 59:
#line 182 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s = %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 60:
#line 186 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 61:
#line 190 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 62:
#line 194 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 63:
#line 196 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "(%s)", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 64:
#line 198 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s ( %s )", (yyvsp[(1) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 65:
#line 200 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s ( %s )", (yyvsp[(1) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 66:
#line 202 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "()"); (yyval.s) = buf;}
    break;

  case 67:
#line 206 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 68:
#line 210 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 69:
#line 212 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 70:
#line 217 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 71:
#line 218 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 72:
#line 222 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 73:
#line 224 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s , %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 74:
#line 228 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 75:
#line 230 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "{ %s }", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 76:
#line 235 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 77:
#line 237 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 78:
#line 241 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 79:
#line 242 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 80:
#line 243 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 81:
#line 244 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 82:
#line 245 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 83:
#line 250 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "{ }"); (yyval.s) = buf;}
    break;

  case 84:
#line 252 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "{ %s };", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 85:
#line 254 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "{ %s };", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 86:
#line 256 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "{ %s %s };", (yyvsp[(2) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 87:
#line 260 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 88:
#line 262 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 89:
#line 266 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 90:
#line 268 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 91:
#line 273 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), ";"); (yyval.s) = buf;}
    break;

  case 92:
#line 276 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 93:
#line 282 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "if ( %s ) then\n{\t %s \n};", (yyvsp[(3) - (5)].s), (yyvsp[(5) - (5)].s)); (yyval.s) = buf; }
    break;

  case 94:
#line 284 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "if ( %s ) then { %s } else { %s };", (yyvsp[(3) - (7)].s), (yyvsp[(5) - (7)].s), (yyvsp[(7) - (7)].s)); (yyval.s) = buf;}
    break;

  case 95:
#line 289 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "while ( %s ) do { %s };", (yyvsp[(3) - (5)].s), (yyvsp[(5) - (5)].s)); (yyval.s) = buf;}
    break;

  case 96:
#line 294 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s;", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 97:
#line 296 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 98:
#line 298 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 99:
#line 300 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 100:
#line 302 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s;", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 101:
#line 306 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s); }
    break;

  case 102:
#line 308 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf; printf("\n%s\n", (yyval.s));}
    break;

  case 103:
#line 312 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 104:
#line 313 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 105:
#line 318 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s %s %s", (yyvsp[(1) - (4)].s), (yyvsp[(2) - (4)].s), (yyvsp[(3) - (4)].s), (yyvsp[(4) - (4)].s)); (yyval.s) = buf;}
    break;

  case 106:
#line 320 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s %s", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 107:
#line 322 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s %s", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 108:
#line 324 "cgram.y"
    {char buf[128]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;


/* Line 1267 of yacc.c.  */
#line 2166 "y.tab.c"
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


#line 328 "cgram.y"

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

