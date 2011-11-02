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
#line 200 "y.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 213 "y.tab.c"

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
#define YYLAST   452

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  64
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  45
/* YYNRULES -- Number of rules.  */
#define YYNRULES  116
/* YYNRULES -- Number of states.  */
#define YYNSTATES  193

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   302

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      48,    49,    51,    53,    50,    54,     2,    52,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    59,    61,
      55,    60,    56,    58,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    62,    57,    63,     2,     2,     2,     2,
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
      45,    46,    47
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
     229,   231,   233,   235,   237,   240,   244,   248,   253,   256,
     260,   264,   269,   271,   274,   276,   279,   281,   284,   290,
     302,   312,   322,   330,   336,   340,   343,   346,   349,   353,
     355,   358,   360,   362,   367,   371,   375
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
     106,     0,    -1,     6,    -1,     5,    -1,    48,    83,    49,
      -1,    65,    -1,    66,    48,    49,    -1,    66,    48,    67,
      49,    -1,    81,    -1,    67,    50,    81,    -1,    66,    -1,
      68,    -1,    69,    -1,    70,    51,    69,    -1,    70,    52,
      69,    -1,    70,    -1,    71,    53,    70,    -1,    71,    54,
      70,    -1,    71,    -1,    72,    -1,    73,    55,    72,    -1,
      73,    56,    72,    -1,    73,    16,    72,    -1,    73,    17,
      72,    -1,    73,    -1,    74,    18,    73,    -1,    74,    19,
      73,    -1,    74,    -1,    75,    -1,    76,    -1,    77,    57,
      76,    -1,    77,    -1,    78,    22,    77,    -1,    78,    -1,
      79,    23,    78,    -1,    79,    -1,    79,    58,    83,    59,
      80,    -1,    80,    -1,    68,    82,    81,    -1,    60,    -1,
      28,    -1,    29,    -1,    30,    -1,    31,    -1,    24,    -1,
      25,    -1,    26,    -1,    27,    -1,    32,    -1,    33,    -1,
      81,    -1,    83,    50,    81,    -1,    85,    61,    -1,    85,
      86,    61,    -1,    88,    -1,    88,    85,    -1,    87,    -1,
      86,    50,    87,    -1,    89,    -1,    89,    60,    95,    -1,
       3,    -1,     4,    -1,    90,    -1,     6,    -1,    48,    89,
      49,    -1,    90,    48,    91,    49,    -1,    90,    48,    94,
      49,    -1,    90,    48,    49,    -1,    92,    -1,    93,    -1,
      92,    50,    93,    -1,    85,    89,    -1,    85,    -1,     6,
      -1,    94,    50,     6,    -1,    81,    -1,    62,    96,    63,
      -1,    95,    -1,    96,    50,    95,    -1,    99,    -1,   102,
      -1,   103,    -1,   104,    -1,   105,    -1,    62,    63,    -1,
      62,   101,    63,    -1,    62,   100,    63,    -1,    62,   100,
     101,    63,    -1,    62,    63,    -1,    62,   101,    63,    -1,
      62,   100,    63,    -1,    62,   100,   101,    63,    -1,    84,
      -1,   100,    84,    -1,    97,    -1,   101,    97,    -1,    61,
      -1,    83,    61,    -1,    42,    48,    83,    49,    97,    -1,
      42,    48,    83,    49,    62,    97,    63,    43,    62,    97,
      63,    -1,    42,    48,    83,    49,    62,    97,    63,    43,
      97,    -1,    42,    48,    83,    49,    97,    43,    62,    97,
      63,    -1,    42,    48,    83,    49,    97,    43,    97,    -1,
      45,    48,    83,    49,    97,    -1,     7,     6,    61,    -1,
       8,    61,    -1,     9,    61,    -1,    10,    61,    -1,    10,
      83,    61,    -1,   107,    -1,   106,   107,    -1,   108,    -1,
      84,    -1,    85,    89,   100,    98,    -1,    85,    89,    98,
      -1,    89,   100,    98,    -1,    89,    98,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    36,    36,    37,    38,    43,    44,    46,    51,    52,
      57,    61,    65,    66,    68,    73,    74,    76,    81,    85,
      86,    88,    90,    92,    97,    98,   100,   105,   109,   113,
     114,   119,   120,   124,   125,   130,   131,   136,   137,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
     156,   157,   162,   164,   169,   170,   175,   176,   181,   182,
     187,   188,   192,   196,   197,   199,   201,   203,   208,   212,
     213,   218,   220,   224,   225,   230,   231,   237,   238,   243,
     244,   245,   246,   247,   251,   253,   255,   257,   262,   264,
     266,   268,   273,   274,   279,   280,   285,   287,   292,   294,
     296,   298,   300,   305,   310,   312,   314,   316,   318,   324,
     325,   330,   331,   335,   337,   339,   341
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INT", "VOID", "CONSTANT", "IDENTIFIER",
  "GOTO", "CONTINUE", "BREAK", "RETURN", "PTR_OP", "INC_OP", "DEC_OP",
  "LEFT_OP", "RIGHT_OP", "LE_OP", "GE_OP", "EQ_OP", "NE_OP",
  "STRING_LITERAL", "SIZEOF", "AND_OP", "OR_OP", "SUB_ASSIGN",
  "LEFT_ASSIGN", "RIGHT_ASSIGN", "AND_ASSIGN", "MUL_ASSIGN", "DIV_ASSIGN",
  "MOD_ASSIGN", "ADD_ASSIGN", "XOR_ASSIGN", "OR_ASSIGN", "TYPE_NAME",
  "TYPEDEF", "EXTERN", "STATIC", "AUTO", "REGISTER", "CASE", "DEFAULT",
  "IF", "ELSE", "SWITCH", "WHILE", "DO", "FOR", "'('", "')'", "','", "'*'",
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
     295,   296,   297,   298,   299,   300,   301,   302,    40,    41,
      44,    42,    47,    43,    45,    60,    62,   124,    63,    58,
      61,    59,   123,   125
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    64,    65,    65,    65,    66,    66,    66,    67,    67,
      68,    69,    70,    70,    70,    71,    71,    71,    72,    73,
      73,    73,    73,    73,    74,    74,    74,    75,    76,    77,
      77,    78,    78,    79,    79,    80,    80,    81,    81,    82,
      82,    82,    82,    82,    82,    82,    82,    82,    82,    82,
      83,    83,    84,    84,    85,    85,    86,    86,    87,    87,
      88,    88,    89,    90,    90,    90,    90,    90,    91,    92,
      92,    93,    93,    94,    94,    95,    95,    96,    96,    97,
      97,    97,    97,    97,    98,    98,    98,    98,    99,    99,
      99,    99,   100,   100,   101,   101,   102,   102,   103,   103,
     103,   103,   103,   104,   105,   105,   105,   105,   105,   106,
     106,   107,   107,   108,   108,   108,   108
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
       1,     1,     1,     1,     2,     3,     3,     4,     2,     3,
       3,     4,     1,     2,     1,     2,     1,     2,     5,    11,
       9,     9,     7,     5,     3,     2,     2,     2,     3,     1,
       2,     1,     1,     4,     3,     3,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    60,    61,    63,     0,   112,     0,    54,     0,    62,
       0,   109,   111,     0,    52,     0,    56,    58,    55,     0,
      92,     0,   116,     0,     0,     1,   110,    64,     0,    53,
       0,   114,     0,     3,     2,     0,     0,     0,     0,     0,
       0,     0,    96,     0,    84,     5,    10,    11,    12,    15,
      18,    19,    24,    27,    28,    29,    31,    33,    35,    37,
      50,     0,    94,    79,     0,     0,    80,    81,    82,    83,
      58,    93,   115,    73,    67,    72,     0,    68,    69,     0,
      57,     0,    75,    59,   113,     0,   105,   106,   107,     0,
       0,     0,     0,    88,     0,     0,     0,    44,    45,    46,
      47,    40,    41,    42,    43,    48,    49,    39,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    97,    86,     0,    85,    95,    71,
      65,     0,    66,     0,    77,     0,   104,   108,     0,     0,
       4,    90,     0,    89,     6,     0,     8,    38,    11,    13,
      14,    16,    17,    22,    23,    20,    21,    25,    26,    30,
      32,    34,     0,    51,    87,    70,    74,     0,    76,     0,
       0,    91,     7,     0,     0,    78,     0,    98,   103,     9,
      36,    94,     0,     0,     0,   102,     0,    94,     0,   100,
     101,    94,    99
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    45,    46,   145,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,   108,    61,
      20,    21,    15,    16,     7,     8,     9,    76,    77,    78,
      79,    83,   135,    62,    22,    63,    94,    95,    66,    67,
      68,    69,    10,    11,    12
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -78
static const yytype_int16 yypact[] =
{
     112,   -78,   -78,   -78,    48,   -78,    56,   130,    27,   -42,
     124,   -78,   -78,    33,   -78,    20,   -78,    24,   -78,    13,
     -78,    56,   -78,    27,    65,   -78,   -78,   -78,    48,   -78,
      47,   -78,    27,   -78,   -78,    79,    49,    68,    51,    40,
      60,   134,   -78,   141,   -78,   -78,    74,   138,   -78,   144,
      82,   -78,    17,   169,   -78,   -78,    69,   109,    42,   -78,
     -78,    37,   -78,   -78,   149,   215,   -78,   -78,   -78,   -78,
      78,   -78,   -78,   -78,   -78,    48,   152,   111,   -78,   143,
     -78,    47,   -78,   -78,   -78,   129,   -78,   -78,   -78,    44,
     134,   134,   150,   -78,   171,   237,   136,   -78,   -78,   -78,
     -78,   -78,   -78,   -78,   -78,   -78,   -78,   -78,   134,   134,
     134,   134,   134,   134,   134,   134,   134,   134,   134,   134,
     134,   134,   134,   134,   -78,   -78,   244,   -78,   -78,   -78,
     -78,   130,   -78,   167,   -78,    14,   -78,   -78,   165,   168,
     -78,   -78,   303,   -78,   -78,   177,   -78,   -78,   -78,   -78,
     -78,   144,   144,   -78,   -78,   -78,   -78,    17,    17,   -78,
      69,   109,    61,   -78,   -78,   -78,   -78,    47,   -78,   310,
     325,   -78,   -78,   134,   134,   -78,   141,   166,   -78,   -78,
     -78,   172,   332,   195,   141,   -78,   390,   176,   141,   -78,
     -78,   178,   -78
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -78,   -78,   -78,   -78,   -72,   119,   125,   -78,    92,   113,
     -78,   -78,   121,   128,   135,   -78,    81,   -30,   -78,   -31,
       3,     1,   -78,   230,   -78,     8,   -78,   -78,   -78,   131,
     -78,   -77,   -78,   -63,    -8,   -78,    84,   -14,   -78,   -78,
     -78,   -78,   -78,   249,   -78
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      82,     6,   128,     5,   134,    65,    24,    89,    18,    31,
      92,     6,    13,     5,    17,    72,     1,     2,    33,    34,
      35,    36,    37,    38,    84,    75,    71,     1,     2,    70,
       1,     2,   128,   113,   114,    71,    70,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   148,
     126,    82,    33,    34,     3,    39,    33,    34,    40,   138,
     139,    41,     3,   128,   167,   121,   146,    71,     1,     2,
      28,    73,   115,   116,    42,    43,    44,   168,   147,   128,
     142,    29,    27,   129,    30,    85,    19,   123,    90,    19,
     175,   162,    23,   163,   123,    41,     4,    71,   124,    41,
     122,    32,   148,    64,     4,   137,   177,   178,    91,    81,
      86,   123,    88,   181,    74,     1,     2,    14,     3,   185,
     174,   187,    96,   189,    25,   191,   119,     1,     2,    87,
       3,   120,    75,     1,     2,   111,   112,    82,    30,    33,
      34,    33,    34,   179,     1,     2,    33,    34,    35,    36,
      37,    38,     1,     2,    33,    34,    35,    36,    37,    38,
       4,   131,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,     4,   166,     1,     2,    33,    34,    35,    36,
      37,    38,    41,    39,    41,   144,    40,   117,   118,    41,
     136,    39,   132,   133,    40,   109,   110,    41,   107,   140,
     123,   130,    42,    43,    93,   153,   154,   155,   156,   182,
      42,    43,   125,    39,   169,   123,    40,   170,   123,    41,
      33,    34,    35,    36,    37,    38,   172,   173,   149,   150,
     157,   158,    42,    43,   141,   183,   151,   152,   186,   190,
     159,   192,    33,    34,    35,    36,    37,    38,   160,    33,
      34,    35,    36,    37,    38,   180,   161,    39,    80,    26,
      40,     0,   165,    41,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    42,    43,   127,    39,
       0,     0,    40,     0,     0,    41,    39,     0,     0,    40,
       0,     0,    41,     0,     0,     0,     0,     0,    42,    43,
     143,     0,     0,     0,     0,    42,    43,   164,    33,    34,
      35,    36,    37,    38,     0,    33,    34,    35,    36,    37,
      38,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      33,    34,    35,    36,    37,    38,     0,    33,    34,    35,
      36,    37,    38,     0,     0,    39,     0,     0,    40,     0,
       0,    41,    39,     0,     0,    40,     0,     0,    41,     0,
       0,     0,     0,     0,    42,    43,   171,    39,     0,     0,
      40,    42,   176,    41,    39,     0,     0,    40,     0,     0,
      41,     0,     0,     0,     0,     0,    42,    43,     0,     0,
       0,     0,     0,    42,   184,    33,    34,    35,    36,    37,
      38,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    39,     0,     0,    40,     0,     0,    41,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    42,   188
};

static const yytype_int16 yycheck[] =
{
      30,     0,    65,     0,    81,    19,    48,    38,     7,    17,
      41,    10,     4,    10,     6,    23,     3,     4,     5,     6,
       7,     8,     9,    10,    32,    24,    23,     3,     4,    21,
       3,     4,    95,    16,    17,    32,    28,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      64,    81,     5,     6,     6,    42,     5,     6,    45,    90,
      91,    48,     6,   126,    50,    23,    96,    64,     3,     4,
      50,     6,    55,    56,    61,    62,    63,    63,   108,   142,
      94,    61,    49,    75,    60,     6,    62,    50,    48,    62,
     167,   122,     8,   123,    50,    48,    48,    94,    61,    48,
      58,    17,   174,    19,    48,    61,   169,   170,    48,    62,
      61,    50,    61,   176,    49,     3,     4,    61,     6,   182,
      59,   184,    48,   186,     0,   188,    57,     3,     4,    61,
       6,    22,   131,     3,     4,    53,    54,   167,    60,     5,
       6,     5,     6,   173,     3,     4,     5,     6,     7,     8,
       9,    10,     3,     4,     5,     6,     7,     8,     9,    10,
      48,    50,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    48,     6,     3,     4,     5,     6,     7,     8,
       9,    10,    48,    42,    48,    49,    45,    18,    19,    48,
      61,    42,    49,    50,    45,    51,    52,    48,    60,    49,
      50,    49,    61,    62,    63,   113,   114,   115,   116,    43,
      61,    62,    63,    42,    49,    50,    45,    49,    50,    48,
       5,     6,     7,     8,     9,    10,    49,    50,   109,   110,
     117,   118,    61,    62,    63,    63,   111,   112,    43,    63,
     119,    63,     5,     6,     7,     8,     9,    10,   120,     5,
       6,     7,     8,     9,    10,   174,   121,    42,    28,    10,
      45,    -1,   131,    48,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    61,    62,    63,    42,
      -1,    -1,    45,    -1,    -1,    48,    42,    -1,    -1,    45,
      -1,    -1,    48,    -1,    -1,    -1,    -1,    -1,    61,    62,
      63,    -1,    -1,    -1,    -1,    61,    62,    63,     5,     6,
       7,     8,     9,    10,    -1,     5,     6,     7,     8,     9,
      10,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       5,     6,     7,     8,     9,    10,    -1,     5,     6,     7,
       8,     9,    10,    -1,    -1,    42,    -1,    -1,    45,    -1,
      -1,    48,    42,    -1,    -1,    45,    -1,    -1,    48,    -1,
      -1,    -1,    -1,    -1,    61,    62,    63,    42,    -1,    -1,
      45,    61,    62,    48,    42,    -1,    -1,    45,    -1,    -1,
      48,    -1,    -1,    -1,    -1,    -1,    61,    62,    -1,    -1,
      -1,    -1,    -1,    61,    62,     5,     6,     7,     8,     9,
      10,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    42,    -1,    -1,    45,    -1,    -1,    48,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    61,    62
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     6,    48,    84,    85,    88,    89,    90,
     106,   107,   108,    89,    61,    86,    87,    89,    85,    62,
      84,    85,    98,   100,    48,     0,   107,    49,    50,    61,
      60,    98,   100,     5,     6,     7,     8,     9,    10,    42,
      45,    48,    61,    62,    63,    65,    66,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    83,    97,    99,   100,   101,   102,   103,   104,   105,
      89,    84,    98,     6,    49,    85,    91,    92,    93,    94,
      87,    62,    81,    95,    98,     6,    61,    61,    61,    83,
      48,    48,    83,    63,   100,   101,    48,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    60,    82,    51,
      52,    53,    54,    16,    17,    55,    56,    18,    19,    57,
      22,    23,    58,    50,    61,    63,   101,    63,    97,    89,
      49,    50,    49,    50,    95,    96,    61,    61,    83,    83,
      49,    63,   101,    63,    49,    67,    81,    81,    68,    69,
      69,    70,    70,    72,    72,    72,    72,    73,    73,    76,
      77,    78,    83,    81,    63,    93,     6,    50,    63,    49,
      49,    63,    49,    50,    59,    95,    62,    97,    97,    81,
      80,    97,    43,    63,    62,    97,    43,    97,    62,    97,
      63,    97,    63
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
    {char buf[2048]; snprintf(buf, sizeof(buf), "(%s)", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 5:
#line 43 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 6:
#line 45 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s()", (yyvsp[(1) - (3)].s)); (yyval.s) = buf;}
    break;

  case 7:
#line 47 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s(%s)", (yyvsp[(1) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 8:
#line 51 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 9:
#line 53 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
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
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s * %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 14:
#line 69 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s / %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 15:
#line 73 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 16:
#line 75 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s + %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 17:
#line 77 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s - %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
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
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s < %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 21:
#line 89 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s > %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 22:
#line 91 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s <= %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 23:
#line 93 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s >= %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 24:
#line 97 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 25:
#line 99 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s == %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 26:
#line 101 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s != %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
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
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s | %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
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
    {char buf[2048]; snprintf(buf, sizeof(buf), "(%s) || (%s)", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 35:
#line 130 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 36:
#line 132 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "(%s) ? (%s) : (%s)", (yyvsp[(1) - (5)].s), (yyvsp[(3) - (5)].s), (yyvsp[(5) - (5)].s)); (yyval.s) = buf;}
    break;

  case 37:
#line 136 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 38:
#line 138 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s = %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 39:
#line 142 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "="); (yyval.s) = buf;}
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
#line 158 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 52:
#line 163 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 53:
#line 165 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s;", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 54:
#line 169 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 55:
#line 171 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 56:
#line 175 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 57:
#line 177 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 58:
#line 181 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 59:
#line 183 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s = %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 60:
#line 187 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 61:
#line 188 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 62:
#line 192 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 63:
#line 196 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 64:
#line 198 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "(%s)", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 65:
#line 200 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s ( %s )", (yyvsp[(1) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 66:
#line 202 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s ( %s )", (yyvsp[(1) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 67:
#line 204 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s()", (yyval.s)); (yyval.s) = buf;}
    break;

  case 68:
#line 208 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 69:
#line 212 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 70:
#line 214 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 71:
#line 219 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 72:
#line 220 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 73:
#line 224 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 74:
#line 226 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s , %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 75:
#line 230 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 76:
#line 232 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "{ %s }", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 77:
#line 237 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 78:
#line 239 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s, %s", (yyvsp[(1) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 79:
#line 243 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 80:
#line 244 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 81:
#line 245 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 82:
#line 246 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 83:
#line 247 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 84:
#line 252 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), ""); (yyval.s) = buf;}
    break;

  case 85:
#line 254 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 86:
#line 256 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 87:
#line 258 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(2) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 88:
#line 263 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "{ }"); (yyval.s) = buf;}
    break;

  case 89:
#line 265 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "{ %s }", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 90:
#line 267 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "{ %s }", (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 91:
#line 269 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "{ %s %s }", (yyvsp[(2) - (4)].s), (yyvsp[(3) - (4)].s)); (yyval.s) = buf;}
    break;

  case 92:
#line 273 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 93:
#line 275 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 94:
#line 279 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 95:
#line 281 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 96:
#line 286 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), ";"); (yyval.s) = buf;}
    break;

  case 97:
#line 288 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 98:
#line 293 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "if ( %s ) then %s;", (yyvsp[(3) - (5)].s), (yyvsp[(5) - (5)].s)); (yyval.s) = buf;}
    break;

  case 99:
#line 295 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "if ( %s ) then {%s} else {%s};", (yyvsp[(3) - (11)].s), (yyvsp[(6) - (11)].s), (yyvsp[(10) - (11)].s)); (yyval.s) = buf; }
    break;

  case 100:
#line 297 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "if ( %s ) then %s else {%s};", (yyvsp[(3) - (9)].s), (yyvsp[(6) - (9)].s), (yyvsp[(9) - (9)].s)); (yyval.s) = buf;}
    break;

  case 101:
#line 299 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "if ( %s ) then {%s} else %s;", (yyvsp[(3) - (9)].s), (yyvsp[(5) - (9)].s), (yyvsp[(8) - (9)].s)); (yyval.s) = buf;}
    break;

  case 102:
#line 301 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "if ( %s ) then {%s} else {%s};", (yyvsp[(3) - (7)].s), (yyvsp[(5) - (7)].s), (yyvsp[(7) - (7)].s)); (yyval.s) = buf; }
    break;

  case 103:
#line 306 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "while ( %s ) do %s;", (yyvsp[(3) - (5)].s), (yyvsp[(5) - (5)].s)); (yyval.s) = buf;}
    break;

  case 104:
#line 311 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s;", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 105:
#line 313 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 106:
#line 315 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 107:
#line 317 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s;", (yyvsp[(1) - (2)].s)); (yyval.s) = buf;}
    break;

  case 108:
#line 319 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s;", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s)); (yyval.s) = buf;}
    break;

  case 109:
#line 324 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s); printf("\n%s\n", (yyval.s));}
    break;

  case 110:
#line 326 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;

  case 111:
#line 330 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 112:
#line 331 "cgram.y"
    {(yyval.s)=(yyvsp[(1) - (1)].s);}
    break;

  case 113:
#line 336 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s %s %s", (yyvsp[(1) - (4)].s), (yyvsp[(2) - (4)].s), (yyvsp[(3) - (4)].s), (yyvsp[(4) - (4)].s)); (yyval.s) = buf;}
    break;

  case 114:
#line 338 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s", (yyvsp[(3) - (3)].s)); (yyval.s) = buf; }
    break;

  case 115:
#line 340 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s %s", (yyvsp[(1) - (3)].s), (yyvsp[(2) - (3)].s), (yyvsp[(3) - (3)].s)); (yyval.s) = buf;}
    break;

  case 116:
#line 342 "cgram.y"
    {char buf[2048]; snprintf(buf, sizeof(buf), "%s %s", (yyvsp[(1) - (2)].s), (yyvsp[(2) - (2)].s)); (yyval.s) = buf;}
    break;


/* Line 1267 of yacc.c.  */
#line 2232 "y.tab.c"
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


#line 345 "cgram.y"

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

