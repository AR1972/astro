;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
** SYMBOLS.H
**
**  This file holds token values for the symbol table in symbols.h
**
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  8/10/89   scottq    written
**  8/13/89   scottq    made new definition of SYMBOL_TABLE_ENTRY
**
*/

/*
 * The following are special tokens with special meanings
 * WARNING! do not alter these values unless you really know whats up
 */
#define TK_EOF     -1    /* End-Of-File token */
#define TK_UNKNOWN -2    /* Lexeme encountered is not in the symbol table */
#define TK_WILD    -3    /* Automatic match token */
#define TK_NOTHING -4    /* Nothing token */
#define TK_LISTHEAD -5   /* variable is head of a list */

/*
 * the following are pre-defined internal keyword tokens.
 * WARNING! do not change these values, as they are indexes into
 * the symbol table in symbol.c--see symbol.c for more info
 */
#define TK_LEFTBRACKET  0
#define TK_RIGHTBRACKET 1
#define TK_EQUALS       2
#define TK_SAVESTATE    3
#define TK_SCREENMODE   4
#define TK_GRAPHICS     5
#define TK_TEXT         6
#define TK_STARTUP      7
#define TK_FILEMGR      8
#define TK_STARTPGRMS   9
#define TK_FILEMGRMODE  10
#define TK_SINGLETREE   11
#define TK_TWOTREE      12
#define TK_SYSTEMTREE   13
#define TK_PROGRAMSTARTER 14
#define TK_HELP         15
#define TK_TITLE        16
#define TK_PROGRAMLIST  17
#define TK_EXPLICITSEL  18
#define TK_GROUP        19
#define TK_PROGRAM      20
#define TK_COMMAND      21
#define TK_LEFTCURLY    22
#define TK_RIGHTCURLY   23
#define TK_ENABLED      24
#define TK_DISABLED     25
#define TK_PASSWORD     26
#define TK_SORTKEY      27
#define TK_NAME         28
#define TK_EXT          29
#define TK_DATE         30
#define TK_SIZE         31
#define TK_DISKORDER    32
#define TK_COMMANDSEP   33
#define TK_VARSENTINEL  34
#define TK_SLASH        35
#define TK_T            36
#define TK_P            37
#define TK_D            38
#define TK_R            39
#define TK_L            40
#define TK_M            41
#define TK_C            42
#define TK_POUND        43
#define TK_AT           44

#define TK_BLACK        45
#define TK_BLUE         46
#define TK_GREEN        47
#define TK_RED          48
#define TK_CYAN         49
#define TK_MAGENTA      50
#define TK_YELLOW       51
#define TK_WHITE        52

#define TK_COLOR        53
#define TK_BASE         54
#define TK_HIGHLIGHT    55
#define TK_ALERT        56
#define TK_SHADOW       57
#define TK_MENU         58
#define TK_MENUTEXT     59
#define TK_DIALOG       60
#define TK_BUTTON       61
#define TK_SELECTION    62
#define TK_BACKGROUND   63
#define TK_FOREGROUND   64

#define TK_LTBLACK      65
#define TK_LTBLUE       66
#define TK_LTGREEN      67
#define TK_LTRED        68
#define TK_LTCYAN       69
#define TK_LTMAGENTA    70
#define TK_LTYELLOW     71
#define TK_LTWHITE      72

#define TK_ASSOCIATIONS 73
#define TK_ASSOCIATION  74

#define TK_SCROLLBAR    75
#define TK_TITLEBAR     76
#define TK_ELEVATOR     77

#define TK_RESOLUTION   78
#define TK_LOWRES       79
#define TK_MEDIUMRES    80
#define TK_HIGHRES      81
#define TK_VERYHIGHRES  82

#define TK_CURRENTCOLOR 83
#define TK_DRIVEBOX     84
#define TK_CURSOR       85
#define TK_SWAPMOUSE    86
#define TK_VIDEODIR     87
#define TK_PROMPT       88
#define TK_INFO         89
#define TK_DEFAULT      90
#define TK_PAUSE        91
#define TK_DIRECTORY    92
#define TK_PARAMETER    93
#define TK_BEEP         94
#define TK_DRIVEICON    95
#define TK_SPECIAL      96
#define TK_BORDERS      97
#define TK_SHARED       98
#define TK_LISTKEYDELAY 99
#define TK_FORCEMONO 100
#define TK_SWITCHING 101
#define TK_TASKLIST 102
#define TK_DCONFIRM 103
#define TK_RCONFIRM 104
#define TK_CROSSDIRSEL 105
#define TK_KBREQUIRED 106
#define TK_XMSREQUIRED 107
#define TK_XMSLIMIT 108
#define TK_ALTTAB 109
#define TK_ALTESC 110
#define TK_CTRLESC 111
#define TK_SHORTCUT 112
#define TK_SHORTCUTCODE 113
#define TK_RESERVETEMP 114
#define TK_MCONFIRM 115
#define TK_SORTORDER 116
#define TK_DISPHIDDENFILES 117
#define TK_DESCENDING 118
#define TK_ASCENDING 119
#define TK_PREVENT 120
#define TK_LOAD 121
#define TK_RUN 122
#define TK_MOUSEINFO 123
#define TK_IGNORE 124
#define TK_READUPDATEFREQ 125
#define TK_TANDY1000 126
#define TK_SWAPDISK 127 		/* M010 */
#define NUMKEYWORDS     128	/* M010 */


/*
 * WARNING WARNING! The MAXSYMBOLS defines the maximum number of
 * symbols which can be allocated!
 */
#define MAXSYMBOLS   2000

/*
 * SYMBOL_TABLE_ENTRY is a template for a symbol in the symbol table
 * Note that lexeme a variable string of bytes--not a pointer!
 *
 * type field is a TOKEN belonging to the SYMBOL_TABLE_ENTRY which
 * has the lexeme field which should be used for this symbol
 * Keywords and non-subclassed symbols will have a type field equal
 * to themselves
 *
 * value field holds a TOKEN value for this symbol--it can be anything
 * really, but is usually another TOKEN in the symbol table
 *
 * next field is TK_NOTHING for atomic symbols, and a TOKEN to the
 * next token in the list if the symbol represents a list of symbols
 *
 * WARNING! SYMBOL_TABLE_ENTRY is really a dynamic length structure
 * with the "lexeme" field being used simply as a name placeholder
 */
typedef struct SYMBOL_TABLE_ENTRY
{
	TOKEN   type;
	TOKEN   value;
	TOKEN   next;
	char    lexeme; /* ...*/

}SYMBOL_TABLE_ENTRY;

/*
 * the symbol table exists in symbols.c
 */
extern SYMBOL_TABLE_ENTRY far *gSymbolTable[MAXSYMBOLS];
extern char far *gTempString;

/*
 * The next macros access fields of a SYMBOL_TABLE_ENTRY
 * Macros should always be used to shield code from symbol table
 * definition changes!
 */
#define Get_Symbol_Type(entry) (entry->type)
#define Set_Symbol_Type(entry,value) entry->type = value

#define Get_Symbol_Value(entry) (entry->value)
#define Set_Symbol_Value(entry,vvalue) entry->value = vvalue

#define Get_Symbol_Next(entry) (entry->next)
#define Set_Symbol_Next(entry,value) entry->next = value


#define Get_Token_Identifier(token) ((token)==TK_NOTHING && gTempString ? gTempString : (char far *)(Get_Symbol_Lexeme(Token_To_Symbol(token))))
/*
 * Since a symbol's lexeme is within the symbols allocated space,
 * we generate the address of the placeholder "lexeme" to generate
 * a pointer to the entire lexeme (which must be null terminated!)
 *
 * Each symbol has a type field which defines the (a TOKEN belonging to)
 * symbol which holds this symbols lexeme name.
 * So here we get the name indirectly by looking at the type's name
 * Keywords and the like have a type equal to their own TOKENs
 */
#define Get_Symbol_Lexeme(entry)   &((Token_To_Symbol(entry->type))->lexeme)

/*
 * extern definitons for routines in symbols.c
 */
TOKEN SymbolAlloc(unsigned int size);
extern int gNumTokens;
#define Get_Num_Tokens() (gNumTokens)
#define Ith_Token(mi) (mi)
#define Token_To_Symbol(mtoken)  (gSymbolTable[mtoken])
#define FreeSymbol(x)   0
#define FreeToken(mtoken)  0
