;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
** LEX.C
**
**      This file holds the low-level source for a lexical analyzer.
**
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  8/10/89   scottq    written
**  8/13/89   scottq    re-wrote to compensate for new SYMBOL_TABLE_ENTRY
**                      definition, took out some of the stupidness,added
**                      lots of comments
**
*/

#include <common.h>
#include <assert.h>
#include <dos.h>

//#define HASHHITTEST  0
#define MAXTEXT_IN_K (2)
#define MAXTEXT (MAXTEXT_IN_K << 10)

#ifdef DBCS
unsigned char far *gParseText;  /* global pointer to text currently being parsed     */
#else
char far *gParseText;   /* global pointer to text currently being parsed     */
#endif
unsigned int gOffset;   /* this is the global text cursor in gParseText      */
unsigned int gSize;     /* gSize defines the logical EOF for gParseText      */
int gFileHandle;        /* gFileHandle is the handle to the file being read  */
TOKEN gLookahead;       /* gLookahead is the next symbol in the TOKEN stream */
BOOL gIsSpecial;
int gNumWhiteSpaces; // used to determine how much white space was skipped

char far *gTempString=NULL; /* gTempString is used if symbol table is full   */
int gTempSize = 0;      /* gTempSize is the size of gTempString              */

extern void strfcpy(char far *dst, char far *src) ;
extern void strfncpy(char far *dst, char far *src, unsigned cnt) ;
extern  void far *pascal far LpbAllocWorkFar(unsigned short cb);
extern  void pascal far FreeWorkFar(void far *lpb);
extern void RepeatMove(char far *dest, char far *src, unsigned int cnt);

#ifdef HASHHITTEST
int ghashhits = 0;
int ghashmisses = 0;
int ghashnotpresent = 0;
int gnohashnotpresent = 0;
#endif

extern  unsigned int myfstrlen(unsigned char far *s);

extern BOOL fgSymbolTableModified; /* see iniparse.c */
int lexeme_cmp(char far *src,int length,char far *lexeme);

/* INTERNATIONALIZE HERE!
    Note the following macros which may need to change.
    Be sure to try it out, since this can be kind of touchy.
*/

#define EOS '\0'
#define isEOF() (gOffset >= gSize)
#define isEOL() ((gParseText[gOffset] == '\r') || (gParseText[gOffset]=='\n'))
#define isEos(c) (c == EOS)
#define isQuote() (gParseText[gOffset] == '"')

/*
 * this macro defines characters which should be ignored as
 * equivalent to a space
 */
#ifdef DBCS
#define isWhiteSpace() (gParseText[gOffset] <= ' ')
#else
#define isWhiteSpace() ((unsigned char)gParseText[gOffset] <= ' ')
#endif

/*
 * this macro defines character which are separator characters to the lexer--
 * see "seektoendoflexeme" for effect of separators
 */
#if 0
#define isSeparator() (((gParseText[gOffset] < 'A') && (gParseText[gOffset] > '9')) || \
		       ((gParseText[gOffset] > 'Z') && (gParseText[gOffset] < 'a')) || \
		       ((gParseText[gOffset] > 'z') || (gParseText[gOffset] < '0')))
#endif
#define isSeparator() ((gParseText[gOffset] == '=') || (gParseText[gOffset] == '%') || \
((gParseText[gOffset] > 'z') && (gParseText[gOffset] <= '~')) || ((gParseText[gOffset] >= '/')&&(gParseText[gOffset] <= ':') || (gParseText[gOffset] == '\\')) || \
(gParseText[gOffset] <= ' ') || (gParseText[gOffset] == '[')|| (gParseText[gOffset] == ']'))


/*
 * this macro moves the global text cursor (gOffset) to the first
 * non-white space in the text (or EOF)
 */
#define eatWhiteSpace() while((!isEOF()) && isWhiteSpace()) ++gOffset;

#define PRIMEHASHBYTES 1061

WORD HashTable[PRIMEHASHBYTES] ;

/* Accepts a string and calculates a HASH value based on it. length is the
 * length of the string passed in. If it is -1, it implies that the string
 * is NULL-terminated.
 * This function was stolen from the DRAGON-BOOK! -- fn haspjw()
 */
WORD Hash(char far *lexeme,int length)
{
	int i;
	WORD retval;

	i = 0;
	retval = 2053;
	if(length == -1)
		length = 32000;

	while(lexeme[i] && i < length)
	{
		retval += (lexeme[i]);
		++i;
	}
	return(retval % PRIMEHASHBYTES);
} /* Hash */

/* This function accepts a HASH-index and sets/gets (depending on "setorget")
 * the symbol table index for this HASH-index.
 * The value "SymbolTableInd" is used only when we want to add this index
 * to the Hash Table.
 * Also, if more than 1 lexeme has the
 * same HashInd and this function is called to perform a "set", only
 * the first entry gets into the symbol table!
 */
int Set_Get_HashBin(int HashInd, BOOL setorget,int SymbolTableInd)
{
	int retval;

	assert(HashInd < PRIMEHASHBYTES) ;

	retval = (int) HashTable[HashInd];

	if ((setorget) && (retval == -1))
	   retval = HashTable[HashInd] = SymbolTableInd;

	return(retval);
} /* MarkHashBin */

/* Mark the Bit hash table with only lexemes that are initially present in
 * "symbols.c"
 */
void Init_Symbol_Hash(void)
{
	int i;
	static BOOL fInited = FALSE;

	if(fInited)
		return;
	fInited = TRUE;

	for (i = 0 ; i < PRIMEHASHBYTES ; i++)
		HashTable[i] = -1 ;

	for(i=0;i<Get_Num_Tokens();i++)
    {
		/*
		 * we only check the symbols who own their own lexmes--
		 * symbols which are not a subclass of another symbol
		 */
		if (Get_Symbol_Type(Token_To_Symbol(Ith_Token(i))) == Ith_Token(i))
		{
		   Set_Get_HashBin(Hash(Get_Token_Identifier(Ith_Token(i)),-1), TRUE,Ith_Token(i));
		}
    }

} /* Init_Symbol_Hash */

/*  NAME
 *      New_Symbol
 *  DESCRIPTION
 *      Create a new symbol in the symbol table for the lexeme at
 *  text. The lexeme will include all character in text up to length.
 *  ARGUMENTS
 *      text is a far pointer to a string. The string does not have
 *  to be null terminated.
 *      length is the length of the string at text.
 *  RETURN VALUE
 *      returns a TOKEN belonging to the new symbol--this token is used
 *  to reference the new symbol.
 *  EFFECTS
 *      calls external function SymbolAlloc to allocate far memory for the
 *  new symbol.
 *  STATE
 *  COMMENTS
 *      This is mainly an internal routine used by "lex" when it needs to
 *  create a new symbol.
 *  WARNINGS
 *      Be sure to set the type before getting a pointer to the lexeme, since
 *  Get_Symbol_Lexeme uses the type to determine the actual lexeme.
 *      Length may not be less than 1!
 *  HACKS
 */
TOKEN New_Symbol(char far *text,int length)
{
    SYMBOL_TABLE_ENTRY far *NewEntry; /* temporary pointer to new symbol     */
    TOKEN retval;                     /* return value is the symbol's token   */
    char far *lexeme;                 /* pointer to lexeme inside the symbol */


    /* first allocate the symbol, get a pointer to it */
    retval = SymbolAlloc(sizeof(SYMBOL_TABLE_ENTRY) + length + 2);
    NewEntry = Token_To_Symbol(retval);

    /* if the new symbol allocation succeeded, set its lexeme and initialize
     * it's type, value, and next fields
     */
    if(retval == TK_NOTHING) {
        if(gTempSize <= length) {
            if(gTempString)
                FreeWorkFar(gTempString);
            if(!(gTempString=(char far *)LpbAllocWorkFar(length+1))) {
                gTempSize = 0;
                return(retval);
            }
            gTempSize = length + 1;
        }

        RepeatMove(gTempString, text, length);
        gTempString[length] = EOS;
    } else {
       /* all newly allocated symbols with unique lexemes should have
	* a type equal to itself--type defines the symbol which holds
	* its lexeme's text
	*/
       Set_Symbol_Type(NewEntry,retval);
       /*
	* Value is set to TK_NOTHING to indicate no value initially
	*/
       Set_Symbol_Value(NewEntry,TK_NOTHING);
       /*
	* The Next field should be set to TK_NOTHING as well to indicate
	* that this symbol is not now a list.
	*/
       Set_Symbol_Next(NewEntry,TK_NOTHING);

       /* WARNING: do not get_symbol_lexeme without first setting type! */
       lexeme = Get_Symbol_Lexeme(NewEntry);
        RepeatMove(lexeme, text, length);
	   lexeme[length] = EOS;

	   Set_Get_HashBin(Hash(lexeme,length), TRUE,retval) ;
	}

    /* If we get here, we failed to allocate a new symbol, reflected by
     * retval's value == TK_NOTHING
	*/
    return(retval);
}

/*  NAME
 *      SubClassSymbol
 *  DESCRIPTION
 *      Creates a new new symbol with a type field equal to "type".
 *  Thus, a subclass of the symbol belonging to "type" is created.
 *  ARGUMENTS
 *      type is a TOKEN that belongs to the symbol being subclassed.
 *  RETURN VALUE
 *      a TOKEN belonging to the newly allocated symbol.
 *  EFFECTS
 *  EFFECTS
 *      calls external function SymbolAlloc to allocate far memory for the
 *  new symbol.
 *  STATE
 *      none
 *  COMMENTS
 *      This call is used by external routines to create two(or more)
 *  symbols each with the same lexeme, but different values. Used for
 *  member of lists.
 *  WARNINGS
 *  HACKS
 */
TOKEN SubClassSymbol(TOKEN type)
{
    SYMBOL_TABLE_ENTRY far *NewEntry;   /* temporary pointer to new symbol   */
    TOKEN retval;                       /* return value is the symbol's token */

     /* first allocate the symbol, get a pointer to it */
    retval = SymbolAlloc(sizeof(SYMBOL_TABLE_ENTRY)+2);
    NewEntry = Token_To_Symbol(retval);
    /* if the new symbol allocation succeeded, set its lexeme and initialize
     * it's type, value, and next fields
     */
    if (retval != TK_NOTHING)
    {
       /*
	* Type is set to the value passed in--the new symbol will have
	* lexeme of the type
	*/
       Set_Symbol_Type(NewEntry,type);
       /*
	* Value is set to TK_NOTHING to indicate no value initially
	*/
       Set_Symbol_Value(NewEntry,TK_NOTHING);
       /*
	* The Next field should be set to TK_NOTHING as well to indicate
	* that this symbol is not now a list.
	*/
       Set_Symbol_Next(NewEntry,TK_NOTHING);
    }
    /* If we get here, we failed to allocate a new symbol, reflected by
     * retval's value == TK_NOTHING
	 */
	fgSymbolTableModified = TRUE;
    return(retval);
}

/*  NAME
 *      Append_Symbol
 *  DESCRIPTION
 *      Appends a symbol to a list (or atomic symbol if not already a list).
 *  ARGUMENTS
 *      list is a TOKEN belonging to the list which is being appended to
 *      new is a TOKEN belonging to the symbol which is being appended
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *      This is used by external routines to add a symbol element to a list.
 *  Note that the list may be an atomic element--appending to one of these
 *  turns the atomic element into a list by changing it's next field to
 *  be the "new" token.
 *  WARNINGS
 *  HACKS
 */
void Append_Symbol(TOKEN list,TOKEN new)
{
   SYMBOL_TABLE_ENTRY far *entry;   /* temporary pointer to list symbol   */

   entry = Token_To_Symbol(list);   /* Get a pointer to list's symbol     */
   /*
    * 'seek' to the end of the list--ie, find the first link with a next
    * field of TK_NOTHING
    */
   while( (Get_Symbol_Next(entry) != TK_NOTHING))
   {
       /*
	* set entry to the next of entry--until next is TK_NOTHING
	*/
       entry = Token_To_Symbol(Get_Symbol_Next(entry));
   }
   /*
    * put the "new" symbol onto the end of the list by setting the
    * last element in the list's next field to the new TOKEN
    */
   Set_Symbol_Next(entry,new);
   fgSymbolTableModified = TRUE;
}

/*
 * Recursively duplicate the symbols belonging to a token, and
 * return a new token to that list
 */
TOKEN Duplicate_Token(TOKEN tkSrc)
{
    TOKEN tkSrcVal, tkDst, tkDstNew, tkDstLast;
    SYMBOL_TABLE_ENTRY far *sySrc; 
    SYMBOL_TABLE_ENTRY far *syDst1;

    if((tkDst=SubClassSymbol(tkSrc)) < 0)
	goto Error1;
    tkDstLast = tkDst;

    sySrc = Token_To_Symbol(tkSrc);
    while((tkSrc=Get_Symbol_Next(sySrc)) != TK_NOTHING) {
	sySrc = Token_To_Symbol(tkSrc);
	tkSrcVal = Get_Symbol_Value(sySrc);
	if((tkDstNew=SubClassSymbol(tkSrc)) < 0)
	    return(TK_NOTHING);

	syDst1 = Token_To_Symbol(tkDstNew);
	if(tkSrcVal>0 && Get_List_Length(tkSrcVal) > 1) {
	    Set_Symbol_Value(syDst1, Duplicate_Token(tkSrcVal));
	} else {
	    Set_Symbol_Value(syDst1, tkSrcVal);
	}
	Set_Symbol_Type(syDst1, Get_Symbol_Type(sySrc));

	Append_Symbol(tkDstLast, tkDstNew);
	tkDstLast = tkDstNew;
    }

    fgSymbolTableModified = TRUE;
Error1:
    return(tkDst);
}

void Insert_Symbol(TOKEN list,TOKEN new,int i)
{
   int n;
   SYMBOL_TABLE_ENTRY far *entry;   /* temporary pointer to list symbol   */

   entry = Token_To_Symbol(list);   /* Get a pointer to list's symbol     */
   /*
    * 'seek' to the end of the list--ie, find the first link with a next
    * field of TK_NOTHING
	*/
   for(n=0;n<i;n++)
   {
		if (Get_Symbol_Next(entry) == TK_NOTHING)
			return;
       /*
		* set entry to the next of entry--until next is TK_NOTHING
		*/
       entry = Token_To_Symbol(Get_Symbol_Next(entry));
   }
   /*
    * put the "new" symbol onto the end of the list by setting the
    * last element in the list's next field to the new TOKEN
    */
   Set_Symbol_Next(Token_To_Symbol(new),Get_Symbol_Next(entry));
   Set_Symbol_Next(entry,new);
   fgSymbolTableModified = TRUE;
}


#ifdef ALLOWRELEASE
/*  NAME
 *      FreeList
 *  DESCRIPTION
 *      Frees all symbols in the list
 *  ARGUMENTS
 *      TOKEN to a list
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      frees global memory
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *      This routine does not remove the list from other lists; future
 *  references to list will be bogus!
 *      This routine is recursive!
 *  HACKS
 *      Does not free things with strings in them; should keep track
 *  of reference count so we can do this
 */
void FreeList(TOKEN list)
{
   SYMBOL_TABLE_ENTRY far *entry;   /* temporary pointer to list symbol   */
   SYMBOL_TABLE_ENTRY far *next;   /* temporary pointer to list symbol   */
   TOKEN tokenlast = list;

   if(list > 0)
   {
      entry = Token_To_Symbol(list);   /* Get a pointer to list's symbol     */
      while( (Get_Symbol_Next(entry) > 0))
      {
	  next = entry;
	  tokenlast = Get_Symbol_Next(entry);
	  entry = Token_To_Symbol(tokenlast);
	  FreeList(next);
      }
       /* if type is the same as this token, the the symbols has a string
       that might be referenced somewhere, so don't really delete it*/
	  if(tokenlast == Get_Symbol_Type(entry))
	 fgSymbolTableModified = TRUE;
	 FreeSymbol(entry);
   }
}
#else
#define FreeList(list)
#endif

/*  NAME
 *      Get_Ith_Element
 *  DESCRIPTION
 *      given a list, return the i'th element in the list
 *  ARGUMENTS
 *      list is a TOKEN belonging to the head of a list
 *  RETURN VALUE
 *      TOKEN value of the i'th element in list.
 *      If ith==0 then return list
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *      Used by external functions to access the (effective) linked list
 *  in an array-style fashion.
 *  WARNINGS
 *      List begin with element number 1, not 0!
 *  HACKS
 */
TOKEN Get_Ith_Element(TOKEN list,int ith)
{
      int i;
      SYMBOL_TABLE_ENTRY far *entry; /* temporary pointer to list symbol   */

      if (list <= 0)
	    return(TK_NOTHING);
      if(ith == 0)
	return(list);
      /* get a pointer to the list's symbol */
      entry = Token_To_Symbol(list);
      /* 'seek' to the i'th element */
      for(i=1;i < ith;i++)
      {
	  /* set entry to its next symbol */
	  entry = Token_To_Symbol(Get_Symbol_Next(entry));
	  /* if we reach the end of the list before we reach
	   * the i'th one, return TK_NOTHING
	   */
	  if (Get_Symbol_Next(entry) < 0)
	    return(TK_NOTHING);
      }
      /*
       * We have 'seeked' to the i'th element, which is in
       * next of the temporary 'entry'
       */
      return(Get_Symbol_Next(entry));
}

/*  NAME
 *      Delete_Ith_Entry
 *  DESCRIPTION
 *      Deletes the ith element of a list
 *  ARGUMENTS
 *      TOKEN list is the list, ith is the index of the element to be deleted
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      frees global memory
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
TOKEN Delete_Ith_Element(TOKEN list,int ith)
{
      TOKEN preventry;
      TOKEN thisentry;
      TOKEN nextentry;

      if((list > 0)&&(ith>0))
      {
	 preventry = Get_Ith_Element(list,ith-1);
	 if(preventry > 0)
	 {
	       thisentry = Get_Symbol_Next(Token_To_Symbol(preventry));
	       if(thisentry > 0)
	       {
		  nextentry = Get_Symbol_Next(Token_To_Symbol(thisentry));
		  Set_Symbol_Next(Token_To_Symbol(preventry),nextentry);
		  fgSymbolTableModified = TRUE;
		  FreeList(thisentry);
	       }
	 }
     }
     return(TK_NOTHING);
}


/*  NAME
 *      Get_List_Length
 *  DESCRIPTION
 *      returns the number of elements in the list
 *  ARGUMENTS
 *      list is a TOKEN to a list of symbols
 *  RETURN VALUE
 *      number of elements in the list
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *      This code does an infinite loop which breaks when
 *  the end of a list is found.
 *  HACKS
 */
int Get_List_Length(TOKEN list)
{
      int i = 0;
      SYMBOL_TABLE_ENTRY far *entry;  /* temporary pointer to list symbol   */

       /* get a pointer to the list's symbol */
      if((entry = Token_To_Symbol(list)) < 0)
				return(0);

		if(Get_Symbol_Next(entry) < 0) // no next
				return(0);

      do
      {
			++i;
			/* traverse the list until we get to the end--next is TK_NOTHING
		 * return how many times we went through this loop
		 */
			if (entry < 0)
				return 0 ;
			entry = Token_To_Symbol(Get_Symbol_Next(entry));
			if (Get_Symbol_Next(entry) == TK_NOTHING)
			return(i);
      }
      while(TRUE);  /* go forever!!! */

}

extern char *NullString;
/*  NAME
 *       Init_ParseText
 *  DESCRIPTION
 *       Sets up global structure for parsing text
 *  ARGUMENTS
 *       size is the length of the text
 *       text is a far point to the beginning of the text to be parsed
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      changes globals used by parsing routines
 *  STATE
 *  COMMENTS
 *      gOffset is a cursor used to indicate how far we have
 *      scanned into the text
 *  WARNINGS
 *      Since the parsing routines use these globals, the parse code
 *      cannot be re-entrant!
 *  HACKS
 */
void Init_ParseText(int fHandle, char far *text,
        unsigned int offset, unsigned int size)
{
    gFileHandle = fHandle;
    gParseText = text;
    gOffset = offset;
    gSize   = size;

    Init_Symbol_Hash() ;
}

#if 0
/*  NAME
 *      Get_Token_Identifier
 *  DESCRIPTION
 *      Given a token, returns the lexeme associated with it.
 *  ARGUMENTS
 *      token in question
 *  RETURN VALUE
 *      far pointer to the lexeme text (null terminated)
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *      The grunt work is done in external routine 'Get_Symbol_Lexeme'
 *  WARNINGS
 *      Does not check to see if token is really a list! Lists return
 *  undefined strings!
 *  HACKS
 */
char far * Get_Token_Identifier(TOKEN token)
{
	if((token >=0) && (token <Get_Num_Tokens()))
	{
	   return(Get_Symbol_Lexeme(Token_To_Symbol(token)));
	}
	Beep();
	return(NullString);
}
#endif
extern BOOL fIsSpecialToken(TOKEN section, TOKEN tok) ;

/*  NAME
 *      SetGet_KeyWord_Assignment
 *  DESCRIPTION
 *      returns the value assignment to a token in list section;
 *      will set the value if set is TRUE, returns same.
 *      if section is TK_NOTHING, returns global assignment of the variable
 *      subclass.
 *  ARGUMENTS
 *      section is a token to a list of symbols to look for "variable" elements
 *      variable is the subclass (see SubClassSymbol) of in section we are
 *      looking for
 *  RETURN VALUE
 *      the value of the variable symbol in section list
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *      This is a common engine to several calls which manipulate the
 *  value of symbols
 *  WARNINGS
 *      This code has an infinite loop in it.
 *  HACKS
 */
TOKEN SetGet_KeyWord_Assignment(TOKEN section,TOKEN variable,TOKEN value,int set)
{
    SYMBOL_TABLE_ENTRY far *entry; /* temporary pointer to symbols */
    SYMBOL_TABLE_ENTRY far *preventry; /* temporary pointer to symbols */
    TOKEN newsym;
    /*
     * if we are looking for the value of a subclassed variable
     */
    if ((section != TK_NOTHING))
    {
	  /* get a pointer to the section's list head */
	  preventry = entry = Token_To_Symbol(section);
	  /*
	   * walk the list until we find an element with the
	   * same type as what we are looking for--return it's value
	   */
	  do
	  {
	      if (Get_Symbol_Type(entry) == variable)
	      {
		  /*
		   * we have found the right element, now set the
		   * value if appropriate
		   */
		  if (set)
		  {
			 if(value==TK_UNKNOWN && preventry!=entry) {
/* Remove the symbol from the list */
				Set_Symbol_Next(preventry, Get_Symbol_Next(entry));
				fgSymbolTableModified = TRUE;
			 } else if(Get_Symbol_Value(entry) != value)
			 {
				/* If variable, is a special variable, it is a candidate
				 * that would be modified several times but one that
				 * could eventually end up with the same value it had at
				 * start up. We check for these changes at the time we try to
				 * write INI file out!
				 */
				if (!fIsSpecialToken(section, variable))
					fgSymbolTableModified = TRUE;
				Set_Symbol_Value(entry,value);
			 }
		  }
		  /*
		   * return the value of the symbol found
		   */
		  return(Get_Symbol_Value(entry));
	      }
	      /*
	       * check to see if we have reached the end of the list--
	       * if so, we cannot find the variable so return TK_UNKNOWN
	       */
	      if (Get_Symbol_Next(entry) == TK_NOTHING)
	      {
		   if(set && value!=TK_UNKNOWN)
		   {
			newsym = SubClassSymbol(variable);
			if(newsym)
			{
				Set_Symbol_Value(Token_To_Symbol(newsym),value);
				Append_Symbol(section,newsym);
				fgSymbolTableModified = TRUE;
				return(newsym);
			}
		   }
		   return(TK_UNKNOWN);
	      }
	      /*
	       * set entry to point to the next symbol in the list
	       */
	      preventry = entry;
	      entry = Token_To_Symbol(Get_Symbol_Next(entry));
	  }
	  while(TRUE);
    }
    else
    {
	/* The value is not a subclassed variable--find the global value */
       entry = Token_To_Symbol(variable);
       /*
	* set the value if approprate
	*/
       if (set)
	  Set_Symbol_Value(entry,value);
       /*
	* return the value of the entry
	*/
       return(Get_Symbol_Value(entry));
    }
}

/*  NAME
 *      Get_KeyWord_Assignment
 *  DESCRIPTION
 *      Given a list and a subclass, find the value of the the subclassed
 *      variable in the list.
 *  ARGUMENTS
 *      section is a token to the list
 *      variable is the token of the subclassed symbol we are looking for
 *  RETURN VALUE
 *      value of the variable
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *      this should probably be a macro, since it just calls one funcion.
 *  WARNINGS
 *  HACKS
 */
TOKEN Get_KeyWord_Assignment(TOKEN section,TOKEN variable)
{
      return(SetGet_KeyWord_Assignment(section,variable,TK_NOTHING,FALSE));
}

/*  NAME
 *      Set_KeyWord_Assignment
 *  DESCRIPTION
 *      Given a list and a subclass, set the value of the the subclassed
 *      variable in the list.
 *  ARGUMENTS
 *      section is a token to the list
 *      variable is the token of the subclassed symbol we are looking for
 *  RETURN VALUE
 *      new value of the variable
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *      this should probably be a macro, since it just calls one funcion.
 *  WARNINGS
 *  HACKS
 */
TOKEN Set_KeyWord_Assignment(TOKEN section,TOKEN variable,TOKEN value)
{
      return(SetGet_KeyWord_Assignment(section,variable,value,TRUE));
}

/*  NAME
 *      Get_Identifier_Assignment
 *  DESCRIPTION
 *      Given a token to a list, find a the value of a symbol in
 *      the list with a lexeme the same as "*identifier"
 *  ARGUMENTS
 *      section is a token to a list
 *      identifier is a far pointer to a string which is the lexeme we
 *      are looking for
 *  RETURN VALUE
 *      a token value of the subclass of the identifier
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *     The grunt work of this code is done in "Get_KeyWord_Assignment"
 *  WARNINGS
 *     See warnings on Get_Keyword_Assignment!
 *  HACKS
 */
TOKEN Get_Identifier_Assignment(TOKEN section,char far *identifier)
{
    TOKEN identtoken;
    identtoken = String_To_Token(identifier,-1);
    if (identtoken != TK_UNKNOWN)
    {
       return(Get_KeyWord_Assignment(section,identtoken));
    }
    return(TK_UNKNOWN);
}

TOKEN Get_Identifier_Token(char far *identifier)
{
#if 0
	TOKEN identtoken;
#endif

	int identlen;

	identlen = myfstrlen(identifier);
		if(identlen <= 0)
			return(TK_NOTHING);


#if 1
	return New_Symbol(identifier, identlen) ;
#else
   identtoken = String_To_Token(identifier,-1);
	if(identtoken >= 0)
	{                                                                                                                       
		/* bug bug this was once required..?? */
		//if(lexeme_cmp(identifier,identlen,Get_Token_Identifier(identtoken)))
		//{             
				return(SubClassSymbol(identtoken));
		//}else
		//      Beep();
	}
	identtoken = New_Symbol(identifier,identlen);
	return(identtoken);
#endif
}

/*  NAME
 *      seektoendoflexeme
 *  DESCRIPTION
 *      moves the text cursor to the end of the lexeme it is currently
 *      positions at
 *  ARGUMENTS
 *      none
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      changes gOffset, the text cursor
 *  STATE
 *  COMMENTS
 *      this is used by "lex" to find lexemes
 *      a lexeme is defined here to be anything between
 *      quotes, or from the cursor to the next separator character,
 *      or simply one separator character if the cursor is at a separator
 *  WARNINGS
 *      this code is not able to be re-entrant
 *  HACKS
 */
void seektoendoflexeme(void)
{
	/* special case for variables that end at a line boundary  only */
	if(gIsSpecial)
	{
	   while((!isEOF()) && !isEOL())
	       ++gOffset;
	}
	else
	/*
	 * if the cursor is at a separation character, treat that
	 * character as a lexeme of it's own
	 */

	if (isSeparator())
	{
	    /* just skip over the separtor */
	    ++gOffset;
	    return;
	}
	else
	/* scan along until we find a separator character or EOF */
	{
	   while((!isEOF()) && !isSeparator() )
#ifdef DBCS
	   {
		if (IsDBCSLeadByte(gParseText[gOffset]))
			++gOffset;
		++gOffset;
	   }
#else
	       ++gOffset;
#endif
	}
}

/*
 * length of -1 means string is null terminated
 */

/*  NAME
 *     lexeme_cmp
 *  DESCRIPTION
 *     compares two strings to see if they are equal. one string
 *     is null terminated (the lexeme), the other (src) has a length
 *     of length, or is null-terminated if length is -1
 *  ARGUMENTS
 *     src is a pointer to a string
 *     length is the length of the src string, -1 if src is null-terminated
 *     lexeme is a null-terminated string
 *  RETURN VALUE
 *     TRUE if strings are equal, FALSE otherwise
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *     The code is divided into to segments for speed--one if src is
 *     null terminated, the other if length is not -1
 *  WARNINGS
 *  HACKS
 */
int lexeme_cmp(char far *src,int length,char far *lexeme)
{
	 int i;

    if (length == -1)
    {
	/*
	 * basic string compare
	 */
	while(*src == *lexeme)
	{
	    if (*src == EOS)
		return(TRUE);
	    ++src;
	    ++lexeme;
	}
	return(FALSE);
    }
    else
    {
		if(lexeme[length] == 0)
		{
			for(i=length-1;i>=0;i--)
			{
				if(src[i] != lexeme[i])
					return(FALSE);
			}
			return(TRUE);
		}
		return(FALSE);
#if 0
	/*
	 * strncmp with a twist: we must be at the end of
	 * lexeme so we are sure src is not just a sub-string
	 * of lexeme
	 */
	while (length && (*src == *lexeme))
	{
	   length--;
	   ++src;
	   ++lexeme;
	}
	return((!length) && (*lexeme == EOS));
#endif
    }
}


/*
 * length of -1 means string is null terminated
 */
/*  NAME
 *      String_To_Token
 *  DESCRIPTION
 *      Given a string of length "length", or null-terminated if length = -1,
 *      returns a token to the a symbol with a lexeme equal to string
 *  ARGUMENTS
 *      string is a far string, null-terminated if length = -1
 *      length is the length of string, or -1 if string is null-terminated
 *  RETURN VALUE
 *      a TOKEN belong to a symbol whose lexeme is the same as string
 *      TK_UNKNOWN if no symbol has a lexeme equal to string
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *      This routine is used internally by "lex", but may also be used
 *      by external routines
 *  WARNINGS
 *  HACKS
 */
TOKEN String_To_Token(char far *string,int length)
{
   int i,j;
	int NumTokens;
	int hashind;
	char far *identifier;

#ifdef HASHHITTEST
	char nearstr[100] ;
	if (length == -1)
		strfcpy(nearstr, string) ;
	else
	{
		strfncpy(nearstr, string, length) ;
		nearstr[length] = '\0' ;
	}
#endif
    if((length ==0) || (*string == '\r') || (*string == '\n'))
	return(TK_NOTHING);
    /*
     * query external symbol table function for the number of tokens
     * in the symbol table
     */
    NumTokens = Get_Num_Tokens();
    /*
     * for each token which has it's own type (has a lexeme defined
     * within its symbol instead of being subclassed), check to
     * see if it's lexeme is the same as string
	 */
	hashind = Set_Get_HashBin(Hash(string,length), FALSE,-1);

	if(hashind != -1)
	{
		/* Check if this string is same as string, In case of collisions
		 * on the hash this could fail.
		 */

			identifier = Get_Token_Identifier(Ith_Token(hashind));
			if(identifier[length] == 0)
		   {
				for(j=length-1;j>=0;j--)
				{
					if(string[j] != identifier[j])
						goto hashmiss;
				}
#ifdef HASHHITTEST
				++ghashhits;
#endif
				return(Ith_Token(hashind));
			}
	}
	else
	{

#ifdef HASHHITTEST
			++ghashnotpresent;
#endif
		/* this item does not map to a filled-in hash entry.
		 * if the total number of items < PRIMEHASHBYTES then
		 * the item just don't exist in the table at all.
		 */
		return(TK_UNKNOWN);
	}
hashmiss:
#ifdef HASHHITTEST
	++ghashmisses;
#endif
	/* If we are here, the HASH failed, so search the whole list */
	for(i=0;i<NumTokens;i++)
	{
		/*
		 * we only check the symbols who own their own lexmes--
		 * symbols which are not a subclass of another symbol
		 */
		if (Get_Symbol_Type(Token_To_Symbol(Ith_Token(i))) == Ith_Token(i))
		{
			identifier = Get_Token_Identifier(Ith_Token(i));
			if(identifier[length] == 0)
		   {
				for(j=length-1;j>=0;j--)
				{
				if(string[j] != identifier[j])
					goto wrongo;
				}
				return(Ith_Token(i));
			}
		}
wrongo:
		;
	}

    /*
     * could not find a symbol which lexeme equal to string
     */
#ifdef HASHHITTEST
	++gnohashnotpresent;
#endif
    return(TK_UNKNOWN);
}

/*  NAME
 *      lex
 *  DESCRIPTION
 *      retuns a token for the next lexeme in the text, and moves up
 *      the text cursor.
 *      if the lexeme encounter has no existing symbol, lex creates
 *      a symbol for it and returns its TOKEN
 *  ARGUMENTS
 *      none
 *  RETURN VALUE
 *      a TOKEN value for the lexeme encountered
 *  EFFECTS
 *      moves the text cursor
 *  STATE
 *  COMMENTS
 *      lex is called by "match"
 *  WARNINGS
 *      this code may not go re-entrant!
 *  HACKS
 */
TOKEN lex(void)
{
    TOKEN rettoken;
    int start;

/* Check if we are near the end of what we have read from the text file,
 * and we have not hit EOF, and there is a file
 */
    if(gOffset>gSize-MAXTEXT && gSize>MAXTEXT && gFileHandle) {
        gSize -= MAXTEXT;
        gOffset -= gSize;

/* Move high memory low, and refill high memory from the file */
        RepeatMove(gParseText, gParseText+gSize, MAXTEXT);
        if(_dos_read(gFileHandle, gParseText+MAXTEXT, gSize, &gSize))
            gSize = MAXTEXT;
        else
            gSize += MAXTEXT;
    }

    /*
     * skip text cursor over all white space before the next valid lexeme
     */
    if(!gIsSpecial)
    {
		/* remember how many white spaces we skipped */
		 gNumWhiteSpaces  = gOffset;
       eatWhiteSpace();
		 gNumWhiteSpaces        = gOffset - gNumWhiteSpaces;
    }
    else
    {
	/*
	 * Things after '=' can be a newline, indicating nothing value
	 */
	while((!isEOF()) && (isWhiteSpace() && !isEOL()))
	 ++gOffset;

/* Skip a beginning quote */
        if(isQuote())
            ++gOffset;
    }

    if (!isEOF())
    {
	/*
	 * text cursor gOffset is at the beginning of a lexeme--remember
	 * location
	 */
	start = gOffset;
	/*
	 * move text cursor to end of this lexeme
	 */
	seektoendoflexeme();
	/*
	 * look up a symbol with the same lexeme
	 */
	rettoken = String_To_Token(&gParseText[start],gOffset-start);
	/*
	 * if we found a symbol for this lexeme, return it's TOKEN
	 */
	if (rettoken != TK_UNKNOWN)
	{
	    return(rettoken);
	}
	else
	{
	    /*
	     * make a new symbol for this lexeme
	     */
	    return(New_Symbol(&gParseText[start],gOffset-start));
	}
    }
    else
    {
	/*
	 * we have reach the End-Of-File, so return special
	 * EOF TOKEN
	 */
	return(TK_EOF);
    }
}

/*  NAME
 *      match
 *  DESCRIPTION
 *      checks to see if the next lexeme in is the same as "token".
 *      if so, peeks ahead for the next lexeme, otherwise just returns
 *  ARGUMENTS
 *      token value of the next lexeme to match
 *  RETURN VALUE
 *      TRUE if the token is matched
 *      FALSE if the token is no matched
 *  EFFECTS
 *      when token is matched, moves global text cursor gOffset
 *  STATE
 *  COMMENTS
 *      this is basically the canonical match function
 *      the very first call to match should be with a token
 *      of TK_WILD which forces a match. This is because
 *      match has to look ahead a token
 *  WARNINGS
 *  HACKS
 */
BOOL match(TOKEN token)
{
    /*
     * if we have found the token we expect, eat it and continue
     * always match the TK_WILD token
     */
    if ((token == gLookahead) || (token == TK_WILD))
    {
     /* special case for variables that end only at eol */
	if(token == TK_EQUALS)
	    gIsSpecial = TRUE;
	else
	    gIsSpecial = FALSE;
	gLookahead = lex();
	return(TRUE);
    }
    /* failed to find the token we expect! */
    return(FALSE);
}

#ifdef DBCS
/*
	Test if the character is DBCS lead byte

	input:  c = character to test
	output: TRUE if leadbyte
*/

int     IsDBCSLeadByte(c)
unsigned char c;
{
	static unsigned char far *DBCSLeadByteTable = NULL;

	union REGS inregs,outregs;
	struct SREGS segregs;
	unsigned char far *p;

	if (DBCSLeadByteTable == NULL)
	{
		inregs.x.ax = 0x6300;           /* get DBCS lead byte table */
		intdosx(&inregs, &outregs, &segregs);
		FP_OFF(DBCSLeadByteTable) = outregs.x.si;
		FP_SEG(DBCSLeadByteTable) = segregs.ds;
	}

	p = DBCSLeadByteTable;
	while (p[0] || p[1])
	{
		if (c >= p[0] && c <= p[1])
			return TRUE;
		p += 2;
	}
	return FALSE;
}


/*
	Check if the character point is at tail byte

	input:  *str = strart pointer of the string
		*point = character pointer to check
	output: TRUE if at the tail byte
*/

int     CheckDBCSTailByte(str,point)
unsigned char *str,*point;
{
	unsigned char *p;

	p = point;
	while (p != str)
	{
		p--;
		if (!IsDBCSLeadByte(*p))
		{
			p++;
			break;
		}
	}
	return ((point - p) & 1 ? TRUE : FALSE);
}
#endif

