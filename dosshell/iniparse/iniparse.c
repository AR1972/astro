;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
** PARSE.H
**
**  This file defines and structures used by lex.c and iniparse.c
**
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  8/10/89   scottq    written
**  8/13/89   scottq    redid quite a bit to account for new symbol table
**
*/
#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <errno.h>

BOOL fgSymbolTableModified = FALSE;
long glStartIni;

extern unsigned gWriteErrs;
extern unsigned FlushBufferedWrite(int fhandle);
extern unsigned Buffered_dos_write(int fhandle,char far *str, unsigned count,
										unsigned *pbytes) ;

#ifdef CONVERT_UTILITY
#include <malloc.h>
#endif

TOKEN Assignment();

/*  NAME
 *      List
 *  DESCRIPTION
 *      Builds a list from the lexemes in the text, attatching list to
 *      parent
 *  ARGUMENTS
 *      parent TOKEN is the symbol whose value will become the list
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      memory allocation, new symbols, the works
 *  STATE
 *  COMMENTS
 *      This code matches all lists, so is recursive in Assignent
 *  WARNINGS
 *      No error checking/recover is provide for
 *  HACKS
 *      NO ERROR CHECKING !!!!!!!!!
 */
void List(TOKEN parent)
{
    TOKEN temp;

    /*
     * We need a symbol, so we subclass it
     * its parameter "parent" will end up being ignored
     */
    temp = SubClassSymbol(parent);

    /*
     * contiute to parse this list until we get to the end of
     * the list (or EOF)
     */
    while( (gLookahead != TK_EOF) && (gLookahead != TK_RIGHTCURLY))
    {
       /*
	* newly allocated symbol is a list of assigned variables,
	* so we append the assignments found
	*/
       Append_Symbol(temp,Assignment());
    }
    /*
     * Set the parent to have a value of the new list
     */
    Set_Symbol_Value(Token_To_Symbol(parent),temp);
    /*
     * there should be a "}" next in the text, or perhaps EOF
     */
    match(TK_RIGHTCURLY);
}

/*  NAME
 *      Assignment
 *  DESCRIPTION
 *      Parses a <lexeme> = <lexeme> pair, and builds symbols for them
 *  ARGUMENTS
 *      none
 *  RETURN VALUE
 *      a TOKEN to the variable (new symbol) being assigned
 *  EFFECTS
 *      memory allocation, new symbols, the works
 *  STATE
 *  COMMENTS
 *      This code matches all assignments, so is recursive in List
 *  WARNINGS
 *      No error checking/recover is provide for
 *  HACKS
 *      NO ERROR CHECKING !!!!!!!!!
 */
TOKEN Assignment()
{
    TOKEN temp;
    BOOL hitnewline;
    hitnewline = FALSE;
    /*
     * create a new subclassed symbol for the up-coming lexeme
     */
    temp = SubClassSymbol(gLookahead);
    match(gLookahead);
    match(TK_EQUALS);
    while(gLookahead == TK_NOTHING)
    {
	hitnewline = TRUE;
	match(TK_NOTHING);
    }
    /*
     * an assignment can be to a list of assignments or a single assignment
     */
    if (gLookahead == TK_LEFTCURLY)
    {
	/*
	 * we are assigning a list to the new symbol
	 */
	match(TK_LEFTCURLY);
	List(temp);
    }
    else
    {
	if(!hitnewline)
	{
	/*
	 * we are only assigning an atom to the symbol
	 */
	Set_Symbol_Value(Token_To_Symbol(temp),gLookahead);
	match(gLookahead);
	}
	else
	{
	  Set_Symbol_Value(Token_To_Symbol(temp),TK_NOTHING);
	}

    }
    return(temp);
}

/*  NAME
 *      SectionBody
 *  DESCRIPTION
 *      Parses all assignments in a section (after [<?>])
 *  ARGUMENTS
 *      section is a TOKEN to the name of the section being parsed
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      memory allocation, new symbols, the works
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *      No error checking/recover is provide for
 *  HACKS
 *      NO ERROR CHECKING !!!!!!!!!
 */
void SectionBody(TOKEN section)
{
    /*
     * Keep matching Assignments until we get to another Section
     */
    while( (gLookahead != TK_EOF) && (gLookahead != TK_LEFTBRACKET))
    {
       Append_Symbol(section,Assignment());
    }
}

/*  NAME
 *      Section
 *  DESCRIPTION
 *      matches the header of a section ([<?>])
 *  ARGUMENTS
 *      none
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      memory allocation, new symbols, the works
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *      No error checking/recover is provide for
 *  HACKS
 *      NO ERROR CHECKING !!!!!!!!!
 */
void Section()
{
    TOKEN temp;
    /*
     * match "[<sectionname>]"
     */
    match(TK_LEFTBRACKET);
    temp = gLookahead;
    match(gLookahead);
    match(TK_RIGHTBRACKET);
    /*
     * parse the body of the Section
     */
    SectionBody(temp);
    /*
     * each section is really a list of its assignments,
     * there is a special token value for list headers TK_LISTHEAD
     */
    Set_Symbol_Value(Token_To_Symbol(temp),TK_LISTHEAD);
}

/*  NAME
 *      inifile
 *  DESCRIPTION
 *      parses an inifile
 *  ARGUMENTS
 *      none
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      memory allocation, new symbols, the works
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
void inifile()
{
    /*
     * keep parsing sections until EOF is reached
     */
    while(gLookahead != TK_EOF)
    {
	Section();
    };
}

/*  NAME
 *      parse
 *  DESCRIPTION
 *      start parsing
 *  ARGUMENTS
 *      none
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      memory allocation, new symbols, the works
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
void parse()
{
    /*
     * the lexer needs to be kick started by matching TK_WILD token
     */
    match(TK_WILD);
    inifile();
}

/* Saves the value of a few tokens that get modified but typically get
 * reset back to their value at start up. For example: the value of
 * "startup" could be "startprograms" initially, and when we get to the
 * filemanager it becomes "filemanager" and then if we exit via the program
 * manager, it gets back its original value, so we don't need to save
 * ini file.
 */
TOKEN gtk_startup = TK_UNKNOWN ;
TOKEN gtk_explicitsel = TK_UNKNOWN ;
TOKEN gtk_filemgrmode = TK_UNKNOWN ;

BOOL fIsSpecialToken(TOKEN section, TOKEN tok)
{
	if ( ((section == TK_SAVESTATE) && (tok == TK_STARTUP)) ||
		 ((section == TK_SAVESTATE) && (tok == TK_EXPLICITSEL)) ||
		 ((section == TK_SAVESTATE) && (tok == TK_FILEMGRMODE))
	   )
		return TRUE ;

	return FALSE ;
} /* fIsSpecialToken */

void InitWriteIniFileOpt(void)
{
	gtk_startup = Get_KeyWord_Assignment(TK_SAVESTATE,TK_STARTUP) ;
	gtk_explicitsel = Get_KeyWord_Assignment(TK_SAVESTATE,TK_EXPLICITSEL);
	gtk_filemgrmode = Get_KeyWord_Assignment(TK_SAVESTATE,TK_FILEMGRMODE);

}  /* InitWriteIniFileOpt */

BOOL fNoEffectiveChange(void)
{
	if ( (gtk_startup == Get_KeyWord_Assignment(TK_SAVESTATE,TK_STARTUP)) &&
		 (gtk_explicitsel == Get_KeyWord_Assignment(TK_SAVESTATE,TK_EXPLICITSEL)) &&
		 (gtk_filemgrmode == Get_KeyWord_Assignment(TK_SAVESTATE,TK_FILEMGRMODE))
	   )
		return TRUE ;

	return FALSE ;

}  /* fNoEffectiveChange */

/*      NAME
 *      Read_IniFile
 *  DESCRIPTION
 *      Read in an ini file and parse it
 *  ARGUMENTS
 *      inifile's name
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      memory allocation, new symbols, the works
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

#define MAXTEXT_IN_K (2)

#ifdef CONVERT_UTILITY
#define IniAlloc(s, x) (x = x>0xfff0L ? 0xfff0L : x, s = _fmalloc(x))
#define IniFree(s) _ffree(s)
#else
#define IniAlloc(s, x) (x=AllocClosestTo1K(&s, 2*MAXTEXT_IN_K, x>=0xfc00L ? 0x003f : (unsigned)(x>>10)+1))
#define IniFree(s) FreeWorkFar(s)
#endif

extern char szDefaultIni[];

/*
 * Read in ini file at inifilename. If inifilename is NULL, use defaults
 *
 */
BOOL Read_Ini_File(char *inifilename)
{
    int fhandle;
    long lSize, lSaveSize;
    unsigned int size;
    char cLBrack;
    char far *buffer;
    char far *saveBuffer = NULL;
	 BOOL fFileOpened = FALSE ; // whether INI file was opened or not

    glStartIni = 0;

/* open the file */
    if(inifilename == NULL)
    {
defaultini:
	/* initialize lexer globals */
		Init_ParseText(0, szDefaultIni, 0, strlen(szDefaultIni));

    }
    else
    if(_dos_open(inifilename, O_RDONLY, &fhandle)) 
    {
	return (FALSE);
    } 
    else 
    {
		  fFileOpened = TRUE ;

/* get the size of the file by seeking to the end */
	lSize = lSaveSize = lseek(fhandle, 0L, SEEK_END);
/* seek back to the start and read in the file */
	lseek(fhandle, 0L, SEEK_SET);

/* allocate memory for the file contents we are about to read in */
	if(!IniAlloc(saveBuffer, lSize))
	    goto defaultini;

/* HACK!! Seek to the first [ so we can put comments at the start of the file */
	cLBrack = *Get_Token_Identifier(TK_LEFTBRACKET);
	size = 1;

	while(TRUE) {
	    if(_dos_read(fhandle, saveBuffer, (int)lSize, &size) || !size)
		goto defaultini;
	    lSize = size;

	    buffer = saveBuffer;
	    while(size>0 && *buffer!=cLBrack) {
		--size;
		++buffer;
	    }
	    glStartIni += (unsigned int)(buffer - saveBuffer);
	    if(*buffer == cLBrack)
		break;
	}

	if(lSaveSize-glStartIni < 3)
	    goto defaultini;

/* initialize lexer globals */
	Init_ParseText(fhandle, saveBuffer,
		(unsigned int)(buffer-saveBuffer), (unsigned int)lSize);
    }
    parse();

/* set global var indicating we have not modified the symbol table */
    fgSymbolTableModified = FALSE;

    if(saveBuffer)
	IniFree(saveBuffer);

    InitWriteIniFileOpt() ;

	 /* close the INI file in case it was opened by us! */
	 if (fFileOpened)
		_dos_close(fhandle) ;

    return(TRUE);
}


unsigned int myfstrlen(s)
unsigned char far *s;
	{
	int i = 0;

	while (*s++)
		i++;
	return(i);
	}
void indent(int fhandle,int level)
{
    int i,size;
    for(i=0;i<level;i++)
    {
	 Buffered_dos_write(fhandle,"     ",4,&size);
    }
}

void write_list(int fhandle,TOKEN token,int level)
{
     int size;
     char far *str;
     SYMBOL_TABLE_ENTRY far *temp;

     while(token != TK_NOTHING)
     {
	 temp = Token_To_Symbol(token);
	 token = Get_Symbol_Next(temp);

	 if (token != TK_NOTHING)
	 {
	    str = Get_Token_Identifier(token);
	    size = myfstrlen(str);
	    indent(fhandle,level);

	    Buffered_dos_write(fhandle,str,size,&size);
	    Buffered_dos_write(fhandle," = ",3,&size);

	    temp = Token_To_Symbol(token);
	    if(Get_Symbol_Value(temp) != TK_NOTHING)
	    {
		temp = Token_To_Symbol(Get_Symbol_Value(temp));
		if(Get_Symbol_Next(temp) != TK_NOTHING)
		{
		     Buffered_dos_write(fhandle,"\r\n",2,&size);
		      indent(fhandle,level);
		     Buffered_dos_write(fhandle,"{",1,&size);
		     Buffered_dos_write(fhandle,"\r\n",2,&size);
		     write_list(fhandle,Get_Symbol_Value(Token_To_Symbol(token)),level+1);
		      indent(fhandle,level);
		     Buffered_dos_write(fhandle,"}",1,&size);

		 }
		 else
		 {
		     str = Get_Token_Identifier(Get_Symbol_Value(Token_To_Symbol(token)));
	    if(*str=='"' || (unsigned char)(*str)<=' ')
			Buffered_dos_write(fhandle,"\"",1,&size);
		     size = myfstrlen(str);
		    Buffered_dos_write(fhandle,str,size,&size);
		 }

	     }
	     Buffered_dos_write(fhandle,"\r\n",2,&size);

	   }
       }
}

#define TEMPNAMECHAR '#'

BOOL Write_Ini_File(char *szFileName)
{
    int fhandle, fold;
    int maxtoks, i, nExt;
    char far *str;
    SYMBOL_TABLE_ENTRY far *cursymbol;
    unsigned int size;
    char szTempName[MAX_PATH+1];

	i = FindLastComponent(szFileName) ;
	if (szFileName[i] == PATHCHAR)
		i++ ;

	/* Copy the drive of the ini file */
    RepeatMove(szTempName, szFileName, i);

/* Create a temp name: #.n, where n is a hex number between 0 and 0xfff */
    szTempName[i] = TEMPNAMECHAR;
    szTempName[i+1] = '.';
    for(nExt=0; ; ++nExt) {
	if(nExt == 0x1000)
	    goto Error1;
	itoa(nExt, szTempName+i+2, 16);
	if(_dos_creatnew(szTempName, _A_NORMAL, &fhandle)) {
	    if(errno == EEXIST)
		continue;
	    else
		goto Error1;
	} else
	    break;
    }

/* Reset the number of write errors and then write the ini comments */
    gWriteErrs = 0;
    if(glStartIni>0 && !_dos_open(szFileName, O_RDONLY, &fold)) {
	long lTotal, lSize;

	lSize = glStartIni;
	if(!IniAlloc(str, lSize))
	    goto Error2;

/* Copy to where we found the first left bracket when reading the file */
	for(lTotal=glStartIni; lTotal>0 &&
		!_dos_read(fold, str, (int)lSize, &size) && size;
		lTotal -= size) {
	    Buffered_dos_write(fhandle, str,
		    lTotal>(long)size ? size : (int)lTotal, &nExt);
	}

	FreeWorkFar(str);
Error2:
	_dos_close(fold);
    }

/* Write the ini information */
    maxtoks = Get_Num_Tokens();
    for(i=0; i<maxtoks; ++i) {
	cursymbol = Token_To_Symbol(Ith_Token(i));
	if(Get_Symbol_Value(cursymbol) == TK_LISTHEAD) {
	    Buffered_dos_write(fhandle,"[",1,&size);
	    str = Get_Token_Identifier(Ith_Token(i));
	    size = myfstrlen(str);
	    Buffered_dos_write(fhandle,str,size,&size);
	    Buffered_dos_write(fhandle,"]",1,&size);
	    Buffered_dos_write(fhandle,"\r\n",2,&size);

	    write_list(fhandle,Ith_Token(i),0);
	}
    }

/* Flush the output, close the file, and delete the old ini file */
    FlushBufferedWrite(fhandle);
    if(_dos_close(fhandle) || gWriteErrs>0)
	goto Error3;

    unlink(szFileName);
    if(!rename(szTempName, szFileName))
	return(TRUE);

/* on error, delete the temp file and return FALSE */
Error3:
    unlink(szTempName);
Error1:
    return(FALSE);
}

