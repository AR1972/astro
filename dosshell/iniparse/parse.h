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
**   Date      Author	Modification
** --------   --------	------------------------------------------------
**  8/10/89   scottq	written
**
*/
/*
 * This is the definition of a TOKEN
 */
typedef int TOKEN;

/*
 * look-ahead value used by the lex.c function "match"
 */
extern TOKEN gLookahead;

/*
 * external functions
 */
char far * Get_Token_Identifier(TOKEN token);
BOOL match(TOKEN token);

void Init_ParseText(int, char far *, unsigned int, unsigned int);
TOKEN String_To_Token(char far *string,int length);
TOKEN Get_KeyWord_Assignment(TOKEN section,TOKEN variable);
TOKEN Get_Identifier_Assignment(TOKEN section,char far *identifier);
TOKEN Set_Identifier_Assignment(TOKEN section,char far *identifier,TOKEN value);
TOKEN Set_KeyWord_Assignment(TOKEN section,TOKEN variable,TOKEN value);
void Append_Symbol(TOKEN list,TOKEN entry);
TOKEN SubClassSymbol(TOKEN type);
int Get_List_Length(TOKEN list);
TOKEN Get_Ith_Element(TOKEN list,int ith);
TOKEN New_Symbol(char far *text,int length);

BOOL Write_Ini_File(char *);

#ifdef DBCS
int	IsDBCSLeadByte(unsigned char);
int	CheckDBCSTailByte(unsigned char *,unsigned char *);
#endif
