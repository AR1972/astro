;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
#include "internat.h"
#include <dos.h>
#define   NULL    0
#define   TRUE    0xffff
#define   FALSE   0
#ifndef DBCS
#define   DBCS   TRUE	
#endif
char	haveinttab = FALSE;
/*
 * ECS Support - This module provides support for international >7FH and 
 * TWO-BYTE character sets.  The toupper routine uses the DOS MAP_CASE call.
 * In addition, STRING.C contains a default_tab containing a default lead
 * byte table for two byte character sets.  If single byte operation is
 * desired, modify this table as follows:  ="\000".  If this utility 
 * is run on a DOS with Function 63H support, the default table will 
 * be replaced by the table in the DOS.  The lbtbl_ptr is the far ptr to
 * which ever table is in use.
*/
long  lbtbl_ptr;
char  *default_tab="\201\237\340\374\000\000";
char	have_lbtbl = FALSE;

struct	InterTbl Currtab;

int toupper(int c);
char *strupr(char *string);
char *strpbrk(char *string1,char *string2);
IToupper(int c, long routine);
#ifdef DBCS
int	IsDBCSLeadByte(unsigned char);
int	CheckDBCSTailByte(unsigned char *,unsigned char *);
#endif

int toupper(c)
int c;
{
	union REGS regs ;

	if(!haveinttab) {
	    regs.x.ax = 0x3800 ;
	    regs.x.dx = (unsigned) &Currtab ;
	    intdos (&regs, &regs) ;		/* INIT the table */

	    haveinttab = (char)TRUE;
	}

	return(IToupper(c,Currtab.casecall));

}

char *strupr(string)
char *string;
{
	register char *p1;

	p1 = string;
	while (*p1 != NULL) {
	/*
	 *  A note about the following " & 0xFF" stuff. This is
	 *  to prevent the damn C compiler from converting bytes
	 *  to words with the CBW instruction which is NOT correct
	 *  for routines like toupper
	 */
#ifdef DBCS
	 if(IsDBCSLeadByte(*p1 & 0xFF))
		p1 += 2 ;
	    else
		*p1++ = toupper(*p1 & 0xFF);
#else
	    *p1++ = toupper(*p1 & 0xFF);
#endif
	}
	return(string);
}

char *strpbrk(string1,string2)
char *string1;
char *string2;
{
	register char *p1;

	while (*string1 != NULL) {
	/*
	 *  A note about the following " & 0xFF" stuff. This is
	 *  to prevent the damn C compiler from converting bytes
	 *  to words with the CBW instruction which is NOT correct
	 *  for routines like toupper
	 */
#ifdef DBCS
	    if(IsDBCSLeadByte(*string1 & 0xFF))
		string1 += 2 ;
	    else {
#endif
		p1 = string2;
		while (*p1 != NULL) {
		    if(*p1++ == *string1)
			return(string1);
		}
		string1++;
#ifdef DBCS
	    }
#endif

	}
	return(NULL);			/* no matches found */
}

#ifdef DBCS
/*
	Test if the character is DBCS lead byte

	input:	c = character to test
	output:	TRUE if leadbyte
*/

int	IsDBCSLeadByte(c)
unsigned char c;
{
	static unsigned char far *DBCSLeadByteTable = NULL;

	union REGS inregs,outregs;
	struct SREGS segregs;
	unsigned char far *p;

	if (DBCSLeadByteTable == NULL)
	{
		inregs.x.ax = 0x6300;		/* get DBCS lead byte table */
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

	input:	*str = strart pointer of the string
		*point = character pointer to check
	output:	TRUE if at the tail byte
*/

int	CheckDBCSTailByte(str,point)
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