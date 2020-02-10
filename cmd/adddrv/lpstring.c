;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include "common.h"

lpstrlen(lp)
LPSTR lp;
{
	unsigned short n;

	for (n = 0; *lp++; n++)
		;
	return(n);
}

lpstrncmp(lp, lp2, n)
LPSTR lp, lp2;
unsigned short n;
{
	register char c, c2;

	while (n-- > 0) {
		c = *lp++;
		c2 = *lp2++;
		if (c < c2)
			return(-1);
		else if (c > c2)
			return(1);
	}
	return(0);
}

void
lpstrnupr(s, count)
LPSTR s;
register unsigned short count;
{
	register BYTE c;

	while(count > 0) {
		c = *s;
		if (IsDBCSLeadByte(c)) {			/* MSKK20 */
			s++, count--;
		} else if (c >= 'a' && c <= 'z') {
			*s -= 0x20;
		}
		s++, count--;
	}
}

void
lpstrcpy(lp, lp2)
LPSTR lp, lp2;
{
	while (*lp++ = *lp2++)
		;
}

void							/* MSKK10 */
lpstrncpy(lp, lp2, count)				/* MSKK10 */
LPSTR	lp, lp2;					/* MSKK10 */
int	count;						/* MSKK10 */
{							/* MSKK10 */
	while (count--) {				/* MSKK10 */
							/* MSKK10 */
		if ((*lp++ = *lp2++) == '\0')		/* MSKK10 */
			break;				/* MSKK10 */
							/* MSKK10 */
	}						/* MSKK10 */
}							/* MSKK10 */

void	conv_dbspace(s, count)				/* MSKK20 */
LPSTR s;
register unsigned short count;
{
	register BYTE c;

	while(count > 0) {
		c = *s;
		if (IsDBCSLeadByte(c)) {
			if (*s == DB_SP_HI && *(s+1) == DB_SP_LO)	/* if DBCS space */
			{
				*s = ' ';
				*(s+1) = ' ';
			}
			s++, count--;
		}
		s++, count--;
	}
}

wordlen(lp)						/* MSKK20 */
LPSTR lp;
{
	unsigned short n;
	unsigned char c;

	for (n = 0; c = *lp++; n++)
	{
		if (c == 0x0a || c == 0x0d || isdelim(c))
			return (n);
	}
	return(n);
}

/*
	Test if the character is DBCS lead byte

	input:	c = character to test
	output:	TRUE if leadbyte
*/

int	IsDBCSLeadByte(c)		/* MSKK20 */
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
