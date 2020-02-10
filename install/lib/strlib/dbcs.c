/***************************************************************************/
/*																									*/
/*	DBCS.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/***************************************************************************/

#ifdef DBCS

#include	<dos.h>
#include	<ctype.h>

/*
	Test if the character is DBCS lead byte

	input:	c = character to test
	output:	TRUE if leadbyte
*/

int	IsDBCSLeadByte(unsigned char c)
{
	static unsigned char far *DBCSLeadByteTable = 0;

	union REGS inregs,outregs;
	struct SREGS segregs;
	unsigned char far *p;

	if (DBCSLeadByteTable == 0)
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
			return 1;
		p += 2;
	}
	return 0;
}



/*
	Check if the character point is at tail byte

	input:	*str = strart pointer of the string
		*point = character pointer to check
	output:	TRUE if at the tail byte
*/

int	CheckDBCSTailByte(unsigned char *str,unsigned char *point)
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
	return ((point - p) & 1);
}

/*
	DBCS enabled strupr
*/
unsigned char	*DBCSstrupr(unsigned char *str)
{
	unsigned char	*s;

	s = str;
	while (*s)
	{
		if (IsDBCSLeadByte(*s))
			s++;
		else
			*s = toupper(*s);
		s++;
	}
	return (str);
}

/*
	DBCS enabled strchr
*/
unsigned char	*DBCSstrchr(unsigned char *str,unsigned char c)
{
	while (*str)
	{
		if (IsDBCSLeadByte(*str))
			str++;
		else if (*str == c)
			return (str);
		str++;
	}
	if (c == '\0')
		return (str);
	else
		return 0;
}

#endif

