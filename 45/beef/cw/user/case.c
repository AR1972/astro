/*
	COW : Character Oriented Windows

	case.c: conversion of foreign lower case to upper case
*/

#define COW
#include <cow.h>

#include "case.h"

/* 
  -- SzLcUc: maps lower -> upper case for special foreign characters
  -- This string contains the uppercase/lowercase correspondence list for
     extended characters.  Every character in an even position in the list
     is lowercase.  The character following it is its uppercase representation.
     Thus, the even entries contain all the lowercase letters among the upper
     128 characters; the odd entries contain the uppercase letters.
  -- NOTE:  Some entries may contain a normal (ASCII) character.  This
     indicates that there is no uppercase (or lowercase) equivalent for the
     character's partner.
  -- Corresponds to IBM Code Page 850
*/
static char szLcUc[] = "\202\220\212\324\210\322\205\267\227\353\207\200\204\216\206\217\224\231\201\232\221\222\244\245\240\265\203\266\211\323\241\326\215\336\214\327\213\330\242\340\225\343\223\342\243\351\226\352\306\307\246\101\325\111\233\235\344\345\247\117\354\355\230\131";

/*	Here is the character set for the list:
	202		e acute	
	220		E acute
	212		e grave
	324		E grave
	210		e circumflex
	322		E circumflex
	205		a grave
	267		A grave
	227		u grave
	353		U grave
	207		c cedilla
	200		C cedilla
	204		a umlaut
	216		A umlaut
	206		a circle
	217		A circle
	224		o umlaut
	231		O umlaut
	201		u umlaut
	232		U umlaut
	221		ae
	222		AE
	244		n tilde
	245		N tilde
	240		a acute
	265		A acute
	203		a circumflex
	266		A circumflex
	211		e umlaut
	323		E umlaut
	241		i acute
	326		I acute
	215		i grave
	336		I grave
	214		i circumflex
	327		I circumflex
	213		i umlaut
	330		I umlaut
	242		o acute
	340		O acute
	225		o grave
	343		O grave
	223		o circumflex
	342		O circumflex
	243		u acute
	351		U acute
	226		u circumflex
	352		U circumflex
	306		a tilde
	307		A tilde
	246		a ordfeminine
	101		A
	325		i no dot
	111		I
	233		o slash
	235		O slash
	344		o tilde
	345		O tilde
	247		o ordmasculine
	117		O
	354		y acute
	355		Y acute
	230		y umlaut
	131		Y
*/


PRIVATE char FARPRIVATE
ChUpperFromChExt(ch)
char ch;
/*
  -- converts extended lower case to upper case using szLcUc
*/
{
	char *pch;

	for (pch = szLcUc; *pch != 0; pch++)
		{
		if (ch == *pch) 	/* even index means lower case */
			{
			if ((pch - szLcUc) & 1)
				return(ch);
			else
				return(*(pch + 1));
			}
		}
	return(ch);
}

