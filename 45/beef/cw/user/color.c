/*
	COW : Character Oriented Windows

	color : default colors
	SWAP TUNE NOTE : belongs in INIT module !!!!
*/

#define COW
#include <cow.h>

#include <uscreen.h>
#include <uisa.h>
#include <ucolor.h>

#include "screen.h"
#include "color.h"


extern SA PASCAL rgsa[];			/* isa -> sa */

#ifdef SCREEN_FFONT
extern WORD PASCAL mpisaffont[];		/* isa -> ffont */
#endif /*SCREEN_FFONT*/



PUBLIC VOID FARPUBLIC
SetIsaColor(isa, coFore, coBack)
/*
  -- set colors for a given ISA
*/
ISA	isa;
WORD	coFore;
WORD	coBack;
	{
	StartPublic();

	AssertSz(coFore < 16 && coBack < 16, "SetSysColor invalid color");

	rgsa[isa].u.draw.caSa = (BYTE) CaMake(coFore, coBack);

	StopPublic();
	}



PUBLIC VOID FARPUBLIC
GetIsaColor(isa, pcoFore, pcoBack)
/*
  -- get colors for a given ISA
*/
ISA	isa;
WORD *	pcoFore;
WORD *	pcoBack;
	{
	BYTE ca;

	StartPublic();

	ca = rgsa[isa].u.draw.caSa;

	*pcoBack = (ca >> 4) & 15;
	*pcoFore = ca & 15;

	StopPublic();
	}



PUBLIC VOID FARPUBLIC
SetIsaRgca(isa, rgcaFill)
ISA	isa;
BYTE *	rgcaFill;
	{
	StartPublic();

	rgsa[isa].u.rgcaFill = rgcaFill;	/* note : trashes ca */

	StopPublic();
	}



#ifdef SCREEN_FFONT
PUBLIC VOID FARPUBLIC
SetIsaFfont(isa, ffont)
ISA	isa;
WORD	ffont;
	{
	mpisaffont[isa] = ffont;
	}
#endif /*SCREEN_FFONT*/
