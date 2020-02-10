/* debug.h - included in each C module via interp.h							*/
/* NOTE: When making changes to this file, be sure to make equivalent	*/
/*			changes to file DEBUG.INC													*/

#undef DEBUG_H
#define DEBUG_H ON			/* remember that this file has been included */

#if !HEAP_H
#include "heap.h"
#endif

/* This function is in exdosfun.asm as part of the TIMER function.	*/
/* It returns a hundredths of a second clock as an unsigned long integer.*/
/* It is available in RELEASE versions to allow internal data collection.*/
unsigned long pascal Timer(void);


#define DbOb(bArg)							{;}
#define DbOHb(bArg)							{;}
#define DbOHbN(bArg)							{;}
#define DbON()									{;}
#define DbOsz(sz)								{;}
#define DbOszN(fsz)							{;}
#define DbOsd(psd)							{;}
#define DbOsdN(psd)							{;}
#define DbOHw(wArg)							{;}
#define DbOHwN(wArg)							{;}
#define DbOHd(dArg)							{;}
#define DbOHdN(dArg)							{;}
#define DbODw(wArg)							{;}
#define DbODwN(wArg)							{;}

#define DbHalt(fsz)							{;}
#define DbAssert(assertion)				{;}
#define DbAssertIf(fTestit,assertion)	{;}



#define STATICF(type) static type PLM NEAR
#define STATICV static

#define DbE(szName,dbglev)								{;}
#define DbEw(szName,dbglev,arg1)						{;}
#define DbEsz(szName,dbglev,arg1)					{;}
#define DbEd(szName,dbglev,arg1)						{;}
#define DbEww(szName,dbglev,arg1,arg2)				{;}
#define DbEszw(szName,dbglev,arg1,arg2)			{;}
#define DbEwsz(szName,dbglev,arg1,arg2)			{;}
#define DbEszsz(szName,dbglev,arg1,arg2)			{;}
#define DbEwww(szName,dbglev,arg1,arg2,arg3)		{;}
#define DbEszww(szName,dbglev,arg1,arg2,arg3)	{;}
#define DbX(szName,dbglev)								{;}
#define DbXw(szName,dbglev,arg1)						{;}
#define DbXsz(szName,dbglev,arg1)					{;}
#define DbXd(szName,dbglev,arg1)						{;}
							
/* perform x only if DEBUG is set */
#define DebugMacro(x)
