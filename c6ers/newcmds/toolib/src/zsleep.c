/*** ZSleep - causes program to _politely_ wait at least X milliseconds
 *	for DOS only; OS/2 and NT have their own methods that should
 *	be set up in ttypes.h.	The routine calculates the number of
 *	clock ticks in X milliseconds and adds the current tick value
 */

#if defined(DOS)

#include <dos.h>
#include "..\h\tools.h"

void ZSleep(long lMilliseconds);

void ZSleep(long lMilliseconds)
{
    union REGS		    inregs, outregs;
    struct SREGS	    segregs;
    flagType		    fCanIdle;
    typedef unsigned long   TIX;
    TIX			    ticks, oldticks, newticks;

    /* Locate Idle interrupt address (to see if we can call it) */

    segread(&segregs);
    inregs.h.ah = 0x35;			//Get Interrupt Vector
    inregs.h.al = 0x2f;			//Specify the interrupt we're asking for
    segregs.es = inregs.x.bx = 0;	//Clear results
    intdosx(&inregs, &outregs, &segregs);
    fCanIdle = (flagType)(segregs.es | outregs.x.bx);

    //Determine number of ticks in x milliseconds, given that there
    //are ~54.9450 millisecs/tick at 18.2 ticks/sec

    ticks = lMilliseconds * 10;
    ticks /= 549;

    //and add those ticks to the current tick count

    newticks = oldticks = *(TIX far * volatile)0x46C;
    ticks += oldticks;

    // Wait until the tick changes to past the desired value.

    do {
	if (oldticks > newticks)
	    ticks -= (oldticks - newticks);
	oldticks = newticks;

	//Note that the Idle interrupt and the Idle call are two separate items

	int86(0x28, &inregs, &outregs); //DOS Idle interrupt

	if (fCanIdle) {
	    inregs.x.ax = 0x1680; //DOS Idle call
	    int86(0x2F, &inregs, &outregs);
	    fCanIdle = (flagType) !outregs.h.al; //don't call if not supported
	}
	newticks = *(TIX far * volatile)0x46C;

    } while (ticks >= newticks);
}
#endif



#ifdef STANDALONE

#include <stdio.h>

main()
{
    puts("Waiting for 10 seconds");
    fflush(stdout);
    ZSleep(10000L);
    puts("\aWait completed");
}
#endif
