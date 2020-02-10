/* KBD.C  */

#include "skdefs.h"
#include "gideidef.h"
#include "vars.h"
#include "kbd.h"
#include "drv.h"
#include "init.h"


/****************************************************************************

	FUNCTION:	processKbdPress(void);

	SYNTAX:		<esc> <press code> [param1] [,param2] [,param3] ... [,param5] <term code>
					at this point, we already have <press code>

	COMMENTS:	With PRESS, each key gets typed individually.
					For example, if string is  a,b,left,f1.  then the sequence goes like:
					a-down, a-up, b-down, b-up, left-down, left-up, f1-down, f1-up.
*/
void processKbdPress(void)
{
	if (gCode==TERMCODE) doPressLst();				/* if term, process the list	*/
	else {													/* if no room for code, ignore	*/
		if (tmpLst.len<MAXLISTLEN && validKCode())	/* if not illegal param, continue*/
																/* If LOCKed or HOLDed, ignore	*/
			if (!inLst(&kLock,gCode) && !inLst(&kHold,gCode))
				tmpLst.list[tmpLst.len++] = gCode;	/* Finally! add to list	*/
		}
}


/****************************************************************************

	FUNCTION:	processKbdCombine(void)

	SYNTAX:		<esc> <combine code> [param1] [,param2] [,param3] ... [,param5] <term code>
					at this point, we already have <combine code>

	COMMENTS:	With COMBINE, the keys are typed in combination.
					For example, if string is  a,b,left,f1.  then the sequence goes like:
					a-down, b-down, left-down, f1-down, f1-up, left-up, b-up, a-up.


*/
void processKbdCombine(void)
{
	if (gCode==TERMCODE) doCombineLst();			/* if term, process the list	*/
	else {													/* if no room for code, ignore	*/
		if (tmpLst.len<MAXLISTLEN && validKCode())	/* if not illegal param, continue*/
																/* If LOCKed HOLDed, ignore	*/
			if (!inLst(&kLock,gCode) && !inLst(&kHold,gCode)) {
				if (!inLst(&tmpLst,gCode))				/* duplicates NOT ok here */
					tmpLst.list[tmpLst.len++] = gCode;
				}

		}
}

/****************************************************************************

	FUNCTION:	processKbdHold(void)

	SYNTAX:		<esc> <hold code> [param1] [,param2] [,param3] ... [,param5] <term code>
					at this point, we already have <hold code>

	COMMENTS:	With HOLD, the keys are put in list to be typed later - after next char
					mode char or press/combine escape sequence.
					For example, if string is:      <esc>,hold,shift. a
					then the shift gets saved until after the "a" and the sequence goes like
					shift-down a-down, a-up, shift-up. The routines doPressLst() and
					doCombineLst() will type the hold keys at the appropriate time.  Here we
					simply put the keys in the kHold list for use later.

*/
void processKbdHold(void)
{
	int i;

	if (gCode==TERMCODE) {								/* term, transfer keys to kHold	*/
		for (i=0; i < tmpLst.len; kHold.list[kHold.len++]=tmpLst.list[i++]);
																/* note: if empty list, nop	*/
		tmpLst.len = 0;									/* clear out tmpLst	*/
		}
	else
		if (((tmpLst.len+kHold.len) < MAXLISTLEN)	/* if no room for code, ignore	*/
				&& validKCode()) {						/* if not illegal param, continue*/
																/* if LOCKed or HOLDed, ignore	*/
			if (!inLst(&kLock,gCode) && !inLst(&kHold,gCode))
				if (!inLst(&tmpLst,gCode))				/* duplicates NOT ok here */
					tmpLst.list[tmpLst.len++] = gCode;
			}
}



/****************************************************************************

	FUNCTION:	processKbdLock(void)

	SYNTAX:		<esc> <lock code> [param1] [,param2] [,param3] ... [,param5] <term code>
					at this point, we already have <lock code>

	COMMENTS:	With lock, the keys are put in list to be locked down i.e. only
					their down code is sent.  They are later released with a REL command.
					For example, if string is:      <esc>,lock,shift.
					the sequence goes like:   shift-down

*/
void processKbdLock(void)
{
	int i;
	BYTE temp;

	if (gCode == TERMCODE) {							/* if term, transfer keys to	*/
		for (i=0; i < tmpLst.len; i++) {				/*  kLock and send down codes.	*/
			temp = tmpLst.list[i];						/*  note: if empty list, nop	*/
			kLock.list[kLock.len++] = temp;
			(*scanDownTransVector)(temp);
			removeKeyFromHoldList(temp);				/* if key was in hold list	*/
			}													/*  remove cuz lock has priority	*/
		tmpLst.len = 0;									/* clear out tmpLst	*/
		}
	else if (((tmpLst.len+kLock.len)<MAXLISTLEN)	/* if no room for code, ignore	*/
				&& validKCode())							/* if not illegal param, continue*/
		if (!inLst(&kLock,gCode)) {					/* if LOCKed ignore	*/
			if (!inLst(&tmpLst,gCode))					/* duplicates NOT ok here */
				tmpLst.list[tmpLst.len++] = gCode;
			}
}



/****************************************************************************

	FUNCTION:	processKbdRel(void)

	SYNTAX:		<esc> <rel code> [param1] [,param2] [,param3] ... [,param5] <term code>
					at this point, we already have <rel code>

	COMMENTS:	REL is used to release keys that were LOCKed (i.e. remove them from
					kLock, and send their up codes) and to remove keys from the kHold list
					(i.e. remove them before they can be acted on).  You can release
					individual keys or all keys.  If no param is given, then all keys
					will be release/removed.

*/
void processKbdRel(void)
{
	int i;

	if (gCode == TERMCODE) {							/* if term, now process	*/
		if (!tmpLst.len) {								/* if list empty, rel all keys	*/
			for (i=0; i < kLock.len; (*scanUpTransVector)(kLock.list[i++]));
			kLock.len=0;									/* rel all locks	*/
			kHold.len = 0;									/* rel all holds	*/
			}
		else {												/* not empty so rel one by one	*/
			for (i=0; i<tmpLst.len; i++) {
				removeKeyFromLockList(tmpLst.list[i]);
				removeKeyFromHoldList(tmpLst.list[i]);
				}
			tmpLst.len = 0;								/* clear tmpList	*/
			}
		}
	else {													/* if no room for code, ignore	*/
		if (tmpLst.len<MAXLISTLEN && validKCode())	/* if not illegal param, continue*/

			/* Even if not LOCKed or HOLDed put key in list in case program and	*/
			/*  computer get out of synch		*/
			if (!inLst(&tmpLst,gCode))
				tmpLst.list[tmpLst.len++] = gCode;
		}
}




BOOL validKCode(void)
{
	int i;

	/* search table of valid codes for gCode */
	for (i=0; okKeyTbl[i] && (okKeyTbl[i] != gCode); i++);
	if (!okKeyTbl[i]) {
		errDetect();
		return FALSE;
		}
	else return TRUE;
}



BOOL inLst(struct listType *listPtr, BYTE searchChar)
{
	int i;
	BOOL found;

	found = FALSE;
	for (i=0; !found && (i < listPtr->len); i++)
		found = (listPtr->list[i] == searchChar);
	return (found);

}


void doPressLst(void)
{
	int i;

	/* notice, if lists are empty, nothing is done */
	for (i=0; i < kHold.len; (*scanDownTransVector)(kHold.list[i++]));
	for (i=0; i < tmpLst.len; i++) {
		(*scanDownTransVector)(tmpLst.list[i]);
		(*scanUpTransVector)(tmpLst.list[i]);
		}
	for (i=kHold.len; i > 0; (*scanUpTransVector)(kHold.list[--i]));
	kHold.len = tmpLst.len = 0;
	return;
}

void doCombineLst(void)
{
	int i;

	/* notice, if lists are empty, nothing is done */
	for (i=0; i < kHold.len; (*scanDownTransVector)(kHold.list[i++]));
	for (i=0; i < tmpLst.len; (*scanDownTransVector)(tmpLst.list[i++]));
	for (i=tmpLst.len; i > 0; (*scanUpTransVector)(tmpLst.list[--i]));
	for (i=kHold.len; i > 0; (*scanUpTransVector)(kHold.list[--i]));
	kHold.len = tmpLst.len = 0;
	return;
}

void removeKeyFromHoldList(BYTE theKey)
{
	int new,old;

	for (new=old=0; old < kHold.len; old++)
		if ((kHold.list[new] = kHold.list[old]) != theKey) new++;
	kHold.len = new;
}

void removeKeyFromLockList(BYTE theKey)
{
	int new,old;

	for (new=old=0; old < kLock.len; old++) {
		if ((kLock.list[new] = kLock.list[old]) != theKey) new++;
		else (*scanUpTransVector)(theKey);
		}
	kLock.len = new;
}



/****************************************************************************

	FUNCTION:

	PURPOSE:
				

	COMMENTS:

*/
void processKbd(void)
{
	switch (gCode) {
		case KBDINDICATORCODE:
			cmdVector = processKbdIndicator;
			aliasPtr = kbdIndicatorAliasTable;
			beginOK = TRUE;
			break;

		case KBDVERSIONCODE:
			cmdVector = processKbdVersion;
			aliasPtr = kbdVersionAliasTable;
			beginOK = TRUE;
			break;

		case KBDMODELCODE:
			cmdVector = processKbdModel;
			aliasPtr = kbdModelAliasTable;
			beginOK = TRUE;
			break;

		case KBDDESCRIPTIONCODE:
			cmdVector = processKbdDescription;
			aliasPtr = kbdDescriptionAliasTable;
			beginOK = TRUE;
			break;

/*		case KBDUNKNOWNCODE:
			cmdVector = processKbdUnknown;
			aliasPtr = kbdUnknownAliasTable;
			beginOK = TRUE;
			break;
*/
		default:
			if (gCode < LOWESTGIDEICODE) errDetect();
			else {
				errDetect();
/*				cmdVector = noOpRoutine;
				beginOK = TRUE;
*/
				}
			break;
		}
	return;
}


void processKbdIndicator(void)
{
	return;
}

void processKbdVersion(void)
{
	return;
}

void processKbdModel(void)
{
	return;
}

void processKbdDescription(void)
{
	return;
}

void processKbdUnknown(void)
{
	return;
}




