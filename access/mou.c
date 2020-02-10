/* MOU.C */

#include "skdefs.h"
#include "gideidef.h"
#include "vars.h"
#include "mou.h"
#include "drv.h"
#include "init.h"

/************************************************************/
void processMouClick(void)
{
	int i;

	switch (gCode) {
		case TERMCODE:
			if (tmpStatus == 0) 							/* select default	*/
				tmpStatus = LEFTBUTTONMASK;
			if ((tmpStatus = (mouseState | tmpStatus)) != mouseState) {

				/* User is requesting clicking at least one button that isn't */
				/*  locked */
				for (i=0; i < kHold.len; (*scanDownTransVector)(kHold.list[i++]));
				dataBlk.id = MOUSEID;
				dataBlk.mou.deltaX = 0;
				dataBlk.mou.deltaY = 0;
				dataBlk.mou.status = tmpStatus;
				putInOutputBuf();
				dataBlk.mou.status = mouseState;		/* return to previous state	*/
				putInOutputBuf();
				for (i=kHold.len; i > 0; (*scanUpTransVector)(kHold.list[--i]));
				kHold.len = 0;
				}
			tmpStatus = 0;
			beginOK = TRUE;
			break;
		case LEFTBUTTONCODE:
			tmpStatus |= LEFTBUTTONMASK;
			break;
		case RIGHTBUTTONCODE:
			tmpStatus |= RIGHTBUTTONMASK;
			break;
		default:
			errDetect();
			break;
		}
}

/************************************************************/
void processMouDoubleClick(void)
{
	int i;

	if (gCode == TERMCODE) {
		if (tmpStatus == 0) 								/* select default	*/
			tmpStatus = LEFTBUTTONMASK;
		if ((tmpStatus = (mouseState | tmpStatus)) != mouseState) {
			/* User is requesting clicking at least one button that isn't */
			/*  locked */
			for (i=0; i < kHold.len; (*scanDownTransVector)(kHold.list[i++]));
			dataBlk.id = MOUSEID;
			dataBlk.mou.deltaX = 0;
			dataBlk.mou.deltaY = 0;
			dataBlk.mou.status = tmpStatus;
			putInOutputBuf();
			dataBlk.mou.status = mouseState;		/* return to previous state	*/
			putInOutputBuf();
			dataBlk.mou.status = tmpStatus;
			putInOutputBuf();
			dataBlk.mou.status = mouseState;		/* return to previous state	*/
			putInOutputBuf();
			for (i=kHold.len; i > 0; (*scanUpTransVector)(kHold.list[--i]));
			kHold.len = 0;
			}
		tmpStatus = 0;
		beginOK = TRUE;
		}
	else processMouClick();
}

/************************************************************/
void processMouLock(void)
{
	int i;

	if (gCode == TERMCODE) {
		if (tmpStatus == 0) 								/* select default	*/
			tmpStatus = LEFTBUTTONMASK;
		if ((tmpStatus = (mouseState | tmpStatus)) != mouseState) {
			/* User is requesting locking at least one button that isn't */
			/*  locked */
			for (i=0; i < kHold.len; (*scanDownTransVector)(kHold.list[i++]));
			dataBlk.id = MOUSEID;
			dataBlk.mou.deltaX = 0;
			dataBlk.mou.deltaY = 0;
			dataBlk.mou.status = tmpStatus;
			putInOutputBuf();
			for (i=kHold.len; i > 0; (*scanUpTransVector)(kHold.list[--i]));
			kHold.len = 0;
			mouseState = tmpStatus;						/* store new state	*/
			}
		tmpStatus = 0;
		beginOK = TRUE;
		}
	else processMouClick();
}


/************************************************************/
void processMouRel(void)
{
	if (gCode == TERMCODE) {
		if (tmpStatus == 0) 								/* select all	*/
			tmpStatus = (LEFTBUTTONMASK | RIGHTBUTTONMASK);
		if ((tmpStatus = (mouseState & (~tmpStatus))) != mouseState) {
			/* User is requesting releasing at least one button that is */
			/*  locked */
			dataBlk.id = MOUSEID;
			dataBlk.mou.deltaX = 0;
			dataBlk.mou.deltaY = 0;
			dataBlk.mou.status = tmpStatus;
			putInOutputBuf();
			mouseState = tmpStatus;						/* store new state	*/
			}
		tmpStatus = 0;
		beginOK = TRUE;
		}
	else processMouClick();
}

/************************************************************/
void processMouReset(void)
{
	if (gCode == TERMCODE) {
		dataBlk.id = MOUSEID;
		dataBlk.mou.deltaX = -1000;
		dataBlk.mou.deltaY = -1000;
		dataBlk.mou.status = 0;
		putInOutputBuf();
		mouseX = mouseY = 0;								/* store new state	*/
		mouseState = 0;
		beginOK = TRUE;
		}
	else errDetect();
}


/************************************************************/
void processMouMove(void)
{
	switch (gCode) {
		case TERMCODE:
			if (tmpLst.len < 4)
				for ( ; tmpLst.len >= 4; tmpLst.list[tmpLst.len++] = 0);
			moveTheMouseRelative();
			tmpLst.len = 0;
			beginOK = TRUE;
			break;
		case BYTECODE:
			cmdVector = collectMoveByte;
			beginOK = FALSE;
			break;
		case INTEGERCODE:
			cmdVector = collectMoveInteger;
			beginOK = FALSE;
			break;
		default:
			errDetect();
			break;
		}
}


/************************************************************/
void collectMoveByte(void)
{
	if (tmpLst.len >= 4) errDetect();
	else {
		tmpLst.list[tmpLst.len++] = gCode;
		tmpLst.list[tmpLst.len++] = 0;
		cmdVector = processMouMove;
		}
}


/************************************************************/
void collectMoveInteger(void)
{
	if (tmpLst.len >= 4) errDetect();
	else {
		tmpLst.list[tmpLst.len++] = gCode;
		if ((tmpLst.len == 2) || (tmpLst.len == 4)) cmdVector = processMouMove;
		}
}

/************************************************************/
void moveTheMouseRelative(void)
{
	int i;

	for (i=0; i < kHold.len; (*scanDownTransVector)(kHold.list[i++]));

	tmpDist = (tmpLst.list[1] << 8) + tmpLst.list[0];
	dataBlk.mou.deltaX = tmpDist;
	tmpDist += mouseX;
	mouseX = (tmpDist < 0) ? 0 : tmpDist;

	tmpDist = (tmpLst.list[3] << 8) + tmpLst.list[2];
	dataBlk.mou.deltaY = tmpDist;
	tmpDist += mouseY;
	mouseY = (tmpDist < 0) ? 0 : tmpDist;

	dataBlk.mou.status = mouseState;
	dataBlk.id = MOUSEID;
	putInOutputBuf();

	for (i=kHold.len; i > 0; (*scanUpTransVector)(kHold.list[--i]));
	kHold.len = 0;
}



/************************************************************/
void processMouGoto(void)
{
	switch (gCode) {
		case TERMCODE:
			if (tmpLst.len < 4)
				for ( ; tmpLst.len >= 4; tmpLst.list[tmpLst.len++] = 0);
			moveTheMouseAbsolute();
			tmpLst.len = 0;
			beginOK = TRUE;
			break;
		case BYTECODE:
			cmdVector = collectGotoByte;
			beginOK = FALSE;
			break;
		case INTEGERCODE:
			cmdVector = collectGotoInteger;
			beginOK = FALSE;
			break;
		default:
			errDetect();
			break;
		}
}

/************************************************************/
void collectGotoByte(void)
{
	if (tmpLst.len >= 4) errDetect();
	else {
		tmpLst.list[tmpLst.len++] = gCode;
		tmpLst.list[tmpLst.len++] = 0;
		cmdVector = processMouGoto;
		}
}

/************************************************************/
void collectGotoInteger(void)
{
	if (tmpLst.len >= 4) errDetect();
	else {
		tmpLst.list[tmpLst.len++] = gCode;
		if ((tmpLst.len == 2) || (tmpLst.len == 4)) cmdVector = processMouGoto;
		}
}

/************************************************************/
void moveTheMouseAbsolute(void)
{
	int i;

	for (i=0; i < kHold.len; (*scanDownTransVector)(kHold.list[i++]));

	tmpDist = (tmpLst.list[1] << 8) + tmpLst.list[0];
	dataBlk.mou.deltaX = tmpDist - mouseX;
	mouseX = (tmpDist < 0) ? 0 : tmpDist;

	tmpDist = (tmpLst.list[3] << 8) + tmpLst.list[2];
	dataBlk.mou.deltaY = tmpDist - mouseY;
	mouseY = (tmpDist < 0) ? 0 : tmpDist;

	dataBlk.mou.status = mouseState;
	dataBlk.id = MOUSEID;
	putInOutputBuf();

	for (i=kHold.len; i > 0; (*scanUpTransVector)(kHold.list[--i]));
	kHold.len = 0;
}


/************************************************************/
void processMou(void)
{
	errDetect();
}
