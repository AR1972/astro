/* init.C  */

#include "skdefs.h"
#include "gideidef.h"
#include "vars.h"
#include "serkeys.h"
#include "gide.h"
#include "init.h"

#include "drv.h"
#include "int9.h"
#include <dos.h>
#include <string.h>
#include <stdio.h>



/***********************/
void selectInjectMethod(void)
{
	scanDownTransVector = downScanPS2;
	scanUpTransVector = upScanPS2;
	switch (skCompId) {
		case 8:
		case 7:
			/* D2 inject / int 15h 4Fh intercept monitor */
			injectKeysVector = D2InjectRoutine;
			break;

		case 2:
		case 90:
			/* write 3F / read 3F / replace / int 15h 4Fh intercept monitor */
			injectKeysVector = write3fInjectRoutine;
			break;

		case 91:
			/* 20 command byte read/ int 15h 4Fh intercept replace */
			injectKeysVector = write20InjectRoutine;
			break;

		case 6:
			/* write 60h / soft Int 9h / int 15h 4Fh intercept monitor */
			/* model 25/30 - 8086 */
			injectKeysVector = write60InjectRoutine;
			break;

		case 26:
			/* soft Int 9h / int 15h 4Fh intercept replace */
			injectKeysVector = int15InjectRoutine;
			break;

		case 5:
		case 4:
		case 3:
		case 1:
		default:
			/* keyboard buffer inject */
			scanDownTransVector = downScanAT;
			scanUpTransVector = upScanAT;
			injectKeysVector = kbdBufferInjectKeysRoutine;
			break;
		}
}

/***********************/

void serialKeysInit(void)
{
	skCompId = comp_id;

	selectInjectMethod();

	fatalErrorFlag = FALSE;
	handleFatalError = FALSE;
	needTimerHelp = FALSE;

	serialKeysStartupInit();
	serialKeysCommInit();
}

/**************************************************************************/
void serialKeysStartupInit(void)
{
	feCount = 0;

	initGIDEI();

	waitForInt9Timeout = INT9TIMEOUT;
	sendSoftHandshakeStatus = HANDSHAKETIMEOUT;
	timerCount = TIMERTIMEOUT;
	_asm mov byte ptr cs:forcedInt9Flag,FALSE
	inPauseFlag = FALSE;
	weInjectedFlag = FALSE;
	inSerialKeys = FALSE;
	tryingToWriteKeyboardData = FALSE;

	serialKeysStateInit();
	_asm mov byte ptr cs:injectByte,0
	putPosSBuf = commBuf;
	getPosSBuf = commBuf;
	numCommChars = 0;
	mouBufTailPtr = mouBuffer;
	mouBufHeadPtr = mouBuffer;

}

/**************************************************************************/
void serialKeysStateInit(void)
{
	mouseX = 0;
	mouseY = 0;
	mouseState = 0;

	kHold.len = 0;
	kLock.len = 0;


	kState.rShift = 0;
	kState.lShift = 0;
	kState.rCtrl = 0;
	kState.lCtrl = 0;
	kState.rAlt = 0;
	kState.lAlt = 0;
	getBiosFlags();
	if (kbFlag & NUM_MODE_MASK) kState.numlck = 1;
	else kState.numlck = 0;
	kbFlag &= ~(ALT_DOWN_MASK | CTRL_DOWN_MASK | LSHIFT_DOWN_MASK | RSHIFT_DOWN_MASK);
	kbFlag1 &= ~(LALT_DOWN_MASK | LCTRL_DOWN_MASK);
	kbFlag2 &= ~(RALT_DOWN_MASK | RCTRL_DOWN_MASK);
	putBiosFlags();

}


/**************************************************************************/
void initGIDEI(void)
{
	asciiTblPtr = asciiTable;
	aliasPtr = nullTable;
	serVector = charHandler;
	codeVector = processGideiCode;
	cmdVector = processCommand;
	ptrStackPtr = ptrStack;
	spos = 0;
	rpos = 0;
	putPosOBuf = outputBuf;
	getPosOBuf = outputBuf;
	mouBufTailPtr = mouBuffer;
	mouBufHeadPtr = mouBuffer;
	dataBlk.id = 0;
	lastCode = 0;
	serByte = 0;
	aliasStr[0] = '\0';
	gCode = 0;
	tmpLst.len = 0;
	passAll = FALSE;
	stdErrorFlag = FALSE;
	waitForClear = FALSE;
	beginOK = FALSE;
	tmpStatus = 0;
	tmpDist = 0;
	nullCount = 0;
	byteCount = 0;
	tempskBaudRate = skBaudRate;
}


/**************************************************************************/
void serialKeysBegin(void)
{

	if ((!passAll) && (serByte == NULLCHAR)) {	/* only if passAll = false */
		if (inp(MCR+skCommPortAddr) & (RTS_BIT | DTR_BIT))
			outp(THR+skCommPortAddr,XON);
		doBeep();
		if (++nullCount >= 3) {
			doBeep();
			_asm cli
			initGIDEI();									/* if 3 nulls then reset */
			disableKeyEnhance();
			_asm sti
			}
		else errorCode(0);								/* just beep if no reset */
		}
	else {
		nullCount = 0;										/* reset null count */
		(*serVector)();									/* vector to current routine */
		if (stdErrorFlag) {
			doBeep();
			_asm cli
			initGIDEI();
			_asm sti
			}
		}
}




/****************************************************************************/
void errorCode(BYTE errorNum)
{
	return;
}



/****************************************************************************

	FUNCTION:	writeCommPort(void)

	PURPOSE:	

	COMMENTS:	outputs string to comm port

*/

void writeCommPort(void)
{
	return;
}


/****************************************************************************/
void errDetect(void)
{
	errorCode(3);
	_asm cli
	initGIDEI();
	_asm sti
	doBeep();
	return;
}




