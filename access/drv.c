/*  drv.c  */

#include "skdefs.h"
#include "gideidef.h"
#include "vars.h"
#include "drv.h"
#include "serkeys.h"
#include "init.h"
#include "int9.h"
#include <dos.h>
#include <stdio.h>
#include <conio.h>
#include <stdlib.h>



/*******************************/
void kickStartSerialKeys(void)
{
	BYTE biosPauseFlag;

	/* take care of mouse stuff */
	sendMouseData();

	/*
		This section is to make sure the serial int does not get hung up if the int 15h does not run.
		This was put here especially for when you are running serialkey in a DOS shell under windows.  When you
		send the keys to exit back to Windows, Windows traps them,
	*/
	_asm cmp byte ptr cs:forcedInt9Flag,TRUE
	_asm jne kickDone
		if (!(--waitForInt9Timeout)) {
			_asm mov byte ptr cs:forcedInt9Flag,FALSE
			tryingToWriteKeyboardData = FALSE;
			initGIDEI();
			putPosSBuf = commBuf;
			getPosSBuf = commBuf;
			numCommChars = 0;
			waitForInt9Timeout = INT9TIMEOUT;
			}
kickDone:


	biosPauseFlag = inPauseCondition();

	/* this will get serial keys running again if we injected a key that caused */
	/* a pause to occur.  Serial keys will then handle the pause itself. */
	if (inSerialKeys && biosPauseFlag) {
		if (weInjectedFlag) {
			inPauseFlag = TRUE;
			clearInPause();
			}
		/* this will clear a pause that occurred from the keyboard while we were in*/
		/* the serial keys routine.  The typist will just have to do it again. */
		else if (!prevPauseState && !inPauseFlag) clearInPause();
		}

	if (!fatalErrorFlag && (keyBufferEmpty() || biosPauseFlag)) {

		/* will turn handshaking back on if we exited because the keyboard buffer was full */
		/* and all other buffers were empty */
		if (needTimerHelp) {
	 		needTimerHelp = FALSE;
			if (!inSerialKeys && !numCommChars) turnOnHandshake();
			}
		/* will get serial keys going if we exited for whatever reason and serial chars stopped */
		/* but our buffers still have chars to process */
		else if (!inSerialKeys && (numCommChars || (putPosOBuf != getPosOBuf))) {
			if (!(--timerCount)) {
				if (inp(LSR+skCommPortAddr) & THRE_FLAG) {
					if (inp(MCR+skCommPortAddr) & (RTS_BIT | DTR_BIT))
						outp(THR+skCommPortAddr,XON);
					else
						outp(THR+skCommPortAddr,XOFF);
					}
				outp(IER+skCommPortAddr,inp(IER+skCommPortAddr) | INT_THRE);
				sendSoftHandshakeStatus = HANDSHAKETIMEOUT;
				}
			}
		}

/*	if (!(--sendSoftHandshakeStatus)) {
		if (inp(MCR+skCommPortAddr) & (RTS_BIT | DTR_BIT))
			outp(THR+skCommPortAddr,XON);
		else
			outp(THR+skCommPortAddr,XOFF);
		sendSoftHandshakeStatus = HANDSHAKETIMEOUT;
		}
*/

}


/*******************************/

/*******************************/
void doSerial (void)
{

	BOOL inIntFlag, inPauseTemp;
	BOOL doneWithLoop;
	BYTE temp;

	inIntFlag = inSerialKeys;							/* get flag to see if 1st instance already active */
	inSerialKeys = TRUE;									/* set global because if we were not already active, we are now */
	timerCount = TIMERTIMEOUT;
	sendSoftHandshakeStatus = HANDSHAKETIMEOUT;
	_asm cli
	temp = inp(IIR+skCommPortAddr) & 7;				/* get interrupt id */
	outp(IER+skCommPortAddr,inp(IER+skCommPortAddr) & (INT_RCV | INT_RLS));		/* clear out int on THRE in case it is set */
	if ((temp == 4) || (temp == 6)) {				/* 4=receive char   6=error */

		/* store character and status if room available */
		if (numCommChars < COMMBUFLEN) {		
			putPosSBuf->serChar = inp(RBR+skCommPortAddr);
			putPosSBuf->status = (temp == 6) ? inp(LSR+skCommPortAddr) : 0;
			putPosSBuf = (putPosSBuf+1 >= (commBuf+COMMBUFLEN)) ? commBuf : putPosSBuf+1;
			numCommChars++;
			if (numCommChars >= DEACTHANDSHAKE) turnOffHandshake();
			}
		/* if no room, just clear interrupt */
		else if (temp == 6) inp(LSR+skCommPortAddr);
		else inp(RBR+skCommPortAddr);
		}
	_asm cli
	if (tryingToWriteKeyboardData)					/* do send EOI if in this mode */
		tryingToWriteKeyboardData = FALSE;
	else
		outp(0x20,0x20);									/* reset int controller so serial ints can come it again */

	if (!waitingForIndicatorUpdate() && !inIntFlag) {
		inPauseFlag = FALSE;
		prevPauseState = inPauseCondition();
		_asm sti;
		if (!needTimerHelp || prevPauseState) {
			doneWithLoop = false;
			do {
				/* clear out output buffer before processing anymore serial chars */
				while ((putPosOBuf != getPosOBuf) && (inPauseFlag || !keyBufferFull())) {
					if (mouBufHeadPtr != mouBufTailPtr) {
						doneWithLoop = true;
						break;
						}
					else {
						timerCount = TIMERTIMEOUT;
						sendSoftHandshakeStatus = HANDSHAKETIMEOUT;
						getOutputBufChar();
						if (dataBlk.id == MOUSEID) injectMouse();
						else if ((dataBlk.id == KEYBDID) && (dataBlk.scanCode != 0)) {
							if (injectKeysVector == kbdBufferInjectKeysRoutine)
								(*injectKeysVector)(dataBlk.scanCode);
							else if ((injectKeysVector == write60InjectRoutine) || (injectKeysVector == int15InjectRoutine))
								softInt9InjectKeysRoutine(dataBlk.scanCode);
							else hardwareInjectKeysRoutine(dataBlk.scanCode);
							}
						}
					}
				timerCount = TIMERTIMEOUT;
				sendSoftHandshakeStatus = HANDSHAKETIMEOUT;
				if (keyBufferFull() && !inPauseFlag) {
					turnOffHandshake();
					doneWithLoop = TRUE;
					needTimerHelp = TRUE;
					}
				/* if output buffer is empty, we can finally get serial chars */
				else if ((putPosOBuf == getPosOBuf) && (numCommChars)) {
					if (getCommByte()) serialKeysBegin();
					}
				else doneWithLoop = true;
				}
			while ((inPauseCondition() && inPauseFlag) || !doneWithLoop);
			}
		_asm cli;
		inPauseFlag = FALSE;
		inSerialKeys = FALSE;
		}
}



/************************/
BOOL getCommByte(void)
{
	BOOL temp;
	struct serialDataType tempData;

	if (temp = numCommChars) {
		_asm cli;
		numCommChars--;
		tempData = *getPosSBuf;
		getPosSBuf = (getPosSBuf+1 >= (commBuf+COMMBUFLEN)) ? commBuf : getPosSBuf+1;
		_asm sti;
		if (tempData.status) {							/* errors found */
			if (tempData.status & (BI_FLAG | FE_FLAG)) {
				if ((++feCount) >= 3) {
					_asm cli;
					handleFatalError = TRUE;
					disableComm();
					turnOffHandshake();
					timerCount = TIMERTIMEOUT;
					clearOutComm();
					_asm sti;
					disableKeyEnhance();
					initGIDEI();
					putPosSBuf = commBuf;
					getPosSBuf = commBuf;
					numCommChars = 0;
					waitForInt9Timeout = INT9TIMEOUT;
					doBeep();
					doBeep();
					doBeep();
					while (!(inp(LSR+skCommPortAddr) & (THRE_FLAG | TSRE_FLAG)));
					if (!singleUserSetup) skBaudRate = BAUD300;
					setBaudRate();
					clearOutComm();
					serialKeysStateInit();
					_asm cli
					feCount = 0;
					enableComm();
					turnOnHandshake();
					handleFatalError = FALSE;
					_asm sti;
					}
				doBeep();
				}
			else doBeep();
			}
		else {
			feCount = 0;
			serByte = tempData.serChar;
			}
		if (temp <= ACTHANDSHAKE) {
			if (!(inp(MCR+skCommPortAddr) & (RTS_BIT | DTR_BIT)))
				turnOnHandshake();
			}
		return (!tempData.status);
		}
	else return (FALSE);

}


	
/*******************************************************/
void hardwareInjectKeysRoutine(BYTE scanCode) 	/* part of keyboard driver */
{
	int timeOut;

	timeOut = 0x100;
	while (--timeOut);
	waitForInt9Timeout = INT9TIMEOUT;
	if (inPauseCondition()) weInjectedFlag = FALSE;
	else {
		inPauseFlag = FALSE;
		weInjectedFlag = TRUE;
		}
	_asm {
		sti
		sub	cx,cx
hardlp1:
		in		al,64h
		jmp	$+2
		test	al,2
		loopnz	hardlp1
		}
		(*injectKeysVector)(scanCode);
	_asm {
		sti
hardlp3:
		mov	al,cs:serialKeysOn
		or		al,al
		jnz	hardlp4
		mov	cs:forcedInt9Flag,0
hardlp4:
//		cmp BYTE PTR cs:forcedInt9Flag,0
//		jne hardlp3
		cli
		}
	if (weInjectedFlag && inPauseFlag)
		setInPause();
	weInjectedFlag = FALSE;
	_asm sti
	return;
}

/*******************************************************/
void D2InjectRoutine(BYTE scanCode)
{
	_asm {
		cli
		mov	al,0d2h
		out	64h,al
		jmp	$+2
		mov	al,scanCode
		mov	BYTE PTR cs:injectByte,al
		mov	BYTE PTR cs:forcedInt9Flag,1;
		sub	cx,cx
D2lp2:
		in		al,64h
		jmp	$+2
		test	al,2
		loopnz	D2lp2
		mov	al,scanCode
		out	60h,al
		sti
		}
}

/*******************************************************/
void write3fInjectRoutine(BYTE scanCode)
{
	_asm {
		cli
		mov	al,07Fh
		out	64h,al
		jmp	$+2
		sub	cx,cx
althardlp2:
		in		al,64h
		jmp	$+2
		test	al,3
		loopnz	althardlp2
		mov	al,scanCode
		out	60h,al
		sti
		mov	BYTE PTR cs:injectByte,al
		mov	BYTE PTR cs:forcedInt9Flag,1;
		sub	cx,cx
althardlp3:
		in		al,64h
		jmp	$+2
		test	al,3
		loopnz	althardlp3
		mov	al,03Fh
		out	64h,al
		sti

althardlp4:
;		in		al,64h
;		jmp	$+2
;		test	al,3
;		loopnz	althardlp4
;		sub	cx,cx
;althardlp5:
;		in		al,64h
;		jmp	$+2
;		test	al,3
;		loopz	althardlp5
;		cli

		}
}

/*******************************************************/
void write20InjectRoutine(BYTE scanCode)
{
	_asm {
		mov	al,scanCode
		mov	BYTE PTR cs:injectByte,al
		mov	BYTE PTR cs:forcedInt9Flag,1;
		mov	al,020h
		out	64h,al
		}
}

/*******************************************************/
void softInt9InjectKeysRoutine(BYTE scanCode)
{
	waitForInt9Timeout = INT9TIMEOUT;
	_asm cli
	if (inp(MCR+skCommPortAddr) & (RTS_BIT | DTR_BIT))
		outp(THR+skCommPortAddr,XON);
	else
		outp(THR+skCommPortAddr,XOFF);
	outp(IER+skCommPortAddr,inp(IER+skCommPortAddr) | INT_THRE);			 /* set to interrupt on receive char, error and THRE */
	tryingToWriteKeyboardData = TRUE;
	_asm sti
	while (tryingToWriteKeyboardData);
	waitForInt9Timeout = INT9TIMEOUT;
	if (inPauseCondition()) weInjectedFlag = FALSE;
	else {
		inPauseFlag = FALSE;
		weInjectedFlag = TRUE;
		}
	_asm {
		cli
		mov	al,scanCode
		mov	BYTE PTR cs:injectByte,al
		mov	BYTE PTR cs:forcedInt9Flag,1;
		}
	(*injectKeysVector)(scanCode);
	_asm	sti
	_asm	int	9h

	if (weInjectedFlag && inPauseFlag)
		setInPause();
	weInjectedFlag = FALSE;
	_asm sti
	return;
}

/*******************************************************/
void write60InjectRoutine(BYTE scanCode)
{
	_asm	mov	al,scanCode
	_asm	out	60h,al
}

/*******************************************************/
void int15InjectRoutine(BYTE scanCode)
{
}


/************************/
void putInOutputBuf(void)
{
	/* putPosOBuf points to next free storage spot */
	/* If putPosOBuf+1 = getPosOBuf then buffer is full*/
	/* also, if putPosOBuf+1 = end of codebuffer then rap around */
	/* to begin unless getPosOBuf=begin cuz buffer is full */

	if (!((putPosOBuf+1 == getPosOBuf) || ((putPosOBuf+1 >= (outputBuf+OUTPUTBUFLEN)) && (getPosOBuf == outputBuf)))) {
		*putPosOBuf = dataBlk;
		putPosOBuf = (putPosOBuf+1 >= (outputBuf+OUTPUTBUFLEN)) ? outputBuf : putPosOBuf+1;
		}
}


/************************/

void getOutputBufChar(void)
{
	if (putPosOBuf != getPosOBuf) {
		dataBlk.id = getPosOBuf->id;
		dataBlk.mou.status = getPosOBuf->mou.status;
		dataBlk.mou.deltaX = getPosOBuf->mou.deltaX;
		dataBlk.mou.deltaY = getPosOBuf->mou.deltaY;
		getPosOBuf = (getPosOBuf+1 >= (outputBuf+OUTPUTBUFLEN)) ? outputBuf : getPosOBuf++;
		}
	return;
}

/************************/
BOOL keyInTbl(BYTE searchElement, BYTE * tblPtr)
{
	for (;(*tblPtr != searchElement) && (*tblPtr != 0); tblPtr++);
	if (*tblPtr == searchElement) return (TRUE);
	else return(FALSE);
}

/************************/
BOOL suppressCode (BYTE keyCode)
{
	/* extended keyboard doesn't send out codes for some keys when both alt keys are down */
	if (kState.lAlt && kState.rAlt && keyInTbl(keyCode,altSuppressedTbl)) return (TRUE);

	/* extended keyboard doesn't send out codes for some keys when both control keys are down */
	else if (kState.lCtrl && kState.rCtrl && keyInTbl(keyCode,ctrlSuppressedTbl)) return (TRUE);

	/* extended keyboard doesn't send out codes for some keys when both shift keys are down */
	else if (kState.lShift && kState.rShift && keyInTbl(keyCode,shiftSuppressedTbl)) return(TRUE);

	else return (FALSE);
}




/*******************************************************/
void upScanPS2 (BYTE keynum)
{

	BYTE *tempPtr;
	BYTE temp;

	dataBlk.id = KEYBDID;

	switch (keynum)
		{
		case 57:
			kState.rShift = FALSE;
			break;
		case 44:
			kState.lShift = FALSE;
			break;
		case 64:
			kState.rCtrl = FALSE;
			break;
		case 58:
			kState.lCtrl = FALSE;
			break;
		case 62:
			kState.rAlt = FALSE;
			break;
		case 60:
			kState.lAlt = FALSE;
			break;
		case 90:
/*			kState.numlck = FALSE;*/
			break;
		}
	keynum &= 0x7F;

	if (suppressCode(keynum)) return;

	temp = IBMextendedScanCodeSet1[keynum] | 0x80;
	switch (keynum)
		{
		case 62:
		case 64:
		case 108:
			dataBlk.scanCode = 0xE0;
			putInOutputBuf ();
			dataBlk.scanCode = temp;
			putInOutputBuf ();
			break;

		case 75:
		case 76:
		case 79:
		case 80:
		case 81:
		case 83:
		case 84:
		case 85:
		case 86:
		case 89:
		case 95:
			dataBlk.scanCode = 0xE0;
			putInOutputBuf ();
			dataBlk.scanCode = temp;
			putInOutputBuf ();
			if (kState.numlck && (keynum != 95))
				{
				if (!(kState.rShift || kState.lShift))
					{
					dataBlk.scanCode = 0xE0;
					putInOutputBuf ();
					dataBlk.scanCode = 0xAA;
					putInOutputBuf ();
					}
				}
			else if (!kState.rShift && kState.lShift)
				{
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0x2A;
				putInOutputBuf ();
				}
			else if (kState.rShift && !kState.lShift)
				{
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0x36;
				putInOutputBuf ();
				}
			else if (kState.rShift && kState.lShift)
				{
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0x36;
				putInOutputBuf ();
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0x2A;
				putInOutputBuf ();
				}
			break;
	
		case 124:
			if (kState.rAlt || kState.lAlt) {
				dataBlk.scanCode = 0xD4;
				putInOutputBuf ();
				}
			else if (kState.rCtrl || kState.lCtrl || kState.rShift || kState.lShift)
				{
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0xB7;
				putInOutputBuf ();
				}
			else
				{
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0xB7;
				putInOutputBuf ();
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0xAA;
				putInOutputBuf ();
				}
			break;
	
		case 126:
			break;
	
		default:
			if (temp != 0x80) {
				dataBlk.scanCode = temp;
				putInOutputBuf ();
				}
			break;
		}
	return;
}
	
	
	
	
/*******************************************************/
void downScanPS2 (BYTE keynum)
{
	BYTE *tempPtr;

	dataBlk.id = KEYBDID;

	switch (keynum)
		{
		case 57:
			kState.rShift = TRUE;
			break;
		case 44: 
			kState.lShift = TRUE;
			break;
		case 64: 
			kState.rCtrl = TRUE;
			break;
		case 58: 
			kState.lCtrl = TRUE;
			break;
		case 62: 
			kState.rAlt = TRUE;
			break;
		case 60:
			kState.lAlt = TRUE;
			break;
		case 90: 
			kState.numlck = ~kState.numlck;
			break;
		}

	keynum &= 0x7F;

	if (suppressCode(keynum)) return;

	switch (keynum)
		{

		case 62:
		case 64:
		case 108:
			dataBlk.scanCode = 0xE0;
			putInOutputBuf ();
			break;
		
		case 75:
		case 76:
		case 79:
		case 80:
		case 81:
		case 83:
		case 84:
		case 85:
		case 86:
		case 89:
		case 95:
			dataBlk.scanCode = 0xE0;
			putInOutputBuf ();
			if (kState.numlck && (keynum != 95))
				{
				if (!(kState.rShift || kState.lShift)) 
					{
					dataBlk.scanCode = 0x2A;
					putInOutputBuf ();
					dataBlk.scanCode = 0xE0;
					putInOutputBuf ();
					}
				}
			else if (!kState.rShift && kState.lShift)
				{
				dataBlk.scanCode = 0xAA;
				putInOutputBuf ();
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				}
			else if (kState.rShift && !kState.lShift)
				{
				dataBlk.scanCode = 0xB6;
				putInOutputBuf ();
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				}
			else if (kState.rShift && kState.lShift)
				{
				dataBlk.scanCode = 0xAA;
				putInOutputBuf ();
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0xB6;
				putInOutputBuf ();
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				}
			break;
	
		case 124:
			if (kState.rAlt || kState.lAlt) {
				dataBlk.scanCode = 0x54;
				putInOutputBuf ();
				}
			else if (kState.rCtrl || kState.lCtrl || kState.rShift || kState.lShift)
				{
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0x37;
				putInOutputBuf ();
				}
			else
				{
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0x2A;
				putInOutputBuf ();
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0x37;
				putInOutputBuf ();
				}
			break;
	
		case 126:
			if (kState.rCtrl || kState.lCtrl)
				{
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0x46;
				putInOutputBuf ();
				dataBlk.scanCode = 0xE0;
				putInOutputBuf ();
				dataBlk.scanCode = 0xC6;
				putInOutputBuf ();
				}
			else
				{
				dataBlk.scanCode = 0xE1;
				putInOutputBuf ();
				dataBlk.scanCode = 0x1D;
				putInOutputBuf ();
				dataBlk.scanCode = 0x45;
				putInOutputBuf ();
				dataBlk.scanCode = 0xE1;
				putInOutputBuf ();
				dataBlk.scanCode = 0x9D;
				putInOutputBuf ();
				dataBlk.scanCode = 0xC5;
				putInOutputBuf ();
				}
			break;
		}
	dataBlk.scanCode = IBMextendedScanCodeSet1[keynum];
	putInOutputBuf ();
}


/*******************************************************/
void downScanAT (BYTE keynum)
{
	dataBlk.id = KEYBDID;
	dataBlk.scanCode = keynum;
	putInOutputBuf ();
}


/*******************************************************/
void upScanAT (BYTE keynum)
{
	dataBlk.id = KEYBDID;
	dataBlk.scanCode = (BYTE)(keynum | 0x80);
	putInOutputBuf ();
}



/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
/********************************************************************************/
/*****
Serial Mouse
(serial format = 7 data bits)

           7  6  5  4  3  2  1  0
           |  |  |  |  |  |  |  |
           |  |  |  |  |  |  |  |-- D6 of X
           |  |  |  |  |  |  |
           |  |  |  |  |  |  |----- X sign bit (positive right)
           |  |  |  |  |  |
           |  |  |  |  |  |-------- D6 of Y
           |  |  |  |  |
           |  |  |  |  |----------- Y sign bit (positive up)
           |  |  |  |
           |  |  |  |-------------- Right Button (0=up 1=down)
           |  |  |
           |  |  |----------------- Left Button (0=up  1=down)
           |  |
           |  |-------------------- 1  1st packet of 3  ( 0 for packets 2 and 3)
           |
           |----------------------- 0

PS2 Mouse

byte order:   status/sign, X, Y

           7  6  5  4  3  2  1  0
           |  |  |  |  |  |  |  |
           |  |  |  |  |  |  |  |-- Left Button (0=up  1=down)
           |  |  |  |  |  |  |
           |  |  |  |  |  |  |----- Right Button (0=up 1=down)
           |  |  |  |  |  |
           |  |  |  |  |  |-------- 0
           |  |  |  |  |
           |  |  |  |  |----------- 1                       
           |  |  |  |                                                         
           |  |  |  |-------------- X sign bit (positive right)
           |  |  |                                                          
           |  |  |----------------- Y sign bit (positive up)
           |  |   
           |  |-------------------- 0 (1 if X overflow)                        
           |
           |----------------------- 0 (1 if Y overflow)
********************************************************************************/

void injectMouse(void)
{
	switch (fmouse_id) {
		case PS2MOUSE:
			injectPS2Mouse();
			break;
		case SERIALMOUSE:
			if (combase)
				injectSerialMouse();
			break;
		default:
			break;
		}
}


/*******************************************************************************/
void injectSerialMouse(void)
{
	volatile BYTE tmp;
	BYTE tmpStatus;
	signed char tempX,tempY;

	/* Can only send -128 to +127 at a time so must repeat until all is sent */
	do {
		if (dataBlk.mou.deltaX < -128) tempX = -128;
		else if (dataBlk.mou.deltaX > 127) tempX = 127;
		else tempX = dataBlk.mou.deltaX;
		dataBlk.mou.deltaX -= tempX;

		if (dataBlk.mou.deltaY < -127) tempY = -127;	/* use 127 because -(-128) undefined */
		else if (dataBlk.mou.deltaY > 127) tempY = 127;
		else tempY = (signed char) dataBlk.mou.deltaY;
		dataBlk.mou.deltaY -= tempY;

		tmpStatus = (dataBlk.mou.status & LEFTBUTTONCODE) ? 0x20 : 0;	/* set or clear left button  */
		tmpStatus |= (dataBlk.mou.status & RIGHTBUTTONCODE) ? 0x10 : 0;	/* set or clear right button  */
		tmpStatus |= 0x40;			/* set first packet bit */
		if (tempX < 0) tmpStatus |= 0x02;
		if (tempY < 0) tmpStatus |= 0x08;
		if (tempX & 0x40) {
			tmpStatus |= 0x01;
			tempX &= 0x3f;
			}
		if (tempY & 0x40) {
			tmpStatus |= 0x04;
			tempY &= 0x3f;
			}

		putInMouseBuffer(tmpStatus,tempX,tempY);
/*		putInMouseBuffer(tmpStatus);
		putInMouseBuffer(tempX);
		putInMouseBuffer(tempY);
*/
		}
	while (dataBlk.mou.deltaX || dataBlk.mou.deltaY);
}



/*******************************************************************************/
void injectPS2Mouse(void)
{
	BYTE tmpStatus;
	signed char tempX,tempY;

	/* Can only send -128 to +127 at a time so must repeat until all is sent */
	do {
		if (dataBlk.mou.deltaX < -128) tempX = -128;
		else if (dataBlk.mou.deltaX > 127) tempX = 127;
		else tempX = dataBlk.mou.deltaX;
		dataBlk.mou.deltaX -= tempX;

		if (dataBlk.mou.deltaY < -127) tempY = -127;	/* use 127 because -(-128) undefined */
		else if (dataBlk.mou.deltaY > 127) tempY = 127;
		else tempY = (signed char) dataBlk.mou.deltaY;
		dataBlk.mou.deltaY -= tempY;

		/* Unfortunately, down on the IBM is negative so we switch the sign */
		/* to make down positive for the GIDEI standard */

		tempY = -tempY;
		tmpStatus = dataBlk.mou.status | 0x08;
		if (tempX < 0) tmpStatus |= 0x10;
		if (tempY < 0) tmpStatus |= 0x20;

		putInMouseBuffer(tmpStatus,tempX,tempY);
		}

	while (dataBlk.mou.deltaX || dataBlk.mou.deltaY);
}
