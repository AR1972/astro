/* GIDE.C  */

#include "skdefs.h"
#include "gideidef.h"
#include "vars.h"
#include "gide.h"
#include "kbd.h"
#include "mou.h"
#include "comm.h"
#include "init.h"
#include "serkeys.h"

/*************************************************************************
**************************************************************************
**************************************************************************
*********                       ******************************************
*********   Program structure   ******************************************
*********                       ******************************************
**************************************************************************
**************************************************************************
**************************************************************************

 	CHAR HANDLER
					changes pointers when Escape character comes in and
					converts ASCII char mode to Escape sequence.
						
 	DETERMINE FORMAT
					determines escape sequence format i.e. is it alias
 					mode, GIDEI code mode, old KEI mode, or implied press
 					mode.  Changes vectors to appropriate routines.

 	PROCESS ALIAS
					converts the alias field into GIDEI codes.
 
	PASS ALL CODES
					a pass through routine for serial vector when in
 					GIDEI code mode since no preprocessing is needed.
 
	PROCESS KEI
					if included, this routine handles the old KEI std.
					escape sequence format.  Since the command is at
					the end in the old KEI std, we have to collect the
					entire string before it is processed.

*****************************************************************************
CODE VECTOR
					Points to routines which sift out special GIDEI codes.
 					The special GIDEI codes affect how escape sequences
 					will be handled
 
 	PROCESS GIDEI CODE
					sifts out the special GIDEI codes, if any.  This
 					routine will call additional routines if the
 					special GIDEI codes require additional processing:
 
 		PROCESS GIDEI CLEAR
					handles the :clear command
 
 		PROCESS GIDEI END
					handles the :end command

 		PROCESS GIDEI BLOCK TRANSFER
					handles :blktrans command
 			PROCESS BLOCK
 
 		PROCESS BYTES
					handles the :byte, :integer, etc commands

*****************************************************************************
COMMAND VECTOR
					These routines do the actual processing of the GIDEI
					codes which control the devices such as the keyboard,
					mouse, and serial port.  This pointer will point to
					the routine to handle a particular action on a
					particular device.  The routines are found in files
					whose names correspond to the device.  (kbd.c, mou.c,
					comm.c, etc.)

	PROCESS COMMAND
					this routine determines which device/command has
					been received and changes the pointers to the
					necessary routine and corresponding alias table
					for that routine.

	other routines
					all the other routines are in their respective
					files as mentioned above.


**************************************************************************
**************************************************************************
**************************************************************************
*/


/**************************************************************************

	FUNCTION:	charHandler

	PURPOSE:	If ESCAPE then set up new state.  If ASCII, processes the char

	COMMENTS:
*/

void charHandler(void)
{
	BYTE tempBuf;

	codeVector = processGideiCode;					/*  make sure these were done  */
	cmdVector = processCommand;
	if (serByte == ESC) {								/*  set up for next state  */
		serVector = determineFormat;
		beginOK = TRUE;
		}
	else if (waitForClear || (serByte > 127)) errDetect();
	else {													/*  get codes from ASCII table */
		tempBuf = (asciiTblPtr[serByte]).code1;
		if (!inLst(&kLock,tempBuf) && (!inLst(&kHold,tempBuf)))
			tmpLst.list[tmpLst.len++] = tempBuf;
		if ((tempBuf=(asciiTblPtr[serByte]).code2) != NOCODE)
			if (!inLst(&kLock,tempBuf) && (!inLst(&kHold,tempBuf)))
				tmpLst.list[tmpLst.len++] = tempBuf;
		doCombineLst();
		}
}





/****************************************************************************

	FUNCTION:	determineFormat

	PURPOSE:	Figure out what Escape Sequence form (i.e. Alias, Code, KEI, etc)

	COMMENTS:
*/

void determineFormat(void)
{
	switch (serByte) {
		case COMMA:											/* signals alias mode */
			serVector = processAlias;					/* set up next state */
			aliasPtr = commandsAliasTable;
			break;
		case ESC:											/* if another esc, do nothing */
			break;
		default:
			if ((serByte >= ' ') && (serByte <= '~'))
				{												/* simulate Implied Press */
				gCode = KBDPRESSCODE;					/* send press code */
				(*codeVector)();
				serVector = processAlias;				/* set up new state */
				processAlias();
				}
			else {  											/*  GIDEI code mode */
				serVector = passAllCodes;				/* set up next state */
				gCode = serByte;
				(*codeVector)();
				}
			break;
		}
}






/****************************************************************************

	FUNCTION:	processAlias()

	PURPOSE:	This routine builds up the alias field.  After receiving field
				it translates to GIDEI codes and passes on to processing routines.

	COMMENTS:
*/

void processAlias(void)
{
	BYTE aLen;
	unsigned int iTemp;

	if (serByte == ESCAPE) aliasStr[0] = '\0';	/* reset field in esc 	*/
	else if (serByte == SPACE) {}						/* ignore space chars in esc seq	*/
	else {
		for (aLen=0;aliasStr[aLen] != '\0';aLen++);
		if (serByte==COMMA || serByte==PERIOD) {	/* if field delimiter, process field */
			if (!aLen) storeByte(DEFAULTCODE);		/* empty field-assume default	*/
			else if (aLen > MAXALIASLEN)				/* field too long, what is it?	*/
				storeByte(UNKNOWNCODE);

			/* Now search alias tables to see if legal alias. */
			/* First check if alias for GIDEI directive, else check */
			/* table related to the command */

			else if (tblSearch(gideiAliasTable)) storeByte(aliasStr[0]);
			else if (aliasUsedInStandard(aLen)) storeByte(aliasStr[0]);
			else {
				/* Well, not in alias tables.  Maybe it is a numeric parameter. */
				iTemp = convertStringToInt();
					/* if cannot convert string to an integer, */
					/* iTemp = 0xFFFF and aliasStr[0] = '\0' */

				/* Now, is it an ASCII coded number or ASCII coded GIDEI code? */
				switch (aliasStr[0]) {
					case '0':
					case '+':
					case '-':								/* ASCII coded number*/
						storeByte(INTEGERCODE);
						storeByte((BYTE) iTemp);
						storeByte((BYTE) (iTemp >> 8));
						break;
					default:
						/* ASCII coded GIDEI code? */
						if (iTemp > 255) storeByte(UNKNOWNCODE);
						else storeByte((BYTE) iTemp);
						break;
					}
				}
			if (serByte == '.') storeByte(TERMCODE);
			aliasStr[0] = '\0';
			for (;retrieveByte();) (*codeVector)();/* byte returned in gCode*/
			}
		else {
	  		/* just add the char to the string */
			if ((serByte > ' ') && (serByte <= '~')) {
				if (aLen < MAXALIASLEN+1) {			/* ignore if no room*/
					if ((serByte>='A')&&(serByte<='Z'))/* make sure lower case	*/
						serByte += 'a' - 'A';
					aliasStr[aLen++] = serByte;		/* now add to string	*/
					aliasStr[aLen] = '\0';
					}
				}
			else
				errDetect();								/* not an alias	*/
			}
		}
}



/****************************************************************************/
BOOL aliasUsedInStandard(BYTE aLen)
{
	BOOL found;
	BYTE iCode;

	/* if alias is an alias for a key and alias is a single character,  */
	/* use ASCII table for code.  This saves table space.  */

	if ((aLen == 1) && (aliasPtr == keyAliasTable)) {
		iCode = asciiTable[aliasStr[0]].code1;
		if ((iCode == control_key) || (iCode == shift_key))
			aliasStr[0] = asciiTable[aliasStr[0]].code2;
		else
			aliasStr[0] = iCode;
		found = TRUE;
		}
	else found = tblSearch(aliasPtr);
	return (found);
}

/****************************************************************************/
BOOL tblSearch(struct aliasTableType * tblPtr)
{
	BOOL found;
	int result;

	found = FALSE;
	for (;(tblPtr->aliasName[0]!='\0')&&(!found); tblPtr++) {
		result = strcmp(aliasStr,tblPtr->aliasName);
 		if ( result == 0) {
			found = TRUE;
			aliasStr[0] = tblPtr->gideiCode;
			}
		else if (result < 0) break;					/* past all so may as well	*/
		}														/*  quit now	*/
	return (found);
}




/****************************************************************************

	FUNCTION:	passAllCodes

	PURPOSE:	Just keeps the GIDEI hierarchy consistant

	COMMENTS:	
*/

void passAllCodes(void)
{
	gCode = serByte;
	(*codeVector)();
	return;
}



/****************************************************************************************/



/****************************************************************************

	FUNCTION:	processGideiCode

	PURPOSE:
				

	COMMENTS:

*/

void processGideiCode(void)
{
	if (waitForClear) {
		if (gCode == CLEARCODE) codeVector = processGideiClear;
		else errDetect();
		}
	else
		switch (gCode) {

			case BEGINCODE:
				if (beginOK) {
					if (pushPointers()) lastCode = gCode;
					else errDetect();
					}
				else errDetect();
				break;

			case ENDCODE:
				if (lastCode == TERMCODE) {
					codeVector = processGideiEnd;
					beginOK = FALSE;
					lastCode = gCode;
					}
				else errDetect();
				break;

			case CLEARCODE:
				codeVector = processGideiClear;
				lastCode = gCode;
				break;

			case TERMCODE:
				(*cmdVector)();
				if (!restorePointers()) {
					cmdVector = processCommand;
					codeVector = processGideiCode;
					serVector = charHandler;
					beginOK = FALSE;
					}
				else
					beginOK = TRUE;
				lastCode = gCode;
				break;

			case BLKTRANSCODE:
				codeVector = processGideiBlockTransfer;
				(*cmdVector)();
				lastCode = gCode;
				break;

			case BYTECODE:
				codeVector = processBytes;
				byteCount = 1;
				passAll = TRUE;
				(*cmdVector)();
				lastCode = gCode;
				break;

			case INTEGERCODE:
				codeVector = processBytes;
				byteCount = 2;
				passAll = TRUE;
				(*cmdVector)();
				lastCode = gCode;
				break;

			case LONGCODE:
				codeVector = processBytes;
				byteCount = 4;
				passAll = TRUE;
				(*cmdVector)();
				lastCode = gCode;
				break;

			case DOUBLECODE:
				codeVector = processBytes;
				byteCount = 8;
				passAll = TRUE;
				(*cmdVector)();
				lastCode = gCode;
				break;

			default:
				(*cmdVector)();
				lastCode = gCode;
				break;
			}

}



/************************/
void processGideiClear(void)
{
	if (gCode == TERMCODE) initGIDEI();
	else errDetect();
}



/************************/
void processGideiEnd(void)
{
	if (gCode == TERMCODE) {
		if (!popPointers()) errDetect();
		else {
			if (restorePointers()) {
				beginOK = TRUE;
				codeVector = processGideiCode;
				}
			else {
				cmdVector = processCommand;
				codeVector = processGideiCode;
				serVector = charHandler;
				beginOK = FALSE;
				}
			lastCode = gCode;
			}
		}
	else errDetect();
}



/************************/
void processGideiBlockTransfer(void)
{
	byteCount = gCode;
	codeVector = processBlock;
	passAll = TRUE;
}




/************************/
void processBlock(void)
{
	if (byteCount--) (*cmdVector)();
	else {
		passAll = FALSE;
		if (gCode == TERMCODE) codeVector = processGideiCode;
		else errDetect();
		}
}



/************************/
void processBytes(void)
{
	(*cmdVector)();
	if (!(--byteCount)) {
		passAll = FALSE;
		codeVector = processGideiCode;
		}
}



/****************************************************************************

	FUNCTION:	processCommand

	PURPOSE:	Determine which command is active.  Then set cmdVector to
				point to appropriate routine.

	COMMENTS:

*/
void processCommand(void)
{
	switch(gCode) {
		case KBDEXPANSIONCODE:
			cmdVector = processKbd;
			aliasPtr = kbdAliasTable;
			beginOK = TRUE;
			break;
		case MOUEXPANSIONCODE:
			cmdVector = processMou;
			aliasPtr = mouseAliasTable;
			beginOK = TRUE;
			break;
		case GENCODE:
			cmdVector = processGen;
			aliasPtr = genAliasTable;
			beginOK = TRUE;
			break;
		case COMMCODE:
			cmdVector = processComm;
			aliasPtr = commAliasTable;
			beginOK = TRUE;
			break;
		case KBDLOCKCODE:
			cmdVector = processKbdLock;
			aliasPtr = keyAliasTable;
			beginOK = TRUE;
			break;
		case KBDRELCODE:
			cmdVector = processKbdRel;
			aliasPtr = keyAliasTable;
			beginOK = TRUE;
			break;
		case KBDPRESSCODE:
			cmdVector = processKbdPress;
			aliasPtr = keyAliasTable;
			beginOK = TRUE;
			break;
		case KBDCOMBINECODE:
			cmdVector = processKbdCombine;
			aliasPtr = keyAliasTable;
			beginOK = TRUE;
			break;
		case KBDHOLDCODE:
			cmdVector = processKbdHold;
			aliasPtr = keyAliasTable;
			beginOK = TRUE;
			break;
		case MOULOCKCODE:
			cmdVector = processMouLock;
			aliasPtr = mouButtonAliasTable;
			beginOK = TRUE;
			break;
		case MOURELCODE:
			cmdVector = processMouRel;
			aliasPtr = mouButtonAliasTable;
			beginOK = TRUE;
			break;
		case MOUCLICKCODE:
			cmdVector = processMouClick;
			aliasPtr = mouButtonAliasTable;
			beginOK = TRUE;
			break;
		case MOUDOUBLECLICKCODE:
			cmdVector = processMouDoubleClick;
			aliasPtr = mouButtonAliasTable;
			beginOK = TRUE;
			break;
		case MOUMOVECODE:
			cmdVector = processMouMove;
			aliasPtr = nullTable;
			beginOK = TRUE;
			break;
		case MOUGOTOCODE:
			cmdVector = processMouGoto;
			aliasPtr = nullTable;
			beginOK = TRUE;
			break;
		case MOURESETCODE:
			cmdVector = processMouReset;
			aliasPtr = nullTable;
			beginOK = TRUE;
			break;
		case BAUDRATECODE:
			cmdVector = processBaudrate;
			aliasPtr = baudrateAliasTable;
			beginOK = TRUE;
			break;
		case UNKNOWNCODE:
			errDetect();
/*			cmdVector = noOpRoutine;
			beginOK = TRUE;
*/
		default:
			if (gCode >= LOWESTGIDEICODE) errDetect();
			else {
				errDetect();
/*				cmdVector = noOpRoutine;
				beginOK = TRUE;
*/
				}
			break;
		}
}

/*****************************************************************************************/
/**********************  MISC ROUTINES     ***********************************************/
/****************************************************************************

	FUNCTION:	pushPointers

	PURPOSE:	push cmdVector on to vectorStack

	COMMENTS:	*/


BOOL pushPointers(void)
{
	if (ptrStackPtr >= (ptrStack+MAXPOINTERSTACK)) return FALSE;
	ptrStackPtr->aliasTablePtr = aliasPtr;
	ptrStackPtr->commandRoutinePtr = cmdVector;
	++ptrStackPtr;
	return TRUE;
}




/****************************************************************************

	FUNCTION:	restorePointers

	PURPOSE:	restore Pointers from vectorStack but does not update 
				stack pointer.

	COMMENTS: */


BOOL restorePointers(void)
{
	if (ptrStackPtr <= ptrStack) return FALSE;
	aliasPtr = (ptrStackPtr-1)->aliasTablePtr;
	cmdVector = (ptrStackPtr-1)->commandRoutinePtr;
	return TRUE;
}




/****************************************************************************

	FUNCTION:	popPointers

	PURPOSE:	pop pointers from stack

	COMMENTS:	*/


BOOL popPointers(void)
{
	if (restorePointers()) {
		--ptrStackPtr;
		return TRUE;
		}
	return FALSE;
}





/************************/
BOOL storeByte(BYTE theByte)
{
	/*spos points to next free storage spot */
	/*If spos+1 = rpos then buffer is full*/
	/*also, if spos+1 = end of codebuffer then rap around */
	/*to 0 unless rpos=0 cuz buffer is full */

	if ((spos+1==rpos)||((spos+1==CODEBUFFERLEN) && !rpos)) return FALSE;
	buf[spos] = theByte;
	spos = (spos+1==CODEBUFFERLEN) ? 0 : spos+1;
	return TRUE;
}


/************************/
BOOL retrieveByte(void)
{
	/* rpos points to byte to retrieve.  If rpos = spos then buffer empty */

	if (spos==rpos) return FALSE;
	gCode = buf[rpos];
	rpos = (rpos+1==CODEBUFFERLEN) ? 0 : rpos+1;
	return TRUE;
}




