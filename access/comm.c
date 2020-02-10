/*  COMM.C  */

#include "skdefs.h"
#include "gideidef.h"
#include "vars.h"
#include "comm.h"
#include "init.h"


/****************************************************************************/
void processSetComputerId(void)
{
	switch(gCode)
		{
		case TERMCODE:
			switch (skCompId) {
				case 5:
				case 4:
				case 3:
				case 1:
					/* only let them change if int 15 / 4F intercept supported */
					if (vector == 0x15) {
						skCompId = tempSkCompId;
						selectInjectMethod();
						}
					break;
				default:
					skCompId = tempSkCompId;
					selectInjectMethod();
					break;
				}
		default:
			tempSkCompId = gCode;
			break;
		}
}

/****************************************************************************/
void processSingleUser(void)
{
	switch(gCode)
		{
		case TERMCODE:
			singleUserSetup = TRUE;
			break;
		default:
			errDetect();
			break;
		}
}



/****************************************************************************/
void processGen(void)
{
	switch(gCode)
		{
		case TERMCODE:
			break;
		case COMPUTERID:
			tempSkCompId = 0;
			cmdVector = processSetComputerId;
			aliasPtr = nullTable;
			beginOK = FALSE;
			break;
		case SINGLEUSER:
			cmdVector = processSingleUser;
			aliasPtr = nullTable;
			beginOK = FALSE;
			break;
		default:
			break;
		}
	return;
}

/****************************************************************************/
void processComm(void)
{
	return;
}


/****************************************************************************/
/*
	FUNCTION:	processBaudrate()

	PURPOSE:	Processes the baudrate commands.

	COMMENTS:

****************************************************************************/

void processBaudrate(void)
{
	switch(gCode)
		{
		case TERMCODE:
			switch (tempskBaudRate)
				{
				case BAUD300:
 				case BAUD600:
				case BAUD1200:
				case BAUD2400:
				case BAUD4800:
				case BAUD9600:
/*				case BAUD19200:*/
					if (skBaudRate != tempskBaudRate) {
						skBaudRate = tempskBaudRate;
						turnOffHandshake();
						disableComm();
						putPosSBuf = commBuf;
						getPosSBuf = commBuf;
						numCommChars = 0;
						while (!(inp(LSR+skCommPortAddr) & (THRE_FLAG | TSRE_FLAG)));
						setBaudRate();
						doBeep();
						doBeep();
						doBeep();
						clearOutComm();
						turnOnHandshake();
						enableComm();
						}
					break;
				default:
					errDetect();
					break;
				}
			break;
		case BAUD300CODE:
			tempskBaudRate = BAUD300;
			break;
		case BAUD600CODE:
			tempskBaudRate = BAUD600;
			break;
		case BAUD1200CODE:
			tempskBaudRate = BAUD1200;
			break;
		case BAUD2400CODE:
			tempskBaudRate = BAUD2400;
			break;
		case BAUD4800CODE:
			tempskBaudRate = BAUD4800;
			break;
		case BAUD9600CODE:
			tempskBaudRate = BAUD9600;
			break;
/*		case BAUD19200CODE:
			tempskBaudRate = BAUD19200;
			break;
*/
		default:
			errDetect();
			break;
		}
}



