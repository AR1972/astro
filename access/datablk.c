#include <bios.h>
#include <graph.h>
#include <stdio.h>
#include <string.h>
#include <dos.h>
#include <fcntl.h>
#include <process.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>

#include "datablk.h"
#include "defines.h"
#include "globals.h"
#include "messages.h"
#include "access.h"
#include "dialogs.h"

#define  ACCESS_FUNCTION   0xAD        /******* any changes here must also be made in KEYBOARD.INC */

#define  TSR_HERE          0xACCE      /******* any changes here must also be made in KEYBOARD.INC */
#define  TSR_DETECT        0xe1        /******* any changes here must also be made in KEYBOARD.INC */
#define  TSR_PARAMS        0xe2        /******* any changes here must also be made in KEYBOARD.INC */


/* Offsets into dataBlock for shared parameters to Mark's code */

#define SK_SERIAL_KEYS_ON            0    /* db */    /* Serial Keys Parameters */
#define SK_PORT_ADDRESS              1    /* dw */
#define SK_BAUD_RATE                 3    /* dw */

#define F_FILTER_KEYS_ON			    5 	/* db */		/* FilterKeys (SlowKeys and RepeatKeys) */
#define F_ON_OFF_FEEDBACK			    6 	/* db */
#define F_USER_SETUP_OPTION_1 	    7 	/* db */
#define F_USER_SETUP_OPTION_2 	    8 	/* db */
#define F_WAIT_TICKS 				    9 	/* dw */
#define F_DELAY_TICKS				   11 	/* dw */
#define F_REPEAT_TICKS				   13 	/* dw */
#define F_RECOVERY_TICKS			   15 	/* dw */
#define F_MAX_DEFAULT				   17 	/* db */
#define F_CLICK_ON					   18 	/* db */
#define F_RECOVERY_ON				   19 	/* db */
#define F_DIALOG_FILTER_KEYS_OFF    20    /* db */

#define S_STICKEY_KEYS_ON			   21		/* db */		/* StickyKeys */
#define S_ON_OFF_FEEDBACK			   22		/* db */
#define S_AUDIBLE_FEEDBACK			   23		/* db */
#define S_TRI_STATE					   24		/* db */
#define S_TWO_KEYS_OFF				   25		/* db */
#define S_DIALOG_STICKEY_KEYS_OFF   26		/* db */
#define S_STICKEY_CLICK_ON			   27		/* db */

#define M_MOUSE_KEYS_ON				   28		/* db */		/* MouseKeys */
#define M_ON_OFF_FEEDBACK			   29		/* db */
#define M_MAX_SPEED					   30		/* dw */
#define M_TIME_TO_MAX_SPEED		   32		/* dw */
#define M_DIALOG_MOUSE_KEYS_OFF     34    /* db */
#define M_MOUSE_KEYS_OVERRIDE       35    /* db */

#define T_TOGGLE_KEYS_ON			   36		/* db */		/* ToggleKeys */
#define T_ON_OFF_FEEDBACK			   37		/* db */
#define T_DIALOG_TOGGLE_KEYS_OFF    38    /* db */

#define TO_TIME_OUT					   39		/* db */		/* TimeOut Parameters */
#define TO_ON_OFF_FEEDBACK			   40		/* db */
#define TO_VALUE						   41		/* dw */
#define TO_DIALOG_TIMEOUT_OFF       43    /* db */

#define COMP_DIALOG                 44    /* db */
#define COMP_DIALOG_ID              45    /* db */
#define COMP_DIALOG_ACTION          46    /* db */

#define F_SPACE_SAVER               47    /* db */
#define F_COMPUTER_NOT_FOUND        48    /* db */
#define F_HEARING_ON                49    /* db */
#define SK_SERIAL_KEYS_LOADED  		50	   /* db */
#define F_ACCESS_ALREADY_LOADED     51    /* db */

#define SK_INTERRUPT                52    /* db */
#define F_VIDEO_FLASH					53		/* db */
#define FILLER_SPACE                54    /* room for expansion */
#define DATA_BLOCK_LENGTH           61    /* length of the datablock */

#define BUTTON_1                    61    /* db */
#define BUTTON_2                    62    /* db */
#define CURRENT_BUTTON              63    /* db */
#define COMPUTER_ID						64		/* db */
#define MOUSE_DRIVER						65		/* db */
#define MOUSE_ID							66		/* db */
#define COMM_BASE							67		/* dw */
#define EXTENDED_SEGMENT				69		/* dw */
#define VECTOR								71		/* db */
#define INJECT_KEYS						72		/* db */
#define VIDEO_TYPE						73		/* db */
#define COMPUTER_FLAG					74		/* db */


char _far *accessString = "ACCESS";
unsigned char transferBuffer[DATA_BLOCK_LENGTH];
int interruptNumber = 0;

void GetInterruptNumber(void)
{
	int i;
	size_t count = _fstrlen(accessString);
	unsigned char _far *strPtr;
	void (_interrupt _far *interruptHandler)();

	if (debug)	_clearscreen(_GCLEARSCREEN);

	for (i=0x60; i<0x68; i++)
	{
		interruptHandler = _dos_getvect(i);
		strPtr = (unsigned char _far *) interruptHandler;
		if (_fstrncmp(accessString, strPtr+3, count) == 0) interruptNumber = i;

		if (debug)
		{
			printf("0x%02X: %Fp", i, strPtr);
			if	(strPtr)	printf(" %.15Fs\n", strPtr+3);
			else printf("\n");
		}
	}

	if	(debug) printf("Interrupt Number: 0x%02X\n", interruptNumber);

	if (interruptNumber == 0)
	{
		printf("TSR not installed.  An error has occured installing TSR helper routine.");
		exit(ABORT_ACCESS);
	}
}	/* end GetInterruptNumber() */


void GetAddressOfParameterBlock(void)
{
	union REGS inputRegisters, outputRegisters;
	unsigned int _far *paramAddressPtr = (unsigned int _far *) &currentParameters;

	inputRegisters.h.ah = ACCESS_FUNCTION;
	inputRegisters.h.al = TSR_DETECT;
	int86(interruptNumber, &inputRegisters, &outputRegisters);
	if (outputRegisters.x.ax == TSR_HERE)
	{
		inputRegisters.h.ah = ACCESS_FUNCTION;
		inputRegisters.h.al = TSR_PARAMS;
		int86(interruptNumber, &inputRegisters, &outputRegisters);
		*paramAddressPtr = outputRegisters.x.ax;
		paramAddressPtr++;
		*paramAddressPtr = outputRegisters.x.bx;
	}
	else
	{
		printf("TSR not installed.  An error has occured reading shared parameter block.");
		exit(ABORT_ACCESS);
	}
}  /* end GetAddressOfParameterBlock() */


void GetAccessParameters(void)
{
   skSerialKeysOn             = *((unsigned char _far *)  (currentParameters+SK_SERIAL_KEYS_ON));
   skPortAddress              = *((unsigned int  _far *)  (currentParameters+SK_PORT_ADDRESS));  oldPortAddress = skPortAddress;
   skBaudRate                 = *((unsigned int  _far *)  (currentParameters+SK_BAUD_RATE));
   skSerialKeysLoaded         = *((unsigned char _far *)  (currentParameters+SK_SERIAL_KEYS_LOADED));
   skInterrupt                = *((unsigned char _far *)  (currentParameters+SK_INTERRUPT));

	fFilterKeysOn			      = *((unsigned char _far *)	(currentParameters+F_FILTER_KEYS_ON));
	fOnOffFeedback			      = *((unsigned char _far *)	(currentParameters+F_ON_OFF_FEEDBACK));
	fWaitTicks				      = *((unsigned int  _far *)	(currentParameters+F_WAIT_TICKS));
	fDelayTicks				      = *((unsigned int  _far *)	(currentParameters+F_DELAY_TICKS));
	fRepeatTicks			      = *((unsigned int  _far *)	(currentParameters+F_REPEAT_TICKS));
	fBounceTicks			      = *((unsigned int  _far *)	(currentParameters+F_RECOVERY_TICKS));
	fMaxDefault				      = *((unsigned char _far *)	(currentParameters+F_MAX_DEFAULT));
	fClickOn					      = *((unsigned char _far *)	(currentParameters+F_CLICK_ON));
	fBounceOn					   = *((unsigned char _far *)	(currentParameters+F_RECOVERY_ON));
	fDialogFilterKeysOff       = *((unsigned char _far *)	(currentParameters+F_DIALOG_FILTER_KEYS_OFF));

	sStickyKeysOn			      = *((unsigned char _far *)	(currentParameters+S_STICKEY_KEYS_ON));
	sOnOffFeedback			      = *((unsigned char _far *)	(currentParameters+S_ON_OFF_FEEDBACK));
	sAudibleFeedback		      = *((unsigned char _far *)	(currentParameters+S_AUDIBLE_FEEDBACK));
	sTriState				      = *((unsigned char _far *)	(currentParameters+S_TRI_STATE));
	sTwoKeysOff				      = *((unsigned char _far *)	(currentParameters+S_TWO_KEYS_OFF));
	sDialogStickyKeysOff			= *((unsigned char _far *)	(currentParameters+S_DIALOG_STICKEY_KEYS_OFF));
	sClickOn							= *((unsigned char _far *)	(currentParameters+S_STICKEY_CLICK_ON));

	mMouseKeysOn			      = *((unsigned char _far *)	(currentParameters+M_MOUSE_KEYS_ON));
	mOnOffFeedback			      = *((unsigned char _far *)	(currentParameters+M_ON_OFF_FEEDBACK));
	mMaxSpeed				      = *((unsigned int  _far *)	(currentParameters+M_MAX_SPEED));
	mTimeToMaxSpeed		      = *((unsigned int  _far *)	(currentParameters+M_TIME_TO_MAX_SPEED));
	mDialogMouseKeysOff        = *((unsigned char _far *)	(currentParameters+M_DIALOG_MOUSE_KEYS_OFF));
	mMouseKeysOverride         = *((unsigned char _far *)	(currentParameters+M_MOUSE_KEYS_OVERRIDE));

	tToggleKeysOn			      = *((unsigned char _far *)	(currentParameters+T_TOGGLE_KEYS_ON));
	tOnOffFeedback			      = *((unsigned char _far *)	(currentParameters+T_ON_OFF_FEEDBACK));
	tDialogToggleKeysOff       = *((unsigned char _far *)	(currentParameters+T_DIALOG_TOGGLE_KEYS_OFF));

	toTimeOut						= *((unsigned char _far *)	(currentParameters+TO_TIME_OUT));
	toOnOffFeedback				= *((unsigned char _far *)	(currentParameters+TO_ON_OFF_FEEDBACK));
	toValue							= *((unsigned int  _far *)	(currentParameters+TO_VALUE));
	toDialogTimeOutOff			= *((unsigned char _far *)	(currentParameters+TO_DIALOG_TIMEOUT_OFF));

   compDialog						= *((unsigned char _far *)  (currentParameters+COMP_DIALOG));
   compDialogId					= *((unsigned char _far *)  (currentParameters+COMP_DIALOG_ID));
   compDialogAction				= *((unsigned char _far *)  (currentParameters+COMP_DIALOG_ACTION));
   fSpaceSaver						= *((unsigned char _far *)  (currentParameters+F_SPACE_SAVER));
   fComputerNotFound				= *((unsigned char _far *)  (currentParameters+F_COMPUTER_NOT_FOUND));
   fHearingOn						= *((unsigned char _far *)  (currentParameters+F_HEARING_ON));
   fVideoFlash						= *((unsigned char _far *)  (currentParameters+F_VIDEO_FLASH));
   fAccessAlreadyLoaded			= *((unsigned char _far *)  (currentParameters+F_ACCESS_ALREADY_LOADED));
}	/* end GetAccessParameters() */


void SetAccessParameters(unsigned char _far *paramBlockPtr, int forceDialogFlags)
{
	unsigned char _far *charPtr;
	unsigned int _far *intPtr;
   compDialogAction = TRUE;

      /* Set fDialogFilterKeysOff to TRUE if a FilterKeys Parameter has changed */
   if (forceDialogFlags) fDialogFilterKeysOff = TRUE;
   else fDialogFilterKeysOff = (unsigned char)
      (*((unsigned char _far *) (paramBlockPtr + F_FILTER_KEYS_ON))        != fFilterKeysOn     ||
       *((unsigned char _far *) (paramBlockPtr + F_ON_OFF_FEEDBACK))       != fOnOffFeedback    ||
       *((unsigned int  _far *) (paramBlockPtr + F_WAIT_TICKS))            != fWaitTicks        ||
       *((unsigned int  _far *) (paramBlockPtr + F_DELAY_TICKS))           != fDelayTicks       ||
       *((unsigned int  _far *) (paramBlockPtr + F_REPEAT_TICKS))          != fRepeatTicks      ||
       *((unsigned int  _far *) (paramBlockPtr + F_RECOVERY_TICKS))        != fBounceTicks      ||
       *((unsigned char _far *) (paramBlockPtr + F_MAX_DEFAULT))           != fMaxDefault       ||
       *((unsigned char _far *) (paramBlockPtr + F_CLICK_ON))              != fClickOn          ||
       *((unsigned char _far *) (paramBlockPtr + F_RECOVERY_ON))           != fBounceOn);

      /* store the globals back into the shared parameter block */
	charPtr	= (unsigned char _far *)	(paramBlockPtr + F_FILTER_KEYS_ON);				*charPtr = fFilterKeysOn;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + F_ON_OFF_FEEDBACK);			*charPtr = fOnOffFeedback;
	intPtr	= (unsigned int  _far *)	(paramBlockPtr + F_WAIT_TICKS);					*intPtr	= fWaitTicks;
	intPtr	= (unsigned int  _far *)	(paramBlockPtr + F_DELAY_TICKS);					*intPtr	= fDelayTicks;
	intPtr	= (unsigned int  _far *)	(paramBlockPtr + F_REPEAT_TICKS);				*intPtr	= fRepeatTicks;
	intPtr	= (unsigned int  _far *)	(paramBlockPtr + F_RECOVERY_TICKS);				*intPtr	= fBounceTicks;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + F_MAX_DEFAULT);					*charPtr = fMaxDefault;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + F_CLICK_ON);						*charPtr	= fClickOn;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + F_RECOVERY_ON);					*charPtr	= fBounceOn;
   charPtr  = (unsigned char _far *)   (paramBlockPtr + F_DIALOG_FILTER_KEYS_OFF);	*charPtr = fDialogFilterKeysOff;



      /* Set sDialogStickyKeysOff to TRUE if a StickyKeys Parameter has changed */
   if (forceDialogFlags) sDialogStickyKeysOff = TRUE;
   else sDialogStickyKeysOff = (unsigned char)
      (*((unsigned char _far *)	(paramBlockPtr + S_STICKEY_KEYS_ON))        != sStickyKeysOn       ||
	    *((unsigned char _far *)	(paramBlockPtr + S_ON_OFF_FEEDBACK))        != sOnOffFeedback       ||
	    *((unsigned char _far *)	(paramBlockPtr + S_AUDIBLE_FEEDBACK))       != sAudibleFeedback     ||
  	    *((unsigned char _far *)	(paramBlockPtr + S_TRI_STATE))              != sTriState            ||
  	    *((unsigned char _far *)	(paramBlockPtr + S_TWO_KEYS_OFF))           != sTwoKeysOff          ||
  	    *((unsigned char _far *)	(paramBlockPtr + S_STICKEY_CLICK_ON))       != sClickOn);

      /* store the globals back into the shared parameter block */
	charPtr	= (unsigned char _far *)	(paramBlockPtr + S_STICKEY_KEYS_ON);			*charPtr	= sStickyKeysOn;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + S_ON_OFF_FEEDBACK);			*charPtr	= sOnOffFeedback;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + S_AUDIBLE_FEEDBACK);			*charPtr	= sAudibleFeedback;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + S_TRI_STATE);					*charPtr	= sTriState;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + S_TWO_KEYS_OFF);				*charPtr	= sTwoKeysOff;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + S_DIALOG_STICKEY_KEYS_OFF);	*charPtr	= sDialogStickyKeysOff;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + S_STICKEY_CLICK_ON);			*charPtr	= sClickOn;



      /* Set sDialogMouseKeysOff to TRUE if a MouseKeys Parameter has changed */
	if (forceDialogFlags) mDialogMouseKeysOff = TRUE;
   else mDialogMouseKeysOff = (unsigned char)
      (*((unsigned char _far *)	(paramBlockPtr + M_MOUSE_KEYS_ON))       != mMouseKeysOn      ||
	    *((unsigned char _far *)	(paramBlockPtr + M_ON_OFF_FEEDBACK))     != mOnOffFeedback    ||
	    *((unsigned int  _far *)	(paramBlockPtr + M_MAX_SPEED))           != mMaxSpeed         ||
	    *((unsigned int  _far *)	(paramBlockPtr + M_TIME_TO_MAX_SPEED))   != mTimeToMaxSpeed   ||
	    *((unsigned char _far *)	(paramBlockPtr + M_MOUSE_KEYS_OVERRIDE)) != mMouseKeysOverride);

      /* store the globals back into the shared parameter block */
	charPtr	= (unsigned char _far *)	(paramBlockPtr + M_MOUSE_KEYS_ON);			   *charPtr	= mMouseKeysOn;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + M_ON_OFF_FEEDBACK);		   *charPtr	= mOnOffFeedback;
	intPtr	= (unsigned int  _far *)	(paramBlockPtr + M_MAX_SPEED);			      *intPtr	= mMaxSpeed;
	intPtr	= (unsigned int  _far *)	(paramBlockPtr + M_TIME_TO_MAX_SPEED);			*intPtr	= mTimeToMaxSpeed;
	intPtr	= (unsigned int  _far *)	(paramBlockPtr + M_DIALOG_MOUSE_KEYS_OFF);	*intPtr	= mDialogMouseKeysOff;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + M_MOUSE_KEYS_OVERRIDE);		*charPtr	= mMouseKeysOverride;



      /* Set sDialogToggleKeysOff to TRUE if a ToggleKeys Parameter has changed */
	if (forceDialogFlags) tDialogToggleKeysOff = TRUE;
   else tDialogToggleKeysOff = (unsigned char)
      (*((unsigned char _far *)	(paramBlockPtr + T_TOGGLE_KEYS_ON))         != tToggleKeysOn  ||
       *((unsigned char _far *)	(paramBlockPtr + T_ON_OFF_FEEDBACK))        != tOnOffFeedback);

      /* store the globals back into the shared parameter block */
	charPtr	= (unsigned char _far *)	(paramBlockPtr + T_TOGGLE_KEYS_ON);				*charPtr	= tToggleKeysOn;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + T_ON_OFF_FEEDBACK);			*charPtr	= tOnOffFeedback;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + T_DIALOG_TOGGLE_KEYS_OFF);	*charPtr	= tDialogToggleKeysOff;



      /* Set sDialogTimeoutOff to TRUE if a Timeout Parameter has changed */
	if (forceDialogFlags) toDialogTimeOutOff = TRUE;
   else toDialogTimeOutOff = (unsigned char)
      (*((unsigned char _far *)	(paramBlockPtr + TO_TIME_OUT))        != toTimeOut       ||
	    *((unsigned char _far *)	(paramBlockPtr + TO_ON_OFF_FEEDBACK)) != toOnOffFeedback ||
	    *((unsigned int  _far *)	(paramBlockPtr + TO_VALUE))           != toValue);

      /* store the globals back into the shared parameter block */
	charPtr	= (unsigned char _far *)	(paramBlockPtr + TO_TIME_OUT);				*charPtr	= toTimeOut;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + TO_ON_OFF_FEEDBACK);		*charPtr	= toOnOffFeedback;
	intPtr	= (unsigned int  _far *)	(paramBlockPtr + TO_VALUE);					*intPtr	= toValue;
	charPtr	= (unsigned char _far *)	(paramBlockPtr + TO_DIALOG_TIMEOUT_OFF);	*charPtr	= toDialogTimeOutOff;


   charPtr  = (unsigned char _far *) (paramBlockPtr + SK_SERIAL_KEYS_ON);	*charPtr = skSerialKeysOn;
   charPtr  = (unsigned char _far *) (paramBlockPtr + SK_INTERRUPT);			*charPtr = skInterrupt;
   intPtr   = (unsigned int  _far *) (paramBlockPtr + SK_PORT_ADDRESS);		*intPtr  = skPortAddress;
   intPtr   = (unsigned int  _far *) (paramBlockPtr + SK_BAUD_RATE);			*intPtr  = skBaudRate;


	compDialog = fComputerNotFound;
   charPtr  = (unsigned char _far *) (paramBlockPtr + COMP_DIALOG);				*charPtr = compDialog;
   charPtr  = (unsigned char _far *) (paramBlockPtr + COMP_DIALOG_ID);			*charPtr = compDialogId;
   charPtr  = (unsigned char _far *) (paramBlockPtr + COMP_DIALOG_ACTION);		*charPtr = compDialogAction;
   charPtr  = (unsigned char _far *) (paramBlockPtr + F_SPACE_SAVER);			*charPtr = fSpaceSaver;
   charPtr  = (unsigned char _far *) (paramBlockPtr + F_COMPUTER_NOT_FOUND);	*charPtr = fComputerNotFound;
   charPtr  = (unsigned char _far *) (paramBlockPtr + F_HEARING_ON);				*charPtr = fHearingOn;
   charPtr  = (unsigned char _far *) (paramBlockPtr + F_VIDEO_FLASH);			*charPtr = fVideoFlash;
}	/* end SetAccessParameters() */


void GetEquipmentParameters(void)
{
   skSerialKeysOn             = *((unsigned char _far *) (currentParameters+SK_SERIAL_KEYS_ON));
   eqButton1                  = *((unsigned char _far *) (currentParameters + BUTTON_1));
   eqButton2                  = *((unsigned char _far *) (currentParameters + BUTTON_2));
   eqCurrentButton            = *((unsigned char _far *) (currentParameters + CURRENT_BUTTON));
   eqComputerId               = *((unsigned char _far *) (currentParameters + COMPUTER_ID));
   eqMouseDriver              = *((unsigned char _far *) (currentParameters + MOUSE_DRIVER));
   eqMouseId                  = *((unsigned char _far *) (currentParameters + MOUSE_ID));
   eqCommBase                 = *((unsigned int  _far *) (currentParameters + COMM_BASE));
   eqExtendedSegment          = *((unsigned int  _far *) (currentParameters + EXTENDED_SEGMENT));
   eqVector                   = *((unsigned char _far *) (currentParameters + VECTOR));
   eqInjectKeys               = *((unsigned char _far *) (currentParameters + INJECT_KEYS));
   eqVideoType                = *((unsigned char _far *) (currentParameters + VIDEO_TYPE));
   compDialog                 = *((unsigned char _far *) (currentParameters + COMP_DIALOG));
   compDialogId               = *((unsigned char _far *) (currentParameters + COMP_DIALOG_ID));
   eqComputerFlag             = *((unsigned char _far *) (currentParameters + COMPUTER_FLAG));
   fComputerNotFound          = *((unsigned char _far *) (currentParameters + F_COMPUTER_NOT_FOUND));
}  /* end GetEquipmentParameters() */


void SetEquipmentParameters(void)
{
	unsigned char _far *charPtr;
	unsigned int _far *intPtr;

	charPtr	= (unsigned char _far *)	(currentParameters + BUTTON_1);				   *charPtr = eqButton1;
	charPtr	= (unsigned char _far *)	(currentParameters + BUTTON_2);				   *charPtr = eqButton2;
	charPtr	= (unsigned char _far *)	(currentParameters + CURRENT_BUTTON);		   *charPtr = eqCurrentButton;
	charPtr	= (unsigned char _far *)	(currentParameters + COMPUTER_ID);			   *charPtr = eqComputerId;
	charPtr	= (unsigned char _far *)	(currentParameters + MOUSE_DRIVER);			   *charPtr = eqMouseDriver;
	charPtr	= (unsigned char _far *)	(currentParameters + MOUSE_ID);				   *charPtr = eqMouseId;
	intPtr	= (unsigned int  _far *)	(currentParameters + COMM_BASE);				   *intPtr  = eqCommBase;
	intPtr	= (unsigned int  _far *)	(currentParameters + EXTENDED_SEGMENT);	   *intPtr  = eqExtendedSegment;
	charPtr	= (unsigned char _far *)	(currentParameters + VECTOR);					   *charPtr = eqVector;
	charPtr	= (unsigned char _far *)	(currentParameters + INJECT_KEYS);			   *charPtr = eqInjectKeys;
	charPtr	= (unsigned char _far *)	(currentParameters + VIDEO_TYPE);			   *charPtr = eqVideoType;
   charPtr  = (unsigned char _far *)	(currentParameters + COMP_DIALOG);           *charPtr = compDialog;
   charPtr  = (unsigned char _far *)	(currentParameters + COMP_DIALOG_ID);        *charPtr = compDialogId;
	charPtr	= (unsigned char _far *)	(currentParameters + COMPUTER_FLAG);		   *charPtr = eqComputerFlag;
	charPtr	= (unsigned char _far *)	(currentParameters + F_COMPUTER_NOT_FOUND);	*charPtr = fComputerNotFound;
}	/* end SetEquipmentParameters() */


int LoadParameters(void)
{
   int height = 9, width = 60;
   int fileHandle = open(configurationFilename, O_RDWR | O_BINARY);

   if (fileHandle != -1)
   {
      unsigned bytesRead;

      lseek(fileHandle, 0L, SEEK_SET);
      bytesRead = read(fileHandle, (void *) transferBuffer, DATA_BLOCK_LENGTH);
      close (fileHandle);
   	if (bytesRead == DATA_BLOCK_LENGTH)
      {
         int i;
         unsigned char _far *charPtr = currentParameters;

         for (i=0; i<DATA_BLOCK_LENGTH; i++)
         {
            *charPtr = transferBuffer[i];
            charPtr++;
         }
         GetAccessParameters();
         return TRUE;
      }
   }

   DisplayAlertBox(LoadFailedAlertText, height, width);
   return FALSE;
}  /* end LoadParameters() */


void SaveParameters(void)
{
   unsigned key;
   int scanCode, ascii, notDone = TRUE, helpMessageFlag = FALSE, saveParametersFlag = FALSE;
   int top=11, bottom=15, left=13, right=67, width;
	char *title = "Save AccessDOS Settings";

	width = right-left+1;

	DrawDialogBox(top, left, bottom, right, dialogBoxBorderFG, dialogBoxBorderBG, dialogBoxFG, dialogBoxBG, title);
	charBufferLength = sprintf(charBuffer, "Are you sure you want to save the current settings?");
	_settextposition(2, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

	charBufferLength = sprintf(charBuffer, "Press Enter to proceed");
	_settextposition(4, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

	charBufferLength = sprintf(charBuffer, "Press Escape to return to menu");
	_settextposition(5, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

	OutputHintBar("This will save the current settings to the ADOS.CFG file.", JUSTIFY_LEFT, helpMessageFlag);

   while (notDone)
   {
         /* Get a key and process it */
	   key = _bios_keybrd(keybrdRead);
	   ascii = key & 0x00ff;
	   scanCode = ascii ? 0 : key >> 8;

	   if (ascii)
	   {
         notDone = FALSE;
			switch (ascii)
			{
				case ENTER		:	saveParametersFlag = TRUE;
										break;

				case ESCAPE		:	saveParametersFlag = FALSE;
										break;

				default			:	putchar('\07');
										ClearKeyboardBuffer();
										notDone = TRUE;
										break;
			}
      }
	   else
		{
			putchar('\07');
			ClearKeyboardBuffer();
		}
   }

	if (saveParametersFlag)
	{
   	int height = 5, width = 60;
	   int fileHandle, forceDialogFlags = FALSE;

      	/* move the current values of the local variables into the transfer buffer */
   	SetAccessParameters(transferBuffer, forceDialogFlags);

      	/* Don't let the user save parameters with SlowKeys or BounceKeys On!! */
			/* if SlowKeys of BounceKeys is on, turn off FilterKeys in the Transfer Buffer */
   	if ((fWaitTicks > 0 || fBounceTicks < 32760) && fFilterKeysOn == TRUE) transferBuffer[F_FILTER_KEYS_ON] = FALSE;

      	/* write the current data to the configuration file */
   	fileHandle = open(configurationFilename, O_CREAT | O_TRUNC | O_RDWR | O_BINARY, S_IREAD | S_IWRITE);
   	if (fileHandle != -1)
   	{
      	unsigned bytesWritten = write(fileHandle, (void *) transferBuffer, DATA_BLOCK_LENGTH);
      	close (fileHandle);
			if (bytesWritten == DATA_BLOCK_LENGTH)	DisplayAlertBox(SaveSuccessfulAlertText, height, width);
	   	else
			{
				DisplayAlertBox(SaveFailedAlertText, height, width);
				remove(configurationFilename);
			}
   	}
   	else DisplayAlertBox(SaveFailedAlertText, height, width);
	}
}	/* end SaveParameters() */


void DisplayCurrentParameters(void)
{
   _settextwindow(1, 1, 25, 80);
	_clearscreen(_GCLEARSCREEN);
	_settextposition(2, 1);

   charBufferLength = sprintf(charBuffer, "SerialKeys:\n");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    skSerialKeysOn: %s  skSerialKeysLoaded: %s\n",
						  				skSerialKeysOn ? "True" : "False", skSerialKeysLoaded ? "True" : "False");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
   charBufferLength = sprintf(charBuffer, "    skPortAddress: 0x%04X  skInterrupt: %d  skBaudRate: 0x%04X\n",
										skPortAddress, skInterrupt, skBaudRate);
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);

   charBufferLength = sprintf(charBuffer, "Keyboard Package:\n");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    fDialog: %s  fFilterKeysOn: %s  fOnOffFeedback: %s\n",
						  				fDialogFilterKeysOff ? "True" : "False",
						  				fFilterKeysOn ? "True" : "False",
						  				fOnOffFeedback ? "True" : "False");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    fWaitTicks: %u  fDelayTicks: %u  fRepeatTicks: %u  fBounceTicks: %u\n",
										fWaitTicks, fDelayTicks, fRepeatTicks, fBounceTicks);
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    fMaxDefault: %s  fClickOn: %s  fBounceOn: %s\n",
										fMaxDefault ? "True" : "False",
										fClickOn ? "True" : "False",
										fBounceOn ? "True" : "False");
   if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);

   charBufferLength = sprintf(charBuffer, "StickyKeys:\n");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
   charBufferLength = sprintf(charBuffer, "    sDialog: %s  sStickyKeysOn: %s  sClickOn: %s\n",
										sDialogStickyKeysOff ? "True" : "False",
										sStickyKeysOn ? "True" : "False",
										sClickOn ? "True" : "False");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    sOnOffFeedback: %s  sAudibleFeedback: %s\n",
										sOnOffFeedback ? "True" : "False",
										sAudibleFeedback ? "True" : "False");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    sTriState: %s  sTwoKeysOff: %s\n",
										sTriState ? "True" : "False",
										sTwoKeysOff ? "True" : "False");
   if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);


   charBufferLength = sprintf(charBuffer, "MouseKeys:\n");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    mDialog: %s  mMouseKeysOn: %s  mOnOffFeedback: %s\n",
										mDialogMouseKeysOff ? "True" : "False",
										mMouseKeysOn ? "True" : "False",
										mOnOffFeedback ? "True" : "False");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    mMouseKeysOverride: %s  mMaxSpeed: %u  mTimeToMaxSpeed: %u\n",
										mMouseKeysOverride ? "True" : "False", mMaxSpeed, mTimeToMaxSpeed);
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);


   charBufferLength = sprintf(charBuffer, "ToggleKeys:\n");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    tDialog: %s  tToggleKeysOn: %s  tOnOffFeedback: %s\n",
										tDialogToggleKeysOff ? "True" : "False",
										tToggleKeysOn ? "True" : "False",
										tOnOffFeedback ? "True" : "False");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);


   charBufferLength = sprintf(charBuffer, "TimeOut:\n");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    toDialog: %s  toTimeOut: %s  toOnOffFeedback: %s  toValue: %u\n",
										toDialogTimeOutOff ? "True" : "False",
										toTimeOut ? "True" : "False",
										toOnOffFeedback ? "True" : "False",
                              toValue);
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);


   charBufferLength = sprintf(charBuffer, "Handicap:\n");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    compDialog: %s  compDialogId: %s  compDialogAction: %s\n",
										compDialog ? "True" : "False",
										compDialogId ? "True" : "False",
										compDialogAction ? "True" : "False");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "    fSpaceSaver: %s  fComputerNotFound: %s  fHearingOn: %s\n",
										fSpaceSaver ? "True" : "False",
										fComputerNotFound ? "True" : "False",
										fHearingOn ? "True" : "False");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);
   charBufferLength = sprintf(charBuffer, "    fAccessAlreadyLoaded: %s\n", fAccessAlreadyLoaded ? "True" : "False");
	if (charBufferLength > LINE_LENGTH) exit(FALSE); else _outtext(charBuffer);

	ClearKeyboardBuffer();
   WaitForAnyKey();
	_clearscreen(_GCLEARSCREEN);
}	/* end DisplayCurrentParameters() */


void DisplayEquipmentParameters(void)
{
   _settextposition(16, 1);
   printf("eqButton1: %d, eqButton2: %d, eqCurrentButton: %d\n", eqButton1, eqButton2, eqCurrentButton);
   printf("eqComputerId: %d, eqMouseDriver: %d, eqMouseId: %d\n", eqComputerId, eqMouseDriver, eqMouseId);
   printf("eqCommBase: 0x%04x, eqExtendedSegment: 0x%04x\n", eqCommBase, eqExtendedSegment);
   printf("eqVector: %d, eqInjectKeys: %d, eqVideoType: %d\n", eqVector, eqInjectKeys, eqVideoType);
   printf("compDialig: %d, compDialogId: %d\n", compDialog, compDialogId);
   printf("eqComputerFlag: %d, fComputerNotFound: %d\n", eqComputerFlag, fComputerNotFound);
   printf("\n\nPress any key to continue");
	ClearKeyboardBuffer();
   WaitForAnyKey();
}  /* end DisplayEquipmentParameters() */
