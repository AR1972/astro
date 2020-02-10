/*  INT9.C  */

#include "skdefs.h"
#include "gideidef.h"
#include "vars.h"
#include "drv.h"
#include "int9.h"
#include "serkeys.h"
#include "init.h"
#include <dos.h>
#include <stdio.h>


/****************************************************************************/
BOOL keyAffectedByCaps (unsigned char key)
{
	int i;

	for (i=0; capsKeysTbl[i] && (capsKeysTbl[i] != key); i++);
	return (capsKeysTbl[i]);
}

/****************************************************************************/
BOOL keyAffectedByNum (unsigned char key)
{
	int i;

	if (key == kpperiod_key) return TRUE;
	for (i=0; keyPadKeysTbl[i] && (keyPadKeysTbl[i] != key); i++);
	return (keyPadKeysTbl[i]);
}


/****************************************************************************/
void kbdBufferInjectKeysRoutine (BYTE key)
{

	if (!inPauseCondition()) inPauseFlag = FALSE;
	if (key & 0x80) injectUpInKbdBuffer((BYTE)(key & 0x7f));
	else injectDownInKbdBuffer(key);
}


/****************************************************************************/
void injectDownInKbdBuffer (BYTE key)
{
	union {
		struct scanCodeType codes;
		unsigned int codeWord;
		BYTE ascii, scan;
		} tmp;
	int i;

	getBiosFlags();

	/* check for modifying key */
	switch (key) {
		case ignoreCode:
			altKeypad = 0;
			break;
		case lcontrol_key:
			kbFlag |= CTRL_DOWN_MASK;
			kbFlag1 |= LCTRL_DOWN_MASK;
			break;
		case rcontrol_key:
			kbFlag |= CTRL_DOWN_MASK;
			kbFlag2 |= RCTRL_DOWN_MASK;
			break;
		case lalt_key:
			kbFlag |= ALT_DOWN_MASK;
			kbFlag1 |= LALT_DOWN_MASK;
			break;
		case ralt_key:
			kbFlag |= ALT_DOWN_MASK;
			kbFlag2 |= RALT_DOWN_MASK;
			break;
		case lshift_key:
			kbFlag |= LSHIFT_DOWN_MASK;
			break;
		case rshift_key:
			kbFlag |= RSHIFT_DOWN_MASK;
			break;
		default:
			if (kbFlag & ALT_DOWN_MASK) {				/* is ALT key down? */
				if ((kbFlag & CTRL_DOWN_MASK) && ((key == delete_key) || (key == kpperiod_key))) {
					doReboot();
					}
				else {
					tmp.codes = scanTbl[key].alt;		/* get alt codes */
/*					if (tmp.codeWord == ignoreCode) altKeypad = 0;*/
					}
				}
			else {
				altKeypad = 0;
				if (kbFlag & CTRL_DOWN_MASK)					/* is CONTROL key down? */
					tmp.codes = scanTbl[key].ctrl;			/* get ctrl code */
				else if ((kbFlag & CAPS_MODE_MASK) && keyAffectedByCaps(key)) {
					if (kbFlag & (LSHIFT_DOWN_MASK | RSHIFT_DOWN_MASK))
						tmp.codes = scanTbl[key].base;		/* they cancel so get base code */
					else
						tmp.codes = scanTbl[key].shift;		/* get shift code */
					}
				else if ((kbFlag & NUM_MODE_MASK) && keyAffectedByNum(key)) {
					if (kbFlag & (LSHIFT_DOWN_MASK | RSHIFT_DOWN_MASK))
						tmp.codes = scanTbl[key].base;		/* they cancel so get base code */
					else
						tmp.codes = scanTbl[key].shift;		/* get shift code */
					}
				else if (kbFlag & (LSHIFT_DOWN_MASK | RSHIFT_DOWN_MASK))
					tmp.codes = scanTbl[key].shift;			/* get shift code */
				else tmp.codes = scanTbl[key].base;			/* just get base code */
				}
			switch (key) {
				case caps_key:
					if (!(kbFlag & CTRL_DOWN_MASK) && (!(kbFlag1 & CAPS_DOWN_MASK)))
						kbFlag ^= CAPS_MODE_MASK;
					kbFlag1 |= CAPS_DOWN_MASK;
					break;
				case numlock_key:
					if (!(kbFlag & CTRL_DOWN_MASK) && (!(kbFlag1 & NUM_DOWN_MASK)))
						kbFlag ^= NUM_MODE_MASK;
					kbFlag1 |= NUM_DOWN_MASK;
					break;
				case scroll_key:
					if (!(kbFlag & CTRL_DOWN_MASK) && (!(kbFlag1 & SCROLL_DOWN_MASK)))
						kbFlag ^= SCROLL_MODE_MASK;
					kbFlag1 |= SCROLL_DOWN_MASK;
					break;
				case insert_key:
					if (!(kbFlag1 & INSERT_DOWN_MASK))
						kbFlag ^= INSERT_MODE_MASK;
					kbFlag1 |= INSERT_DOWN_MASK;
					break;
				default:
					{
					}
				}


			if (kbFlag1 & PAUSE_MODE_MASK) {
				switch (key) {
					case caps_key:
					case scroll_key:
						if (kbFlag & CTRL_DOWN_MASK) kbFlag1 &= ~PAUSE_MODE_MASK;
						break;
					case insert_key:
						if (kbFlag & (CTRL_DOWN_MASK | ALT_DOWN_MASK)) kbFlag1 &= ~PAUSE_MODE_MASK;
						break;
					case numlock_key:
						break;
					default:
						kbFlag1 &= ~PAUSE_MODE_MASK;
					}
				if (!(kbFlag1 & PAUSE_MODE_MASK)) tmp.codeWord = ignoreCode;
				}
			else if (key == pause_key) {
				altKeypad = 0;
				if (kbFlag & CTRL_DOWN_MASK) {
					if (kbFlag & ALT_DOWN_MASK) {
						inPauseFlag = TRUE;
						kbFlag1 |= PAUSE_MODE_MASK;		/* pause */
						}
					else {
						handleBreak();							/* break */
						getBiosFlags();
						}
					}
				else {
					inPauseFlag = TRUE;
					kbFlag1 |= PAUSE_MODE_MASK;		/* pause */
					}
				}
			else if (key == print_key) {
				altKeypad = 0;
				if (kbFlag & ALT_DOWN_MASK) {			/* sys req */
					if (!(kbFlag1 & SYSREQ_DOWN_MASK)) {
						kbFlag1 |= SYSREQ_DOWN_MASK;
						_asm mov ax,0x8500;
						_enable();
						_asm	int 15h;
						}
					}
				else if (!(kbFlag & CTRL_DOWN_MASK)) _asm  int 05h;		/* print screen */
				}
			else if (kbFlag & ALT_DOWN_MASK) {
				for (i=0; keyPadKeysTbl[i] && (keyPadKeysTbl[i] != key); i++);
				if (keyPadKeysTbl[i]) altKeypad = (altKeypad * 10) + i;
				else {
					switch (key) {
						case caps_key:
						case numlock_key:
						case scroll_key:
						case insert_key:
							break;
						default:
							altKeypad = 0;
							break;
						}
					}
				}
			else altKeypad = 0;
			if (tmp.codeWord != ignoreCode) putInKbdBuffer(tmp.codeWord);
		}
	putBiosFlags();
}			



/****************************************************************************/
void injectUpInKbdBuffer (unsigned char key)
{
	unsigned int kpVal;

  	getBiosFlags();
	/* check for modifying key */
	switch (key) {
		case ignoreCode:
			break;
		case lcontrol_key:
			kbFlag1 &= ~LCTRL_DOWN_MASK;
			if (!(kbFlag2 & RCTRL_DOWN_MASK))
				kbFlag &= ~CTRL_DOWN_MASK;
			break;
		case rcontrol_key:
			kbFlag2 &= ~RCTRL_DOWN_MASK;
			if (!(kbFlag1 & LCTRL_DOWN_MASK))
				kbFlag &= ~CTRL_DOWN_MASK;
			break;
		case lshift_key:
			kbFlag &= ~LSHIFT_DOWN_MASK;
			break;
		case rshift_key:
			kbFlag &= ~RSHIFT_DOWN_MASK;
			break;
		case lalt_key:
			kbFlag1 &= ~LALT_DOWN_MASK;
			if (!(kbFlag2 & RALT_DOWN_MASK))
				kbFlag &= ~ALT_DOWN_MASK;
			if ((kpVal=altKeypad) != 0) {
				putInKbdBuffer(kpVal);
				altKeypad = 0;
				}
			break;
		case ralt_key:
			kbFlag2 &= ~RALT_DOWN_MASK;
			if (!(kbFlag1 & LALT_DOWN_MASK))
				kbFlag &= ~ALT_DOWN_MASK;
			if ((kpVal=altKeypad) != 0) {
				putInKbdBuffer(kpVal);
				altKeypad = 0;
				}
			break;
		case caps_key:
			kbFlag1 &= ~CAPS_DOWN_MASK;
			break;
		case numlock_key:
			kbFlag1 &= ~NUM_DOWN_MASK;
			break;
		case scroll_key:
			kbFlag1 &= ~SCROLL_DOWN_MASK;
			break;
		case insert_key:
			kbFlag1 &= ~INSERT_DOWN_MASK;
			break;
		case print_key:
			/* sys req */
			if (kbFlag1 & SYSREQ_DOWN_MASK) {
				kbFlag1 &= ~SYSREQ_DOWN_MASK;
				_asm mov ax,0x8501;
				_enable();
				_asm	int 15h;
				}
		default:
			{
			}
		}

	putBiosFlags();
}
