/*
	CW : Character Windows

	keysynth.c : Extra shift state synthesis (KK => VK)

	(note: up for VK_SPACE and down/up for VK_ALT is handled by driver)
*/

#define COW
#include <cow.h>

#include <uevent.h>
#include <kkeyboar.h>
#include <vkey.h>


VOID FARPUBLIC
SynthesizeShiftKeys(kkNew, kkOld)
/*
  -- kkNew and kkOld should be the parameters passed to UpdateShiftKk
*/
REG WORD kkNew;
WORD	kkOld;
	{
	REG WORD kkDelta = kkNew ^ kkOld;

	if (kkDelta & KK_SHIFT)
#ifdef KANJI
		KeyboardMessage(0, LOBYTE(VK_SHIFT), VK_SHIFT, kkNew,
#else
		KeyboardMessage(LOBYTE(VK_SHIFT), VK_SHIFT, kkNew,
#endif
		    ~kkNew & KK_SHIFT);
	if (kkDelta & KK_CONTROL)
#ifdef KANJI
		KeyboardMessage(0, LOBYTE(VK_CONTROL), VK_CONTROL, kkNew,
#else
		KeyboardMessage(LOBYTE(VK_CONTROL), VK_CONTROL, kkNew,
#endif
		    ~kkNew & KK_CONTROL);
	/* NOTE: driver does this for KK_ALT */
	if (kkDelta & KK_SCRLOCK)
#ifdef KANJI
		KeyboardMessage(0, LOBYTE(VK_SCRLOCK), VK_SCRLOCK, kkNew,
#else
		KeyboardMessage(LOBYTE(VK_SCRLOCK), VK_SCRLOCK, kkNew,
#endif
		    ~kkNew & KK_SCRLOCK);
	if (kkDelta & KK_NUMLOCK)
#ifdef KANJI
		KeyboardMessage(0, LOBYTE(VK_NUMLOCK), VK_NUMLOCK, kkNew,
#else
		KeyboardMessage(LOBYTE(VK_NUMLOCK), VK_NUMLOCK, kkNew,
#endif
		    ~kkNew & KK_NUMLOCK);
	if (kkDelta & KK_CAPLOCK)
#ifdef KANJI
		KeyboardMessage(0, LOBYTE(VK_CAPLOCK), VK_CAPLOCK, kkNew,
#else
		KeyboardMessage(LOBYTE(VK_CAPLOCK), VK_CAPLOCK, kkNew,
#endif
		    ~kkNew & KK_CAPLOCK);
	}
