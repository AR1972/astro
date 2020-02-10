/*
	COW : Character Oriented Windows

	itl.h   : Internationalization header

	This contains strings for use in QBASIC dialog boxes.

	You have a string in quotes, followed by its length +
	the total number of spaces to have on each side of
	the string.

	eg. "OK" is 2 long, the 4 means put 4/2=2 spaces on
	    each side of the word when it is being displayed.
	    make sure you leave the brackets around the numbers
	    ie, 2+4 is incorrect, (2+4) is correct.

	Some also have their hotkey following the key and the
	size, make sure that you leave the single qutoes (')
	around these characters.

	You may have to make some adjustments to the .des files
	when you have changed this file.

	eg, say you translate "OK" to "Okay", then the size
	might go to (4+4), or whatever you want.
	Then you would go to the .des files.
	lines with the following would have to be modified:

	OK_BUTTON AT (col, row, 6, 1) ACT...........
				^

	to

	OK_BUTTON AT (col, row, 8, 1) ACT...........
				^

*/


#define szOk            "OK"
#define cchOk           (2+4)
/* chAccelOk no accelerator */

#define szYes           "Yes"
#define cchYes          (3+2)
#define chAccelYes      'Y'

#define szNo            "No"
#define cchNo           (2+4)
#define chAccelNo       'N'

#define szCancel        "Cancel"
#define cchCancel       6
/* chAccelCancel no accelerator */

#define szRetry         "Retry"
#define cchRetry        5
#define chAccelRetry    'R'

#define szAbort         "Abort"
#define cchAbort        5
#define chAccelAbort    'A'

#ifdef HELP_BUTTON
#define szHelp          "Help"
#define cchHelp         (4+2)
#define chAccelHelp     'H'
#endif

