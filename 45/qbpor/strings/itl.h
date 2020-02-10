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

#define szYes           "OK"
#define cchYes          (2+2)
#define chAccelYes      'O'

#define szNo		"Nao"
#define cchNo		(3+2)
#define chAccelNo       'N'

#define szCancel	"Cancelar"
#define cchCancel	8
/* chAccelCancel no accelerator */

#define szRetry 	"Repetir"
#define cchRetry	7
#define chAccelRetry    'R'

#define szAbort 	"Anular"
#define cchAbort	6
#define chAccelAbort    'A'

#ifdef HELP_BUTTON
#define szHelp		"Socorro"
#define cchHelp 	(7+2)
#define chAccelHelp     'S'
#endif
