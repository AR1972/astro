/*
	COW : Character Oriented Windows

	itl.h	: Internationalization header
*/

#ifdef LANGUAGE_ENGLISH

#define	szOk		"OK"
#define cchOk		(2+4)
/* chAccelOk no accelerator */

#define	szYes		"Yes"
#define cchYes		(3+2)
#define	chAccelYes	'Y'

#define	szNo		"No"
#define cchNo		(2+4)
#define	chAccelNo	'N'

#define szCancel	"Cancel"
#define cchCancel	6
/* chAccelCancel no accelerator */

#define	szRetry		"Retry"
#define	cchRetry	5
#define	chAccelRetry	'R'

#define	szAbort		"Abort"
#define cchAbort	5
#define	chAccelAbort	'A'

#ifdef HELP_BUTTON
#define szHelp		"Help"
#define cchHelp 	(4+2)
#define chAccelHelp	'H'
#endif

#endif /*LANGUAGE_ENGLISH*/



#ifdef LANGUAGE_FRENCH

#define szOk		"OK"
#define cchOk		(2+4)
/* chAccelOk no accelerator */

#define szYes		"Oui"
#define cchYes		(3+2)
#define chAccelYes	'O'

#define szNo		"Non"
#define cchNo		(3+3)
#define chAccelNo	'N'

#define szCancel	"Annuler"
#define cchCancel	7
/* chAccelCancel no accelerator */

#define szRetry 	"R\202essayer"
#define cchRetry	9
#define chAccelRetry	'R'

#define szAbort 	"Annuler"
#define cchAbort	7
#define chAccelAbort	'A'

#endif /*LANGUAGE_FRENCH*/



#ifdef LANGUAGE_JAPANESE

#define	szOk		"\212\155\224\106"		/* kakunin */
#define cchOk		(4+2)
/* chAccelOk no accelerator */

#define	szYes		"Y/\202\315\202\242"		/* hai */
#define cchYes		(6+2)
#define	chAccelYes	'Y'

#define	szNo		"N/\202\242\202\242\202\246"	/* iie */
#define cchNo		(8+0)
#define	chAccelNo	'N'

#define szCancel	"\216\346\217\301"		/* torikeshi */
#define cchCancel	(4+2)
/* chAccelCancel no accelerator */

#define	szRetry		"R/\215\304\216\300\215\163"	/* saijikkou */
#define	cchRetry	(8+0)
#define	chAccelRetry	'R'

#define	szAbort		"A/\222\206\216\176"		/* cyuushi */
#define cchAbort	(6+2)
#define	chAccelAbort	'A'

#ifdef HELP_BUTTON
#define szHelp		"H/\203\167\203\213\203\166"	/* herupu */
#define cchHelp 	(8+0)
#define chAccelHelp	'H'
#endif

#endif /*LANGUAGE_JAPANESE*/
