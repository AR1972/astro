
//WARNING: This file contains some of the same constants as DLG.INC
//WARNING: When making a change to one, make it to both!

#include <cw/csdm.h>
#include <cw/csdmtmpl.h>

#define DLG_CONST	FAR
TMC NEAR PASCAL 	TmcDoDlgFar(VOID FAR *, WORD, HCAB);

#define isNothing	0xffff
#define HELP_BUTTON


#include <uihelpid.h>	// define help Ids for all the dialogs

#define tmcMake 	(tmcUserMin+0)
#define tmcMakeExit	(tmcUserMin+1)
#define tmcSzFileName	(tmcUserMin+2)
#define tmcFileType	(tmcUserMin+3)
			// NOTE: next one must start at +6
