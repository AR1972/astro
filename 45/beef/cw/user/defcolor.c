/*
	COW : Character Oriented Windows

	defcolor : default colors
	NOTE : the use may define their own colors
	8 User Colors

*/

#define COW
#include <cow.h>

#include <uisa.h>
#include "color.h"

/* Font drawing information */
WORD PASCAL mpisaffont[isaMax] = {0};

SA PASCAL rgsa[isaMax]=
	{
	/* list : b&w ca, color ca */

	/* isaBackground : all backgrounds */
	{ caBlackOnWhite,	Ca(coWhite)			},
	/* isaHilite : Hilited items */
	{ caWhite,		CaMake(coBlack, coBrightWhite)	},
	/* isaGreyed : greyed items */
	{ caBlackOnWhite,	Ca(coWhite)			},
	/* isaEnabled : enabled items */
	{ caBlackOnWhite,	Ca(coWhite)			},
	/* isaDisabled : disabled items */
	{ caBlackOnWhite,	Ca(coBlack)			},

	/* isaAlert : for MessageBox alerts */
	{ caBlackOnWhite,	Ca(CoBright(coRed))		},

	/* isaDialogBox : background for dialogs */
	{ caBlackOnWhite,	Ca(coWhite)			},
	/* isaPushButton : push button color */
	{ caBlackOnWhite,	Ca(coBrightWhite)		},
	/* isaButtonDown : pushed button color */
	{ caWhite,		CaMake(coGrey, coWhite)		},
	/* isaListBox : listbox background */
	{ caBlackOnWhite,	Ca(coWhite)			},
	/* isaScrollbar : scroll bar Background & arrows */
	{ caBlackOnWhite,	Ca(coWhite)			},
	/* isaElevator : scroll bar elevator */
	{ caWhite,		CaMake(coGrey, coWhite)		},

	/* isaMenuBox : background for menus */
	{ caBlackOnWhite,	Ca(coWhite)			},
	/* isaMenu : menu bar color */
	{ caBlackOnWhite,	Ca(coWhite)			},
	/* isaMenuSelected : Selected menus */
	{ caWhite,		CaMake(coWhite, coBlack)	},
	/* isaMenuHilite : for single character */
	{ caBrightBlack, 	Ca(coBrightWhite)		},
	/* isaMenuHiliteSel : for single character (under selection) */
	{ caBrightWhite, 	CaMake(coBrightWhite, coBlack)	},
	/* isaItemHiliteSel : for single character (under selection) */
	{ caBrightWhite, 	CaMake(coWhite, coBrightWhite)	},

	/* isaDialogAccel : dialog accelerators */
	{ caBrightBlack,	Ca(coYellow)			},

	/* isaShadow : shadows */
	{ caWhite,		CaMake(coGrey, coBlack)		},

	/* isaUserMin : 16 USER COLORS */
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },

	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	{ caWhite, caWhite },
	};

