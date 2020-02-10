/***
*defcolor.c
*
*	Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*	default color table, and color initialization.
*
*******************************************************************************/

#include <version.h>

/* Next, include COW's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>
#include <cw/color.h>
#include <uiext.h>

#ifndef UI_H
#include <ui.h>
#endif

/* Font drawing information */
//[3] Not currently used
//[3] WORD PASCAL mpisaffont[isaUserMax] = {0};

SA PASCAL rgsa[isaUserMax] = {
    /* list : b&w ca, color ca */
    /* isaBackground : all backgrounds */
    { caWhite,			caWhite 			},
    /* isaHilite : Hilited items */
    { caWhite,			caWhite 			},
    /* isaGreyed : greyed items */
    { caBlackOnWhite,		CaMake(coGrey, coWhite) 	},
    /* isaEnabled : enabled items */
    { caBlackOnWhite,		caBlackOnWhite			},
    /* isaDisabled : disabled items */
    { caBlackOnWhite,		CaMake(coGrey, coWhite) 	},

    /* isaAlert : for MessageBox alerts */
    { caBlackOnWhite,		caBlackOnWhite			},

    /* isaDialogBox : background for dialogs */
    { caBlackOnWhite,		caBlackOnWhite			},
    /* isaPushButton : push button color */
    { caBlackOnWhite,		caBlackOnWhite			},
    /* isaButtonDown : pushed button color */
    { caWhite,			caWhite 			},
    /* isaListBox : listbox background */
    { caBlackOnWhite,		caBlackOnWhite			},
    /* isaScrollbar : scroll bar Background & arrows */
    { caBlackOnWhite,		caBlackOnWhite			},
    /* isaElevator : scroll bar elevator */
    { caWhite,			caWhite 			},

    /* isaMenuBox : background for menus */
    { caBlackOnWhite,		caBlackOnWhite			},
    /* isaMenu : menu bar color */
    { caBlackOnWhite,		caBlackOnWhite			},
    /* isaMenuSelected : Selected menus */
    { caWhite,			caWhite 			},
    /* isaMenuHilite : for single character */
    { CaMake(coBrightWhite, coWhite), CaMake(coBrightWhite, coWhite) },
    /* isaMenuHiliteSel : for single character (under selection) */
    { caBrightWhite,		caBrightWhite			},
    /* isaItemHiliteSel : for single character (under selection) */
    { caBrightWhite,		caBrightWhite			},

    /* isaDialogAccel : dialog accelerators */
    { CaMake(coBrightWhite, coWhite), CaMake(coBrightWhite, coWhite) },

    /* isaShadow : shadows */
    { caWhite,			CaMake(coGrey, coBlack) 	},

    /* isaUserMin : 16 USER COLORS */
    /* isaEditWindow : color of edit window */
    { caWhite,			CaMake(coWhite, coBlue) 	},
    /* isaCurStmt : color of current statement */
    { caBrightWhite,		CaMake(coBrightWhite, coBlue)	},
    /* isaBreakpoint : color of breakpoint line */
    { caBlackOnWhite,		CaMake(coWhite, coRed)		},
    /* isaCurBreakpoint : color of current statement if it is a breakpoint line as well */
    { CaMake(coBrightWhite, coWhite), CaMake(coBrightWhite, coRed) },
    /* isaStatusLine : color of status line */
    { caBlackOnWhite,		CaMake(coBlack, coCyan) 	},
    /* isaStatusAlert : color for status line alert messages */
    { caBlackOnWhite, CaMake(coBrightWhite, coCyan)		}, //[5]
    /* isaStatusLock : color of Shift Lock status */
    { caBlackOnWhite,		CaMake(coBlack, coCyan) 	},
    /* isaDebugWindow : color of watch lines */
    { coWhite,			CaMake(coBlack, coCyan) 	},

    /* isaHelpWindow, isaSyntaxHelp : color of help window */
    { caWhite,			caWhite 			},
    /* isaIncludeFileHilite : color of Include File text */
    { caBrightWhite,		CaMake(coYellow, coBlue)	},
    /* isaListBoxHilite : color of list box hilite if different than isaHilite */
    { caWhite,			caWhite 			},
    /* isaWatchpointHilite : color of TRUE watchpoints */
    { caBrightWhite,		CaMake(coBrightWhite, coCyan)	},
    /* isaBold : bold help text */
    { caBrightWhite,		caBrightWhite			},
    /* isaItalic : italic help text */
    { CaMake(coBrightWhite, coWhite), CaMake(CoBright(coGreen), coBlack) },
    /* isaUnderline : underlined help text */
    { caBlackOnWhite,		caBlackOnWhite			},
    /* isaScratch : This is a scratch isa */
    { caWhite,			caWhite 			},
};


// [QH1]
typedef struct {
    WORD isa;
    BYTE caMono;
    BYTE caColor;
    } ISANEW;


//
// isa's to change if LCD or AMDEK, and colors to change them with.
//
ISANEW rgsaNoH[] = {
    { isaDisabled,		caBlackOnWhite, CaMake(coOrange, coWhite)},
    { isaMenuHilite,		caWhite,	CaMake(coRed, coWhite)	 },
    { isaMenuHiliteSel, 	caBlackOnWhite, CaMake(coGreen, coBlack) },
    { isaItemHiliteSel, 	caBlackOnWhite, CaMake(coGreen, coBlack) },
    { isaDialogAccel,		caWhite,	CaMake(coRed, coWhite)	 },
    { isaCurStmt,		caBlackOnWhite, CaMake(coGreen, coBlue)  }, //[5]
    { isaCurBreakpoint, 	caBlackOnWhite, CaMake(coGreen, coRed)	 }, //[5]
    { isaStatusAlert,		caBlackOnWhite, CaMake(coBlack, coCyan)  },
    { isaIncludeFileHilite,	caWhite,	CaMake(coYellow, coBlue) },
    { isaWatchpointHilite,	caBlackOnWhite, CaMake(coRed, coCyan)	 },
    { isaBold,			caBlackOnWhite, CaMake(coRed, coBlack)	 },
    { isaItalic,		caWhite,	CaMake(coGreen, coBlack) }
};


// [QH1]
//
//  ISA's to change if in QHELP mode
//
// WARNING: Remember that QBasic saves the edit window color, and possibly one
//    or two other colors, so be careful which colors you change here!
//
ISANEW rgsaQHelp[] = {
   { isaStatusLine,         caBlackOnWhite,     CaMake(coBlack, coWhite) },
   { isaStatusLock,         caBlackOnWhite,     CaMake(coBlack, coWhite) },
   { isaStatusAlert,        caBlackOnWhite,     CaMake(coBlack, coWhite) },
   { isaItalic,        caBrightWhite,     CaMake(CoBright(coGreen), coBlack) }
};

// [QH1]
// Alternate bold color for /NOHI mode, since the default Red looks awful.
//
ISANEW gsaQHNoH = { isaBold, caBlackOnWhite,     CaMake(coCyan, coBlack) };






/************************************************************************
*WORD FAR PASCAL fInitColorTable () - Initialize User interface color table
*Purpose:
*       Initialize user interface color table based on:
*
*       1. /b command line switch
*       2. /nohi command line switch
*       3. detection of monochrome or color display adapter
*
*Entry:
*       no parameters
*       important global variables:
*               cmdSwitches:
*                       bit flags indicating which command line switches
*                       were used when qb was invoked
*               fMonochrome:
*                       TRUE if monochrome adapter
*                       FALSE if color adapter
*
*Exit:
*	Returns TRUE if we are on an NEC (or a machine that uses the fore
*	ground highlight bit to reverse the colors), and otherwise FALSE.
*
*************************************************************************/
WORD FAR PASCAL
fInitColorTable ()
{
    REG1 short i;

// If /NOHI switch, then set new color options
    if (cmdSwitches & CMD_SW_NOH)
    {
        for (i = 0; i < (sizeof (rgsaNoH) / sizeof (rgsaNoH[0])); i++)
        {
            rgsa[rgsaNoH[i].isa].u.init.caMono  = rgsaNoH[i].caMono;
            rgsa[rgsaNoH[i].isa].u.init.caColor = rgsaNoH[i].caColor;
        }
        // [QH1]
        // QHELP - That Red bold color looks hideous, so change it to Cyan.
        if (cmdSwitches & CMD_SW_QHELP)
        {
            rgsa[isaBold].u.init.caMono  = gsaQHNoH.caMono;
            rgsa[isaBold].u.init.caColor = gsaQHNoH.caColor;
        }
    }

// [QH1]
//
// If QHelp mode, use new online-help colors.
//
    if (cmdSwitches & CMD_SW_QHELP)
    {
	     for (i = 0; i < (sizeof (rgsaQHelp) / sizeof (rgsaQHelp[0])); i++)
        {
	         rgsa[rgsaQHelp[i].isa].u.init.caMono  = rgsaQHelp[i].caMono;
	         rgsa[rgsaQHelp[i].isa].u.init.caColor = rgsaQHelp[i].caColor;
	     }

    }

    if (fMonochrome || (cmdSwitches & CMD_SW_MNO))
        for (i = 0; i < isaUserMax; i++)
            rgsa[i].u.draw.caSa = rgsa[i].u.init.caMono;


    return (FALSE);		// fReverse = FALSE on U.S. IBM
}








