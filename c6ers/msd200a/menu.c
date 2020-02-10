/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * SHOWINFO.C - Source file for displaying information
 ********************************************************************/


/* Include Files */

#include "msd.h"


#ifdef CW_INCLUDED


/********
// the rgmpvkeyid is a list of accelerators 
// don't use F1 -- reserved for HELP 
// list must end with a 0,0 entry

MPVKEYID rgmpvkeyid[]=
  {
  {0, 0},
  {VK_F2,         midNew},
  {VK_F3,         midOpen},
  {VK_F4,         midSave},
  {'C'-'@' | KK_CONTROL,  midColorChange1},
  {VK_F5,         midQuit},
  {VK_F7 | KK_ALT,    midOverlapMove},
  {VK_F7 | KK_SHIFT,    midOverlapSize},
  {VK_F7,         midOverlapPop},
  {'B'-'@' | KK_CONTROL,  midTimeBltRrc},
  {'F'-'@' | KK_CONTROL,  midTimeFillArc},
  {'T'-'@' | KK_CONTROL,  midTimeTextOut},
  {VK_F10,        midMode},
  {'L'-'@' | KK_CONTROL,  midColorLoad},
  {'S'-'@' | KK_CONTROL,  midColorSave},
  {VK_F2 | KK_SHIFT,    midOtherFlush},
  {'Q',         midQuit},
  {0,0}
  };

MPVKEYID *prgmpvkeyid = &rgmpvkeyid[0];

********/

//  -- respond to a menu command (i.e. WM_COMMAND)

VOID
MenuCommand(pwnd, mid)
PWND pwnd;
WORD mid;
{
  Unreferenced(pwnd);

  switch (mid)
    {
      case midFind:
        {
          WORD wReturnValue;  /* Return value from DialogBox */
          QSZ * pqszStrings;  /* String array                */


          wReturnValue = DialogBox (&dlgFindFile1, FindFileDlg1);

          if (wReturnValue == IDD_USER + 11 ||
              wReturnValue == IDOK)
            {
              BYTE * pVer = NULL;   /* Version information */


              /* Get the version information */
              pVer = GetFileVersion (pszBrowseTitle, FALSE);
              if (pVer == NULL && wReturnValue != IDOK)
                {
                  MessageBox ("Version information for",
                              pszBrowseTitle,
                              "is not available",
                              MB_OK | 0x8000);
                  break;
                }

              if (pVer != NULL)
                {
                  /* Put the version information into strings */
                  pqszStrings = SprintFileVersion (pVer);
                  if (pqszStrings == NULL)
                    break;

                  /* Display the version information */
                  CreateInfoWnd (pszBrowseTitle, pqszStrings, TRUE);
                  wReturnValue = IDD_USER + 11;
                }
            }

          if (wReturnValue == IDD_USER + 10 ||
              wReturnValue == IDOK)
            {
              /* Read the file */
              pqszStrings = ViewFile (pszBrowseTitle, FALSE);

              if (pqszStrings       != NULL &&
                  pqszStrings[0]    != NULL &&
                  pqszStrings[0][0] != '\0')
                {
                  /* Display the strings in an Info Window */
                  CreateInfoWnd (pszBrowseTitle, pqszStrings, TRUE);
                }
            }

          break;
        }

      case midReport:
        if (DialogBox (&dlgReport, ReportDlg) == IDOK)
          ReportFromCW();
        break;

      case midAutoexecBat:
        FindAndViewFile (rgszSystemFiles[0], rgwSystemFiles[0], FALSE);
        break;

      case midConfigSys:
        FindAndViewFile (rgszSystemFiles[1], rgwSystemFiles[1], FALSE);
        break;

      case midSystemIni:
        FindAndViewFile (rgszSystemFiles[2], rgwSystemFiles[2], FALSE);
        break;

      case midWinIni:
        FindAndViewFile (rgszSystemFiles[3], rgwSystemFiles[3], FALSE);
        break;

      case midMsmailIni:
        FindAndViewFile (rgszSystemFiles[4], rgwSystemFiles[4], FALSE);
        break;

      case midProtocolIni:
        FindAndViewFile (rgszSystemFiles[5], rgwSystemFiles[5], FALSE);
        break;

      case midDblSpaceIni:
	FindAndViewFile (rgszSystemFiles[6], rgwSystemFiles[6], FALSE);
	break;

      case midMemMakerSts:
	FindAndViewFile (rgszSystemFiles[7], rgwSystemFiles[7], FALSE);
        break;

      case midQuit:
        Exit(0);
        break;


      case midBrowser:
        if (DialogBox (&dlgMemoryBrowser, MemoryBrowserDlg) == IDOK)
          {
            if (pqszBrowseStrings       != NULL &&
                pqszBrowseStrings[0]    != NULL &&
                pqszBrowseStrings[0][0] != '\0')
              {
                /* Display the strings in an Info Window */
                CreateInfoWnd (pszBrowseTitle, pqszBrowseStrings, TRUE);
              }
            else
              MessageBox ("Search text not found", NULL, NULL, MB_OK | 0x8000);
          }
        break;

      case midBlockDisplay:
        DialogBox (&dlgMemoryBlockDisplay, MemoryBlockDisplayDlg);
        break;

      case midInsert:
        InsertCommand();
        break;

      case midPrtTst:
        if (DialogBox (&dlgTestPrinter, TestPrinterDlg) == IDOK)
          TestPrinter (tpValue.fPostscript,
                       tpValue.f8BitTest,
                       tpValue.fSerialTest,
                       tpValue.wPort);
        break;

      case midBlackWhite:
        SetIsaColors (fBlackWhite = !fBlackWhite);
        break;


      case midAbout:
        DialogBox (&dlgAbout, AboutDlg);
        break;


      default:
        break;

    }
}



/**********
//  -- put in names when they are asked for

CHAR * FAR
SzFromSid(sid)
WORD sid;
{
  static CHAR aszIsa[cchIsaMax+5];

  if (sid >= midIsaList)
    {
      aszIsa[0] = '~';
      strcpy(&aszIsa[1], rgszIsa[sid-midIsaList]);
      return(&aszIsa[0]);
    }
  else
    return ((CHAR *) sid);
}


//  -- Keep running track of the colour change menus as they open

VOID FAR
ColourChange3(mid)
WORD mid;
{
  static ISA isa;
  static WORD coFore;
  static WORD fHilite;
  static WORD coBack;
  static WORD fBlink;
  static WORD midLastColour = midCh3Black;
  static BOOL fForeground = FALSE;

  if (mid >= midIsaList)
    {
      isa = (ISA) (mid - midIsaList);
    }
  else if (mid == midCh3Foreground || mid == midCh3Background)
    {
      fForeground = mid & 1;
      GetColor(isa, &coFore, &fHilite, &coBack, &fBlink);
      CheckMenuItem(midLastColour, FALSE);
      if (fForeground)
        {
          CheckMenuItem(midCh3Hilight, fHilite);
          midLastColour = midCh3Black + coFore;
          CheckMenuItem(midLastColour, TRUE);
        }
      else
        {
          CheckMenuItem(midCh3Hilight, fBlink);
          midLastColour = midCh3Black + coBack;
          CheckMenuItem(midLastColour, TRUE);
        }
    }
  else if (mid == midCh3Hilight)
    {
      if (fForeground)
        {
          SetThisColor(isa, coFore, !fHilite, coBack, fBlink);
        }
      else
        {
          SetThisColor(isa, coFore, fHilite, coBack, !fBlink);
        }
    }
  else if (mid >= midCh3Black)
    {
      if (fForeground)
        {
          SetThisColor(isa, mid-midCh3Black, fHilite, coBack,
            fBlink);
        }
      else
        {
          SetThisColor(isa, coFore, fHilite, mid-midCh3Black,
            fBlink);
        }
    }
}

**********/

#endif /* CW_INCLUDED */
