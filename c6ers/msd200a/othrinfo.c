/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * OTHRINFO.C - Source file for the Other Adapters detection.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/*********************************************************************
 * GetOtherInfo - Gets the Other Adapters (game/sound) information.
 *
 * pOther       - Other adapters information structure
 * fMinimumInfo - TRUE if minimum information is requested.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetOtherInfo (OTHER_STRUCT *pOther, BOOL fMinimumInfo)
{
  WORD i = 100;                 /* Check port 201h this many times */
  WORD wPortValue;              /* Game port's value               */
  static BOOL fGamePortExists = FALSE;  /* Used to retain the knowledge of */
                                /* a game adapter's presence.  When a game */
                                /* adapter exists on a system and there is */
                                /* no joystick attached, it can only be    */
                                /* detected once after a cold boot.  This  */
                                /* will cause MSD to continue to believe   */
                                /* that a game adapter exists even if it   */
                                /* was found only once.                    */


  /* Is a game adapter present on the system */
  if (fGamePortExists == FALSE)
    {
      while (i-- && (wPortValue = inp (0x201)) == 0xFF)
        ;

      fGamePortExists = (wPortValue != 0xFF) ? TRUE : FALSE;
    }

  pOther->fGameInstalled = fGamePortExists;

  /* Load up the values for the game adapter */
  if (fMinimumInfo == FALSE)
    GetGameAdapterValues (pOther);

  return (FALSE);
}


/*********************************************************************
 * GetGameAdapterValues - Gets the values of the joystick (button
 *                        settings and X-Y values).
 *
 * pOther - Structure for storing the data.
 *
 * Returns:  TRUE if the values are different than the last reading.
 *********************************************************************/

BOOL GetGameAdapterValues (OTHER_STRUCT *pOther)
{
  union REGS inregs, outregs;   /* int86 register structures   */
  BOOL  fDifferent = FALSE;     /* TRUE if values have changed */
  static WORD wAX = 0xFFFF;     /* Previous values             */
  static WORD wBX = 0xFFFF;
  static WORD wCX = 0xFFFF;
  static WORD wDX = 0xFFFF;
  static BYTE bAL = 0xFF;


  if (pOther->fGameInstalled)
    {
      /* Read Game Adapter Resitive Settings */

      inregs.h.ah = 0x84;
      inregs.x.dx = 0x0001;
      inregs.x.cflag = 0x00;

      int86 (0x15, &inregs, &outregs);

      pOther->wJoystickAX = outregs.x.ax;
      pOther->wJoystickAY = outregs.x.bx;
      pOther->wJoystickBX = outregs.x.cx;
      pOther->wJoystickBY = outregs.x.dx;


      /* Check to see if the values are different */

      if (outregs.x.ax != wAX ||
          outregs.x.bx != wBX ||
          outregs.x.cx != wCX ||
          outregs.x.dx != wDX)
        {
          fDifferent = TRUE;
          wAX = outregs.x.ax;
          wBX = outregs.x.bx;
          wCX = outregs.x.cx;
          wDX = outregs.x.dx;
        }


      /* Read Joystick Buttons */

      inregs.h.ah = 0x84;
      inregs.x.dx = 0x0000;
      int86 (0x15, &inregs, &outregs);

      pOther->fButtonA1 = (outregs.h.al & 0x10) ? TRUE : FALSE;
      pOther->fButtonA2 = (outregs.h.al & 0x20) ? TRUE : FALSE;
      pOther->fButtonB1 = (outregs.h.al & 0x40) ? TRUE : FALSE;
      pOther->fButtonB2 = (outregs.h.al & 0x80) ? TRUE : FALSE;


      /* Check against the previous values */

      if (outregs.h.al != bAL)
        {
          fDifferent = TRUE;
          bAL = outregs.h.al;
        }
    }

  return (fDifferent);
}


/*********************************************************************
 * SprintOtherInfo - Put Other Adapters information into a set of
 *                   strings to be printed or displayed.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintOtherInfo (OTHER_STRUCT *pOther,
                       CHAR szSumStrings[][MAX_SUMM_INFO + 5])
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD i = 0;               /* Looping variable                      */
  CHAR chBuffer[80];        /* Local string                          */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers  */
  WORD wAlignColumn;        /* Column to align titles                */


  /* Summary Strings */
  if (szSumStrings != NULL)
    {
      if (pOther->fGameInstalled)
        strncpy (szSumStrings[i++], pszGameAdapter, MAX_SUMM_INFO);

      /*
      if (pOther->fSoundInstalled)
        strncpy (szSumStrings[i++], pOther->szSoundName, MAX_SUMM_INFO);
      */

      return (NULL);
    }


  /* Overestimate the amount of space required for the strings */

  wNmbrStrings = 2;
  wAlignColumn = 14;

  if (pOther->fGameInstalled)
    {
      wNmbrStrings += 8;
      wAlignColumn = 16;
    }

  /*
  if (pOther->fSoundInstalled)
    {
      wNmbrStrings += 2;
    }
  */

  wNmbrChars = wNmbrStrings * REPORT_WIDTH;


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the information in place */

  /* Game Adapter: [Not] Detected */
  QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_GAME_ADAPTER],
               wAlignColumn);

  if (pOther->fGameInstalled)
    Qstrcat (pqszStrings[i], pszDetected);
  else
    Qstrcat (pqszStrings[i], pszNotDetected);

  /* Set the next pointer */
  PrepNextString (pqszStrings, i++);


  /* Joystick parameters */

  if (pOther->fGameInstalled)
    {
      /* Joystick A */

      /* X Value */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_JOY_A_X],
                   wAlignColumn);

      sprintf (chBuffer, "%u", pOther->wJoystickAX);
      Qstrcat (pqszStrings[i], chBuffer);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);


      /* Y Value */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_JOY_A_Y],
                   wAlignColumn);

      sprintf (chBuffer, "%u", pOther->wJoystickAY);
      Qstrcat (pqszStrings[i], chBuffer);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);


      /* Button 1 */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_JOY_A_BUTTON_1],
                   wAlignColumn);

      Qstrcat (pqszStrings[i], (pOther->fButtonA1) ? pszOn : pszOff);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);


      /* Button 2 */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_JOY_A_BUTTON_2],
                   wAlignColumn);

      Qstrcat (pqszStrings[i], (pOther->fButtonA2) ? pszOn : pszOff);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);


      /* Joystick B */

      /* X Value */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_JOY_B_X],
                   wAlignColumn);

      sprintf (chBuffer, "%u", pOther->wJoystickBX);
      Qstrcat (pqszStrings[i], chBuffer);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);


      /* Y Value */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_JOY_B_Y],
                   wAlignColumn);

      sprintf (chBuffer, "%u", pOther->wJoystickBY);
      Qstrcat (pqszStrings[i], chBuffer);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);


      /* Button 1 */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_JOY_B_BUTTON_1],
                   wAlignColumn);

      Qstrcat (pqszStrings[i], (pOther->fButtonB1) ? pszOn : pszOff);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);


      /* Button 2 */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_JOY_B_BUTTON_2],
                   wAlignColumn);

      Qstrcat (pqszStrings[i], (pOther->fButtonB2) ? pszOn : pszOff);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);
    }


#if 0
  /* Sound Device: [Not] Detected */
  QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_SOUND_DEVICE],
               wAlignColumn);

  if (pOther->fSoundInstalled)
    Qstrcat (pqszStrings[i], pszDetected);
  else
    Qstrcat (pqszStrings[i], pszNotDetected);

  /* Set the next pointer */
  PrepNextString (pqszStrings, i++);


  /* Sound Device parameters */

  if (pOther->fSoundInstalled)
    {
      /* Sound Device Name */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_SOUND_DEVICE],
                   wAlignColumn);

      Qstrcat (pqszStrings[i], pOther->szSoundName);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);


      /* Sound Device IRQ */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_SOUND_IRQ],
                   wAlignColumn);

      sprintf (chBuffer, "%u", pOther->wSoundIrq);
      Qstrcat (pqszStrings[i], chBuffer);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);


      /* Sound Device I/O Port */
      QstrcpyAlign (pqszStrings[i], paszOtherTitles[OTHER_SOUND_PORT],
                   wAlignColumn);

      sprintf (chBuffer, "%u", pOther->wSoundPort);
      Qstrcat (pqszStrings[i], chBuffer);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i++);
    }
#endif

  /* Set the last pointer to NULL */
  pqszStrings[i] = NULL;

  /* Return the pointer to pqszStrings */
  return (pqszStrings);
}
