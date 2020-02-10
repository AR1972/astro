/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * SPRNINFO.C - Source file for generating info strings for display
 *              or report.
 ********************************************************************/


/* Include Files */

#include "msd.h"

/********************************************************************
 * SprintInfo - Converts data structure into an array of strings.
 *
 * wRecordType     - Record type requested.
 * pStructWithInfo - Pointer to record containing the data.
 * szSumStrings    - Summary strings.  NULL if full strings are to
 *                   be reported, non-NULL if summary strings are to
 *                   be filled.
 * fReportFlag     - TRUE if printing to a report.
 *
 * Returns: Pointer to the array of string pointers.  NULL pointer
 *          indicates that an error occured.
 ********************************************************************/

QSZ * SprintInfo (WORD wRecordType,
                  VOID *pStructWithInfo,
                  CHAR szSumStrings[][MAX_SUMM_INFO + 5],
                  BOOL fReportFlag)
{
  QSZ *pqszStrings = NULL;  /* Pointer to the array of pointers to the */
                            /*   string area.                          */


  if (szSumStrings != NULL                         &&
      (wRecordType < IDI_FIRST_RECORD_TO_SUMMARIZE ||
       wRecordType > IDI_LAST_RECORD_TO_SUMMARIZE))
    return (NULL);

  switch (wRecordType)
    {
      case IDI_CUSTOMER_INFORMATION:
        {
          /* Put the customer titles and strings together */

          pqszStrings = SprintCustInfo (pStructWithInfo);

          /* Return the pointer to pqszStrings */

          break;
        }

      case IDI_SUMMARY_SCREEN:
        {
          pqszStrings = SprintSummaryInfo (paszButtonNames, pStructWithInfo);

          break;
        }

      case IDI_COMPUTER_RECORD:
        {
          pqszStrings = SprintComputerInfo (pStructWithInfo, szSumStrings);

          break;
        }

      case IDI_MEMORY_RECORD:
        {
          pqszStrings = SprintMemInfo (MEM_ALL,
                                       pStructWithInfo,
                                       szSumStrings,
                                       TRUE);

          break;
        }

      case IDI_VIDEO_RECORD:
        {
          pqszStrings = SprintVideoInfo (pStructWithInfo, szSumStrings);

          break;
        }

      case IDI_NETWORK_RECORD:
        {
          pqszStrings = SprintNetworkInfo (pStructWithInfo, szSumStrings);

          break;
        }

      case IDI_OS_VERSION_RECORD:
        {
          pqszStrings = SprintOsVersionInfo (pStructWithInfo, szSumStrings);

          break;
        }

      case IDI_MOUSE_RECORD:
        {
          pqszStrings = SprintMouseInfo (pStructWithInfo, szSumStrings);

          break;
        }

      case IDI_OTHER_ADAPTERS_RECORD:
        {
          pqszStrings = SprintOtherInfo (pStructWithInfo, szSumStrings);

          break;
        }

      case IDI_DISK_DRIVE_RECORD:
        {
          pqszStrings = SprintDiskInfo (pStructWithInfo, szSumStrings);

          break;
        }

      case IDI_LPT_RECORD:
        {
          pqszStrings = SprintLptInfo (pStructWithInfo, szSumStrings);

          break;
        }

      case IDI_COM_RECORD:
        {
          pqszStrings = SprintComInfo (pStructWithInfo, szSumStrings);

          break;
        }

      case IDI_IRQ_RECORD:
        {
          pqszStrings = SprintIrqInfo (pStructWithInfo);

          break;
        }

      case IDI_TSR_PROGRAMS_RECORD:
        {
          pqszStrings = SprintTsrInfo (pStructWithInfo, TRUE);

          break;
        }

      case IDI_DEVICE_DRIVERS_RECORD:
        {
          pqszStrings = SprintDeviceDriverInfo (pStructWithInfo);

          break;
        }

      default:
        pqszStrings = NULL;
        break;
    }


  return (pqszStrings);
}
