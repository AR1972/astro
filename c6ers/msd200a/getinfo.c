/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * GETINFO.C - Source file with the primary GetInfo loop.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/********************************************************************
 * GetInfoSize - Returns the number of bytes required to store the
 *               specified record type.
 *
 * wRecordType - Record type requested.  NULL for a total of all
 *               record types.
 *
 * Returns: Count of bytes required to store the data.
 ********************************************************************/

INT GetInfoSize (WORD wRecordType, BOOL fHeaderRecord)
{
  WORD wHeaderSize;         /* Set to the size of the header record, */
                            /*   zero if the header record is not    */
                            /*   required.                           */

  /* If the header record was requested, get the header size only once */

  if (fHeaderRecord &&
      wRecordType != IDI_MSD_HEADER_RECORD &&
      wRecordType != IDI_ALL_RECORDS)
    wHeaderSize = GetInfoSize (IDI_MSD_HEADER_RECORD, FALSE);
  else
    wHeaderSize = 0;


  /* Return the size based on wRecordType */

  switch (wRecordType)
    {
      case IDI_ALL_RECORDS:
        {
          INT i;              /* Looping variable */
          INT iByteCount = 0; /* Count of bytes needed to store all records */

          for (i = 1; i < NMBR_OF_RECORDS; ++i)
            iByteCount += GetInfoSize (rwRecordTypes[i], FALSE);

          return (iByteCount);
        }

      case IDI_MSD_HEADER_RECORD:
        return (21);

      case IDI_CUSTOMER_INFORMATION:
        return (sizeof (CUSTINFO) + wHeaderSize);

      case IDI_SUMMARY_SCREEN:
        return (sizeof (SUMMARY_STRUCT) + wHeaderSize);

      case IDI_COMPUTER_RECORD:
        return (sizeof (COMPUTER_STRUCT) + wHeaderSize);

      case IDI_MEMORY_RECORD:
        return (sizeof (MEMORY_STRUCT) + wHeaderSize);

      case IDI_VIDEO_RECORD:
        return (sizeof (VIDEO_STRUCT) + wHeaderSize);

      case IDI_NETWORK_RECORD:
        return (sizeof (NETWORK_STRUCT) + wHeaderSize);

      case IDI_OS_VERSION_RECORD:
        return (sizeof (OS_VERSION_STRUCT) + wHeaderSize);

      case IDI_MOUSE_RECORD:
        return (sizeof (MOUSE_STRUCT) + wHeaderSize);

      case IDI_OTHER_ADAPTERS_RECORD:
        return (sizeof (OTHER_STRUCT) + wHeaderSize);

      case IDI_DISK_DRIVE_RECORD:
        return (sizeof (DISK_STRUCT) + wHeaderSize);

      case IDI_LPT_RECORD:
        return (sizeof (LPT_STRUCT) + wHeaderSize);

      case IDI_COM_RECORD:
        return (sizeof (COM_STRUCT) + wHeaderSize);

      case IDI_IRQ_RECORD:
        return (sizeof (IRQ_STRUCT) + wHeaderSize);

      case IDI_TSR_PROGRAMS_RECORD:
        return (GetTsrInfoSize() + wHeaderSize);

      case IDI_DEVICE_DRIVERS_RECORD:
        return (GetDeviceDriversSize() + wHeaderSize);

      default:
        return (0);
    }
}

/********************************************************************
 * GetInfo - Fills area pointed to by pStructForInfo with requested
 *           data.
 *
 * wRecordType    - Record type requested.  NULL to fill
 *                  pStructForInfo with all record types.
 * pStructForInfo - Pointer to data area to be filled by GetInfo.
 * fMinimumInfo   - Obtains the minimum amount of data, if TRUE.
 * fHeaderRecord  - Includes header record data if TRUE.
 * fReportFlag    - Used by routines that normally add data to an
 *                  existing window -- BIOS and Disk Drives for
 *                  example.  When TRUE, those routines obtain all
 *                  information before returning.
 *
 * Returns: TRUE if an error condition occured.
 ********************************************************************/

BOOL GetInfo (WORD wRecordType,
              VOID *pStructForInfo,
              BOOL fMinimumInfo,
              BOOL fHeaderRecord,
              BOOL fReportFlag)

{
  /* If the header record was requested, store the header record only once */

  if (fHeaderRecord &&
      wRecordType != IDI_MSD_HEADER_RECORD &&
      wRecordType != IDI_ALL_RECORDS)
    {
      GetInfo (IDI_MSD_HEADER_RECORD,
               pStructForInfo,
               fMinimumInfo,
               FALSE,
               fReportFlag);

      /* Now, move pStructForInfo past the header record */

      pStructForInfo = (BYTE *) pStructForInfo +
                       GetInfoSize (IDI_MSD_HEADER_RECORD, FALSE);
    }


  /* Return the size based on wRecordType */

  switch (wRecordType)
    {
      case IDI_ALL_RECORDS:
        {
          INT i;              /* Looping variable */
          INT iByteCount = 0; /* Count of bytes needed to store all records */

          for (i = 1; i < NMBR_OF_RECORDS; ++i)
            {
              GetInfo (wRecordType,
                       pStructForInfo,
                       fMinimumInfo,
                       FALSE,
                       fReportFlag);

              /* Now, move pStructForInfo past the new record */

              pStructForInfo = (BYTE *) pStructForInfo +
                               GetInfoSize (rwRecordTypes[i], FALSE);
            }
          return (FALSE);
        }

      case IDI_MSD_HEADER_RECORD:
        return (FALSE);

      case IDI_CUSTOMER_INFORMATION:
        return (GetCustInfo (pStructForInfo, fMinimumInfo));

      case IDI_SUMMARY_SCREEN:
        return (GetSummaryInfo (pStructForInfo, fMinimumInfo));

      case IDI_COMPUTER_RECORD:
        return (GetComputerInfo (pStructForInfo, fMinimumInfo));

      case IDI_MEMORY_RECORD:
        return (GetMemInfo (pStructForInfo, fMinimumInfo));

      case IDI_VIDEO_RECORD:
        return (GetVideoInfo (pStructForInfo, fMinimumInfo));

      case IDI_NETWORK_RECORD:
        return (GetNetworkInfo (pStructForInfo, fMinimumInfo));

      case IDI_OS_VERSION_RECORD:
        return (GetOsVersionInfo (pStructForInfo, fMinimumInfo));

      case IDI_MOUSE_RECORD:
        return (GetMouseInfo (pStructForInfo));

      case IDI_OTHER_ADAPTERS_RECORD:
        return (GetOtherInfo (pStructForInfo, fMinimumInfo));

      case IDI_DISK_DRIVE_RECORD:
        return (GetDiskInfo (pStructForInfo, fMinimumInfo));

      case IDI_LPT_RECORD:
        return (GetLptInfo (pStructForInfo, fMinimumInfo));

      case IDI_COM_RECORD:
        return (GetComInfo (pStructForInfo, fMinimumInfo));

      case IDI_IRQ_RECORD:
        return (GetIrqInfo (pStructForInfo, fMinimumInfo));

      case IDI_TSR_PROGRAMS_RECORD:
        return (GetTsrInfo (pStructForInfo, fMinimumInfo));

      case IDI_DEVICE_DRIVERS_RECORD:
        return (GetDeviceDriversInfo (pStructForInfo, fMinimumInfo));

      default:
        {
#if DEBUG
          /* A bogus record arrived */

          CHAR chBuffer[17];  /* Stores the message number */

          itoa (wRecordType, chBuffer, 10);

          ShowError (ERR_OK_BUTTON, "GetInfo", "Invalid Record", chBuffer);
          return (TRUE);
#endif
          return (FALSE);
        }
    }
}
