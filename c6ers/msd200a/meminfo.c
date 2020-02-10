/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * MEMINFO.C - Source file for obtaining memory information.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/*********************************************************************
 * GetMemInfo - Gets the memory information.
 *
 * pMem         - Pointer to memory information structure.
 * fMinimumInfo - TRUE if minimum info is requested.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetMemInfo (MEMORY_STRUCT *pMem, BOOL fMinimumInfo)
{
  BOOL fReturnValue = FALSE;  /* Return value from various GetInfo calls */
  WORD wSize;                 /* Size, in bytes, to store the data       */
  COMPUTER_STRUCT *pComputer = NULL;  /* Computer info structure pointer */
  WORD wProcessor;            /* Stores the processor type               */
  WORD wBusType;              /* Stores the computer's bus type          */


  wSize = GetInfoSize (IDI_COMPUTER_RECORD, FALSE);

  if (wSize == 0)
    return (TRUE);

  pComputer = malloc (wSize);

  if (pComputer == NULL)
    {
      OutOfMemory();
      return (TRUE);
    }

  /* Zero out the structure */
  memset (pComputer, '\0', wSize);

  fReturnValue = GetComputerIrqInfo (pComputer);
  if (fReturnValue)
    {
      free (pComputer);
      return (fReturnValue);
    }

  wProcessor = pComputer->wProcessor;
  wBusType   = pComputer->wBusType;
  free (pComputer);


  /* Initialize the memory info structures */
  MInitialize ((MEMORY_STRUCT FAR *) pMem);

  /* Get conventional memory size */
  Get_Conv_Mem_Info ((MEMORY_STRUCT FAR *) pMem);

  if (wProcessor >= _80286)
    {
      /* Get installed extended according to CMOS */
      GetCMOSExtended ((MEMORY_STRUCT FAR *) pMem);

      /* Get non-XMS extended memory size */
      Get_Ext_Mem_Info ((MEMORY_STRUCT FAR *) pMem);

      /* Get XMS extended memory info */
      Get_Xmm_Info ((MEMORY_STRUCT FAR *) pMem, wProcessor);

      /* Make sure we're on at least a 386 for DPMI information */
      if (wProcessor != _80286)
        GetDPMIInfo ((MEMORY_STRUCT FAR *) pMem);
    }

  /* Check for an EMM driver */
  Test_For_Emm ((MEMORY_STRUCT FAR *) pMem);

  if (pMem->iEmm_Is_There)
    {
      /* Get expanded memory info */
      Get_Emm_Info ((MEMORY_STRUCT FAR *) pMem);

      /* If on a 386 or greater */
      if (/* !(pMem->iDPMIPresent) && */ wProcessor >= _80386)
        {
          /* check for and get VCPI info */
          GetVCPIInfo ((MEMORY_STRUCT FAR *) pMem);
        }
    }

  if (fMinimumInfo)
    return (FALSE);

  /* Get the visual memory map */
  Get_VisualMemMap ((MEMORY_STRUCT FAR *) pMem, wBusType);

  /* Fill the visual memory map overlay with EMS page frame and UMBs */
  VisualMapOverlay ((MEMORY_STRUCT FAR *) pMem);

  return (FALSE);
}


/************************************************************************
*
* Function MInitialize()
*
* This function initializes fields in the Mem_Info_St structure to
* zero values.
*
************************************************************************/

int MInitialize (MEMORY_STRUCT FAR * pMem_Info)

{
  /* Initialize the fields in the Mem_Info_St structure */
  _fmemset (pMem_Info, '\0', sizeof (MEMORY_STRUCT));

  return (0);
}

/***************************************************************************
*
* Function Get_Conv_Mem_Info()
*
* This function gets the amount of conventional memory in bytes and the
* amount of free conventional memory in bytes. It uses a call to BIOS
* function Int 12H.                                                                    *
*
*
* Mem_Info_St Fields Set
* ----------------------
*
* lConv_Mem     = The amount of conventional memory in bytes
* lFree_Conv_Mem  = The amount of free conventional memory in bytes
*
*
* Local Variables Used                                      
    *
* --------------------
*
*
* inregs, outregs : Used for reading and writing the general purpose
*         registers.
* segment_prefix  : Used to hold the segment address of the program
*         segment prefix.
*
***************************************************************************/

int Get_Conv_Mem_Info (MEMORY_STRUCT FAR * pMem_Info)

{
  union REGS inregs, outregs;
  long segment_prefix;

  int86(0x12, &inregs, &outregs);

  pMem_Info->lConv_Mem = outregs.x.ax * 1024L;

  segment_prefix = 0L;
  segment_prefix = segment_prefix + ((long)(_psp) * 0x10);

  pMem_Info->lFree_Conv_Mem = pMem_Info->lConv_Mem - segment_prefix;

  return (0);
}

/***************************************************************************
*
* Function Get_Ext_Mem_Info()
*
* This function gets the amount of extended memory in bytes. It uses a
* call to BIOS function Int 15H, subfunction 88H.
*
*
* Mem_Info_St Fields Set
* ----------------------
*
* lExt_Mem      = The amount of extended memory in bytes
*
*
* Local Variables Used
* --------------------
*
* inregs, outregs : Used for reading and writing the general purpose
*         registers.
*
***************************************************************************/

int Get_Ext_Mem_Info (MEMORY_STRUCT FAR * pMem_Info)

{
  union REGS inregs, outregs;
  static int iFirstTime = 1;


  /* Check to see if this is the first time this routine was called.
     We do this because of the XMS memory routine which is called
     after this. In the XMS memory routine, if an XMS driver is found,
     the driver hooks into INT 15 calls. Subsequent calls to INT 15
     to get the amount of extended memory then return 0K. By calling
     this routine first and then never checking it again, we are able
     to get a valid value for extended memory at least the first time
     we are run on the machine */

  if (iFirstTime)
  {
    inregs.h.ah = 0x88;
    int86(0x15, &inregs, &outregs);

    pMem_Info->iExt_Mem = outregs.x.ax;

    iFirstTime = 0;
  }

  return (0);
}

/***************************************************************************
*
* Function GetCMOSExtended ()
*
* This function gets the amount of installed extended memory according to
* the CMOS.
*
***************************************************************************/

int GetCMOSExtended (MEMORY_STRUCT FAR * pMem_Info)

{
  int iHighByte, iLowByte, iTotalMemory;
  int iLoop;

  outp (0x70, 0x17);

  for (iLoop = 0; iLoop < 100; iLoop++)
    ;

  iLowByte = inp (0x71);


  outp (0x70, 0x18);

  for (iLoop = 0; iLoop < 100; iLoop++)
    ;

  iHighByte = inp (0x71);


  iTotalMemory = (iHighByte << 8) + iLowByte;

  pMem_Info->iCMOSExtended = iTotalMemory;

  return (0);
}

/***************************************************************************
*
* Function Test_For_Emm()
*
* This function tests for the existence of an EMM driver. It makes a call
* to Int 21H Function 35H to get the address of the interrupt handler for
* the EMM driver.  It then looks at offset 0AH of the device header to
* check for the guaranteed name of the EMM driver, EMMXXXX0. If the
* guaranteed name is found, there is an EMM driver installed and the value
* True is put in the iEmm_Is_There field of the Mem_Info_St structure.
* If the guaranteed name is not found, a value of False is put in the
* iEmm_Is_There field of the Mem_Info_St structure.
*
*
* Mem_Info_St Fields Set
* ----------------------
*
* iEmm_IS_There  = 1 if an EMM driver is found, 0 if not
*
*
* Local Variables Used
* --------------------
*
* inregs, outregs:  For loading and reading the general purpose regs.
* segments     :  For loading and reading the segment regs.
* addr_of_emm  :  The base address of the EMM driver, if it is there.
* ptr_to_emm   :  A pointer to the base address of the EMM driver
* name_field   :  An array to hold the string found at the name field of
*         device header of the EMM driver.
* test_name    :  An array to hold the guaranteed name of the EMM
*         driver, EMMXXXX0.
* index      :  Used for loop control.
* True, False  :  Used for Boolean testing.
*
***************************************************************************/

int Test_For_Emm (MEMORY_STRUCT FAR * pMem_Info)

{
  union REGS inregs, outregs;
  struct SREGS segments;
  unsigned long addr_of_emm;
  char far *ptr_to_emm = NULL;
  char name_field[9];
  char test_name[9] = {
        'E', 'M', 'M', 'X',
        'X', 'X', 'X', '0', '\0'
  };
  int index;
  int True = 1;
  int False = 0;

  /* load AL with the EMM int vector of 67H and prepare
     to call Int 21H, Function 35H */

  inregs.h.al = 0x67;
  inregs.h.ah = 0x35;

  /* After this call, the ES reg will contain the EMM handler address
     if it is there */

  int86x(0x21, &inregs, &outregs, &segments);
  segread(&segments);

  addr_of_emm = 0L;

  /* Load addr_of_emm with address in the ES reg, then shift it left
     16 bits to get it into the high order word */

  addr_of_emm = (addr_of_emm | segments.es) <<16;

  /* Add the offset of 0AH to make ptr_to_emm point to the name field
     in the device header of the EMM driver */

  ptr_to_emm = (unsigned char far *)addr_of_emm + 0x0A;

  /* Load the the first 8 characters beginning at the address pointed
     to by the pointer ptr_to_emm, then add a terminating \0 */

  for(index = 0; index < 8; ++index)
  {
    name_field[index] = *ptr_to_emm;
         ++ptr_to_emm;
  }
  name_field[index] = '\0';

  /* Compare the characters loaded into name_field[] against the
     guaranteed EMM driver name stored in test_name[]. Set
     iEmm_Is_There to True if equal, False if not */

  if (strcmp(test_name, name_field))
    pMem_Info->iEmm_Is_There = False;
  else
    pMem_Info->iEmm_Is_There = True;

  return (0);
}

/**************************************************************************
*
* Function Get_Emm_Info()
*
* This function gets the EMM driver version, the total number of 16K EMM
* pages and the number of 16K EM pages currently available.
*
*
* Mem_Info_St Fields Set
* ----------------------
*
* iEmm_VersionMajor = The EMS version supported by the EMS driver
* iEmm_VersionMinor = The EMS version supported by the EMS driver
* iTotal_Emm_Pages  = The total number of logical expanded memory pages
*                     present
* iFree_Emm_Pages   = The number of logical pages not allocated
* iEmm_Ver_Err      = Error code returned while getting the version
* iEmm_Size_Err     = Error code returned while getting allocation info
*
*
* Local Variables Used
* --------------------
*
* inregs, outregs  : Used to load and read the general purpose
*            registers
* int_portion    : Used to figure the integer portion of the version
* frac_portion     : Used to figure the fractional portion of the
*            version
* uiPageFrameAddress : Used to get the EMS page frame address
* uiMappablePages  : Used to get the number of LIM 4.0 mappable pages
*
**************************************************************************/

int Get_Emm_Info(MEMORY_STRUCT FAR * pMem_Info)

{
  union REGS inregs, outregs;
  struct SREGS segments;
  int int_portion, frac_portion;
  unsigned int uiPageFrameAddress;
  unsigned int *pPtrToConfigBuffer = NULL;


  /* Get the EMM version */

  inregs.h.ah = 0x46;
  int86(0x67, &inregs, &outregs);

  if (outregs.h.ah == 0x0)
  {
    frac_portion = (outregs.h.al & 0x0F);
    int_portion = ((outregs.h.al >> 4) & 0x0F);
    pMem_Info->iEmm_VersionMajor = int_portion;
    pMem_Info->iEmm_VersionMinor = frac_portion;
  }
  else
    pMem_Info->iEmm_Ver_Err = outregs.h.ah;


  /* Get the total number of EM pages and the number free using a
     LIM 3.X call */

  inregs.h.ah = 0x42;
  int86(0x67, &inregs, &outregs);

  if (outregs.h.ah)
    pMem_Info->iEmm_Size_Err = outregs.h.ah;
  else
  {
    pMem_Info->iTotal_Emm_Pages = outregs.x.dx;
    pMem_Info->iFree_Emm_Pages = outregs.x.bx;
  }


  /* If LIM 4.0 was detected use a LIM 4.0 specific call to get the
     total number of raw EM pages and the number of raw EM pages
     available */

  if (pMem_Info->iEmm_VersionMajor == 4 && pMem_Info->iEmm_VersionMinor == 0)
    {
      /* Get the size of a raw EM page from the EM hardware configuration */

      if (( pPtrToConfigBuffer = (unsigned int *) malloc (10)) == NULL )
        pMem_Info->iEmm_Size_Err = outregs.h.ah; /* The malloc failed */
      else
        {
          /* Get the EM hardware configuration */

          inregs.x.ax = 0x5900;
          segread (&segments);
          segments.es = (unsigned int) FP_SEG (pPtrToConfigBuffer);
          inregs.x.di = (unsigned int) FP_OFF (pPtrToConfigBuffer);

          int86x (0x67, &inregs, &outregs, &segments);


          if (outregs.h.ah) /* There was an error getting the EM config */
            pMem_Info->iEmm_Size_Err = outregs.h.ah;
          else
            {
              /* At this point pPtrToConfigBuffer is pointing to an integer
                 value which is the raw EM page size, in paragraphs. Next,
                 get the total number of raw pages and the number available */

              inregs.x.ax = 0x5901;
              int86 (0x67, &inregs, &outregs);

              if (outregs.h.ah) /* There was an error returned */
                pMem_Info->iEmm_Size_Err = outregs.h.ah;
              else
                {
                  pMem_Info->iTotal_Emm_Pages = outregs.x.dx;
                  pMem_Info->iFree_Emm_Pages = outregs.x.bx;
                }
            }
        }
    }

  /* Free up the EMS pointer */
  free (pPtrToConfigBuffer);

  /* Get the EMS page frame address */

  inregs.h.ah = 0x41;
  int86(0x67, &inregs, &outregs);
  uiPageFrameAddress = outregs.x.bx;

  if (!outregs.h.ah) /* A page frame address was returned */
    pMem_Info->iPageFrameAddress = uiPageFrameAddress;

  return (0);
}

/************************************************************************
*
* Function GetVCPIInfo ()
*
* This function detects whether or not a VCPI control program is present.
* If one is, information about the amount of VCPI memory available is
* gathered.
*
************************************************************************/

int GetVCPIInfo (MEMORY_STRUCT FAR * pMem_Info)

{
  union REGS inregs, outregs;
  struct SREGS segments;

  inregs.x.ax = 0xDE00;
  int86x (0x67, &inregs, &outregs, &segments);

  if( outregs.h.ah == 0x00 )
  {
    pMem_Info->iVCPIPresent = 1;
    pMem_Info->iVCPIMajorVersion = outregs.h.bh;
    pMem_Info->iVCPIMinorVersion = outregs.h.bl;

    /* Query the amount of available VCPI memory */

    inregs.x.ax = 0xDE03;
    int86x (0x67, &inregs, &outregs, &segments);

    if (outregs.h.ah == 0x00 )
    pMem_Info->iVCPIAvailMemory = outregs.x.dx * 4;
  }

  return (0);
}

/************************************************************************
*
* Function GetDPMIInfo ()
*
* This function detects whether or not a DPMI control program is present.
* If one is, information about the amount of DPMI memory available is
* gathered by calling the function QueryDPMIMemInfo ().
*
************************************************************************/

int GetDPMIInfo (MEMORY_STRUCT FAR * pMem_Info)

{
  union REGS inregs, outregs;
  struct SREGS segments;

  inregs.x.ax = 0x1687;
  int86x (0x2F, &inregs, &outregs, &segments);

  if (outregs.x.ax == 0x00)
  {
    pMem_Info->iDPMIPresent = 1;
    pMem_Info->iDPMIMajorVersion = outregs.h.dh;
    pMem_Info->iDPMIMinorVersion = outregs.h.dl;

    /* Query the DPMI control program */
/*
    QueryDPMIMemInfo (pMem_Info);
*/
  }

  return (0);
}

/************************************************************************
*
* Function QueryDPMIMemInfo ()
*
* This function queries the DPMI control program for amounts of DPMI
* memory.
*
************************************************************************/

int QueryDPMIMemInfo (MEMORY_STRUCT FAR * pMem_Info)

{
  long DPMIControl;
  unsigned short wRMMemAvail;
  DPMI_STRUCT FAR * pDPMIMemInfo = NULL;

  pDPMIMemInfo = (DPMI_STRUCT FAR *) &(pMem_Info->DPMIMemInfo);

  _asm
  {
    ; Get the entry point of the DPMI control program
    ; and save it

    mov   ax, 1687h
    int   2Fh
    test  ax, ax
    jnz   Failure
    mov   word ptr [DPMIControl],di
    mov   word ptr [DPMIControl+2],es

    ; if si != 0 then we need to allocate some memory
    ; for the dos extenders private data

    test  si, si
    jz    SiZero
    mov   bx, si
    mov   ah, 48h
    int   21h
    jc    Failure
    mov   es, ax
SiZero:
    xor   ax,ax

    call  [DPMIControl]

    jc    Failure
    les   di, pDPMIMemInfo
    mov   ax, 0500h
    int   31h
    jc    Failure
    mov   bx, 0ffffh
    mov   ax, 0100h
    int   31h
    mov   wRMMemAvail, bx
}

  pMem_Info->ulRealModeDPMIMemAvail = wRMMemAvail * 16;

Failure:

  return (0);
}

/************************************************************************
*
* Function Get_Xmm_Info()
*
* This function first tests for the existence of an XMS driver.  If an
* XMS driver is present, it returns the version of the XMS driver, the
* version of the XMS specification being used, whether or not the high
* memory area (HMA) is present, and whether or not the HMA is available.
* It also returns the size of the largest free extended memory block in
* bytes, and the total amount of free extended memory in bytes.
*
*
* Mem_Info_St Fields Set
* ----------------------
*
* iXmm_Is_There            = 1 if an XMS driver is present, 0 if not
* iXmm_Spec_VersionMajor   = The major version of the XMS specification
* iXmm_Spec_VersionMinor   = The minor version of the XMS specification
* iXmm_Driver_VersionMajor = The internal major version of the XMM driver
* iXmm_Driver_VersionMinor = The internal minor version of the XMM driver
* lLargest_Free_Xm         = The size in bytes of the largest free block of
*                            extended memory. Includes the 64K HMA if it is
*                            available.
* lTotal_Free_XM           = The size in bytes of the total amount of free
*                            extended memory. Includes the 64K HMA if it is
*                            available.
*
* Local Variables Used
* --------------------
*
* XMSControl       : The address of the XMS driver control function
* XMSIsThere       : Used to flag whether or not an XMS driver is
*                    present
* XMSSpecVersion   : The BCD version of the XMS spec being used
* XMSDriverVersion : The BCD version of the XMS driver being used
* LargestFreeBlock : Used to calculate the largest free block
* TotalFreeBlocks  : Used to calculate the total amount free
* HMAStatus        : Used to check the availability of the HMA
* XMSError         : Used to hold error codes returned from the XMS
*                    calls
* A20Status        : Used to hold the status of the A20 address line
* True, False      : Used to set value of iXmm_Is_There
*
************************************************************************/

int Get_Xmm_Info(MEMORY_STRUCT FAR * pMem_Info, WORD wProcessorType)

{
  long XMSControl;
  unsigned char XMSIsThere;
  unsigned char XMSSpecVersionMajor, XMSSpecVersionMinor;
  unsigned char XMSDriverVersionMajor, XMSDriverVersionMinor;
  unsigned int  LargestFreeBlock, TotalFreeBlocks;
  unsigned int  HMAStatus;
  unsigned char XMSError;
  unsigned int  A20Status;
  int True = 1;
  int False = 0;
  int Available = 1;
  int NotAvailable = 0;
  DWORD SxmsLargestFree;  /* Largest free SXMS block */
  DWORD SxmsTotalFree;    /* Total free SXMS         */


  /* Determine if an XMS driver is there */
  
  _asm

  {
    /* After setting AX=43h, AL=00h and calling INT 2Fh, if an
       XMS driver is present, AL will be set to 80h */

    mov   ax,4300h
    int   2Fh
    mov   XMSIsThere,al
  }


  if ( XMSIsThere == 0x80 ) /* There is an XMS driver present */
  {
    pMem_Info->iXmm_Is_There = True;
    
    _asm

    {
      /* Get the address of the XMS driver's control function.
         Once we have that we can make calls to it to get the
         other info we want */

      mov   ax,4310h
      int   2Fh
      mov   word ptr [XMSControl],bx
      mov   word ptr [XMSControl+2],es

      /* Get the XMS spec and driver version numbers, in BCD */

      mov   ah,00h
      call  [XMSControl]

      mov   XMSSpecVersionMajor,ah
      mov   XMSSpecVersionMinor,al

      mov   XMSDriverVersionMajor,bh
      mov   XMSDriverVersionMinor,bl
    }


    /* Get the XMS specification version */

    pMem_Info->uchXmm_Spec_VersionMajor = XMSSpecVersionMajor;
    pMem_Info->uchXmm_Spec_VersionMinor = XMSSpecVersionMinor;


    /* Get the XMS driver version */

    pMem_Info->uchXmm_Driver_VersionMajor = XMSDriverVersionMajor;
    pMem_Info->uchXmm_Driver_VersionMinor = XMSDriverVersionMinor;


    /* Query the amount of free extended memory */
    
    _asm

    {
      mov   ah,08h
      call  [XMSControl]
      mov   LargestFreeBlock,ax
      mov   TotalFreeBlocks,dx
      mov   XMSError,bl
    }

    /* Store the largest free block and the total free blocks */

    pMem_Info->iLargest_Free_Xm = LargestFreeBlock;
    pMem_Info->iTotal_Free_Xm = TotalFreeBlocks;


    /* Check to see if the HMA is available by issuing a request for it */
    
    _asm

    {
      /* Return the status of the request in HMAStatus. 0x0001 means
         the request succeeded, 0x0000 means it failed */

      mov   ah,01h
      call  [XMSControl]
      mov   HMAStatus,ax
      mov   XMSError,bl
    }

    if (HMAStatus) /* The HMA request succeeded */
    {
      pMem_Info->iHMAStatus = Available;


      /* Release the HMA */

      _asm

      {
        /* Return the status of the release in HMAStatus. 0x0001 means
           the HMA was released successfully, 0x0000 means otherwise */

        mov   ah,02h
        call  [XMSControl]
        mov   HMAStatus,ax
        mov   XMSError,bl
      }

      if (!HMAStatus) /* The HMA was not successfully released */
        pMem_Info->iXMSError = 6; /* There was an error releasing the HMA */
    }
    else /* the HMA request failed */
    {
      pMem_Info->iHMAStatus = NotAvailable;

      switch (XMSError) /* Get the reason why the HMA request failed */
      {
        case 0x80: pMem_Info->iXMSError = 1; /* Feature not implemented */
                   break;
        case 0x81: pMem_Info->iXMSError = 2; /* VDISK was detected */
                   break;
        case 0x90: pMem_Info->iXMSError = 3; /* HMA does not exist */
                   break;
        case 0x91: pMem_Info->iXMSError = 4; /* HMA is already in use */
                   break;
          default: pMem_Info->iXMSError = 5;   /* Unknown reason */
      }
    }


    /* Query the status of the A20 address line */
    
    _asm

    {
      mov   ah,07h
      call  [XMSControl]
      mov   A20Status,ax
      mov   XMSError,bl
    }

    /* Store the A20 line status */

    pMem_Info->iA20Status = A20Status;


    /* Detect the XMS UMB free areas */

#define XMS_UMB_ERROR       2
#define MAX_XMS_UMB_SEGS  100

    {
      BYTE  bErrorCode;         /* Error code from XMS UMB call         */
      WORD  wSegmentAddr;       /* Segment address of free XMS UMB      */
      WORD  wSegmentSize;       /* Size of XMS UMB                      */

      WORD wTotalFree = 0;      /* Total amount of free XMS UMB space   */
      WORD wLargestFree = 0;    /* Largest free area                    */
                                /* Segment addresses of XMS UMBs        */
      WORD  wSegments[MAX_XMS_UMB_SEGS + 1];
      WORD  i;                  /* Looping variable                     */
      BOOL  fDoneFlag = FALSE;  /* TRUE when all XMS UMBs accounted for */

      int iRow, iCol;             /* Row and Column of visual map overlay   */
      int iNumOfChars;            /* Number of characters (or 1K blocks)    */

      
      for (i = 0; i < MAX_XMS_UMB_SEGS && fDoneFlag == FALSE; ++i)
        {
          _asm
            {
              mov   ah, 0x10            ; Request XMS UMB
              mov   dx, 0xFFFF          ; Requested size in paragraphs

              call  [XMSControl]        ; Make the call

              or    ax,ax               ; AX == 0x0000 if it failed

              jz    FirstErrResults     ; Jump around if it failed

              mov   fDoneFlag, XMS_UMB_ERROR ; There is no way this could
                                        ;   have succeeded.  If it did,
                                        ;   bomb out immediately.

              jmp   FirstXmsUmbDone


            FirstErrResults:
              mov   bErrorCode, bl      ; Store the error code
              mov   wSegmentSize, dx    ; Store size of largest free block

            FirstXmsUmbDone:
            }

          /* Bomb out if 0xFFFF succeeded */
          if (fDoneFlag == XMS_UMB_ERROR)
            break;

          /* Are there no more segments free */
          if (bErrorCode == 0xB1)
            fDoneFlag == TRUE;

          /* If the function was not implemented, bomb out */
          if (bErrorCode == 0x80)
            {
              pMem_Info->fXmsUmbAvailable = FALSE;
              fDoneFlag = XMS_UMB_ERROR;
              break;
            }

          /* If an unexpected error occured, bomb out */
          if (bErrorCode != 0xB0)
            {
              fDoneFlag = XMS_UMB_ERROR;
              break;
            }

          /* The XMS UMB calls are available */
          pMem_Info->fXmsUmbAvailable = TRUE;


          /* One segment of wSegmentSize size is free -- allocate it */
          _asm
            {
              mov   ah, 0x10            ; Request XMS UMB
              mov   dx, wSegmentSize    ; Requested size in paragraphs

              call  [XMSControl]        ; Make the call

              or    ax,ax               ; AX == 0x0000 if it failed

              jz    SecondErrResults    ; Jump around if it failed

              mov   wSegmentAddr, bx    ; Store XMS UMB segment address
              mov   wSegmentSize, dx    ; Store size of block

              jmp   SecondXmsUmbDone


            SecondErrResults:
              mov   fDoneFlag, XMS_UMB_ERROR ; This should not have failed.
                                        ;   If it did, bomb out
                                        ;   immediately.
            SecondXmsUmbDone:
            }

          /* Bomb out if 0xFFFF succeeded */
          if (fDoneFlag == XMS_UMB_ERROR)
            break;

          /* Accumulate the results */
          wSegments[i] = wSegmentAddr;

          if (wSegmentSize > wLargestFree)
            wLargestFree = wSegmentSize;

          wTotalFree += wSegmentSize;

          /* Fill the memory map with "X"es */
          iRow        = wSegmentAddr / 0x400;
          iCol        = (wSegmentAddr % 0x400) / 0x40;
          iNumOfChars = (WORD) ((((DWORD) wSegmentSize << 4) + 1023) / 1024);
          FillMemMapOverlay (pMem_Info, iRow, iCol, iNumOfChars,
                             REPORT_FREE_XMS_UMB);
        }

      wSegments[i] = 0x0000;


      /* Store the results in the XMS UMB area of pMem_Info */

      pMem_Info->dwXmsUmbFree = (DWORD) wTotalFree << 4;
      pMem_Info->dwXmsUmbLargestFree = (DWORD) wLargestFree << 4;


      /* The amounts have been determined -- free the blocks */
      for (i = 0; i < MAX_XMS_UMB_SEGS && wSegments[i] != 0x0000; ++i)
        {
          wSegmentAddr = wSegments[i];

          _asm
            {
              mov   ah, 0x11            ; Release XMS UMB
              mov   dx, wSegmentAddr    ; paragraph to release

              call  [XMSControl]        ; Make the call

              or    ax,ax               ; AX == 0x0000 if it failed

              jnz   ThirdXmsUmbDone     ; Jump around if it failed

              mov   fDoneFlag, XMS_UMB_ERROR ; This should not have failed.
                                        ;   If it did, bomb out
                                        ;   immediately.
            ThirdXmsUmbDone:
            }

          if (fDoneFlag == XMS_UMB_ERROR)
            break;
        }
    }


    /* Get the Super Extended Memory values */
    {
      if (wProcessorType > _80286 &&
          GetSuperXmsInfo (XMSControl, &SxmsLargestFree, &SxmsTotalFree) == 0)
        {
          /* Super XMS exists */
          pMem_Info->fSxmsAvailable    = TRUE;
          pMem_Info->dwSxmsLargestFree = SxmsLargestFree;
          pMem_Info->dwSxmsTotalFree   = SxmsTotalFree;
        }
      else
        {
          /* There is no Super XMS */
          pMem_Info->fSxmsAvailable = FALSE;
        }
    }
  }
  else /* There is no XMS driver present */
    pMem_Info->iXmm_Is_There = False;

  return (0);
}

/************************************************************************
 * Function GetRomMap - Fills a ROM_MAP structure with the information
 *                      about ROM BIOSes and Option ROMs in the system.
 *
 * pRomMap - Pointer to the structure for storing the ROM map.
 ***********************************************************************/

VOID GetRomMap (ROM_MAP *pRomMap, WORD wBusType)
{
  int  iMapSegment, iMapOffset;
  WORD wOptIndex = 0;   /* Option ROM index */

  unsigned uSegment;
  unsigned uOffset;
  int iValidRom;
  int iFalse = 0;
  int iTrue = 1;


  unsigned END_SEGMENT = 0xEC00;

  /* Check if we are on a ps/2 or not. If this is a ps/2, the
     range E000-EFFF is system ROM so we only need to check up
     through the DC00 page.  If this is not a ps/2, the range
     F000-EFFF is ROM so we only need to check up through page EC00 */


  if (wBusType == 3)         /* Is this a PS/2? (MCA) */
    {
      END_SEGMENT = 0xDC00;
      pRomMap->wRomBiosLoc[0]   = 0xE000;
      pRomMap->dwRomBiosSize[0] = 0x10000;
      pRomMap->wRomBiosLoc[1]   = 0xF000;
      pRomMap->dwRomBiosSize[1] = 0x10000;
    }
  else
    {
      pRomMap->wRomBiosLoc[0]   = 0xF000;
      pRomMap->dwRomBiosSize[0] = 0x10000;
      pRomMap->wRomBiosLoc[1]   = 0;
      pRomMap->dwRomBiosSize[1] = 0;
    }


  /* Set the indexes for the memory search to point to just past
     the conventional memory limit, first column */

  /* Begin stepping through memory, forcing uSegment to start on
     a true "visual memory map" line number */
  uSegment = 0xA000;

  /* Make iMapSegment start on the appropriate line */
  iMapSegment = uSegment / 1024;
  iMapOffset  = 0;

  iValidRom   = iFalse;

  for (; uSegment <= END_SEGMENT; uSegment += SEGMENT_INC)
    {
      for (uOffset = START_OFFSET; uOffset < ENDING_OFFSET;
           uOffset += OFFSET_INC)
        {
          /* Check the value of iValidRom. If it is non-zero, we are in  */
          /*   an Option ROM and iValidRom is the number of 1K blocks    */
          /*   that we still have to mark as being ROM. Mark the current */
          /*   1K block as being ROM and then decrement iValidRom by 1.  */
          /*   If iValidRom is zero, we are not currently inside an      */
          /*   Option ROM area.                                          */

          if (iValidRom)
            {
              iValidRom--;
            }
          else /* Not currently in an Option ROM */
            {
              /* Attempt to determine what is at address uSegment:uOffset.
                 First check for an Option ROM with a valid signature */

              iValidRom = OptionRomCheck (uSegment, uOffset);

              if (iValidRom)
                {
                  /* An Option ROM with a signature was detected with a
                     length of iValidRom */

                  if (wOptIndex < 10)
                    {
                      pRomMap->wOptRomLoc[wOptIndex]   = uSegment +
                                                         (uOffset >> 4);
                      pRomMap->dwOptRomSize[wOptIndex] = (DWORD) iValidRom *
                                                         1024L;
                      ++wOptIndex;
                    }

                  iValidRom--;
                }
            }

          /* Set the index for the memory map array to point to the
             next column, same row */

          iMapOffset++;
        }

      /* Set the indexes for the memory map array to point to the
         next row, first column of the array */

      iMapSegment++;
      iMapOffset = 0;
    }

  return;
}

/************************************************************************
*
* Function Get_VisualMemMap()
*
* This function generates a visual memory map of the address range
* 0000 to EFFF. It attempts to detect RAM, ROM and WOM (nothing).
*
* Mem_Info_St Fields Set
* ----------------------
*
* auchMemoryMap = The array holding the characters used to display the
*                 map.
*
*
* Local Variables Used
* --------------------
*
* iMapSegment, iMapOffset : Used to keep track of the location in the
*                         : array holding the memory map.
* iRow, iCol              : Used to point to a specific location in the
*                         : array holding the memory map.
* iMemLimit               : RAM and WOM extends up to the limit set
*                         : by this variable
* uSegment, uOffset       : The current segment:offset being looked at.
* uiPageFrameAddress      : The address of EMS page frame.
* Ram, Rom, Wom
* NotCertain              : Used when building the memory map.
* iFalse, iTrue           : Used for Boolean checking.
* iValidRom               : Used to figure out if we are currently in a
*                         : ROM area which had a valid signature byte.
* END_SEGMENT             : The final segment to check through.
*
************************************************************************/

int Get_VisualMemMap (MEMORY_STRUCT FAR * pMem_Info, WORD wBusType)

{
  int  iMapSegment, iMapOffset;
  int  iRow, iCol;
  int  iMemLimit;

  unsigned uSegment;
  unsigned uOffset;
  unsigned char Ram, Rom, Wom, NotCertain;
  int iValidRom;
  int iFalse = 0;
  int iTrue = 1;
  unsigned END_SEGMENT = 0xEC00;


  /* Check if we are on a ps/2 or not. If this is a ps/2, the
     range E000-EFFF is system ROM so we only need to check up
     through the DC00 page.  If this is not a ps/2, the range
     F000-EFFF is ROM so we only need to check up through page EC00 */


  if (wBusType == 3)         /* Is this a PS/2? (MCA) */
    END_SEGMENT = 0xDC00;


  /* Define the characters to be used to display the map. Check
     fpOutfile to determine if this map is going to the screen
     or to a printer */


  /* Going to the screen, use extended chars */
  if (fReportFlag == FALSE)
    {
      Ram = DISPLAY_RAM;
      Rom = DISPLAY_ROM;
      Wom = DISPLAY_WOM;
      NotCertain = DISPLAY_NOT_CERTAIN;
    }
  else /* Going to a report. Use standard ASCII */
    {
      Ram = REPORT_RAM;
      Rom = REPORT_ROM;
      Wom = REPORT_WOM;
      NotCertain = REPORT_NOT_CERTAIN;
    }

  /* Special case for black and white screen display */
  if (fBlackWhite == TRUE && fReportFlag == FALSE)
    {
      Rom = REPORT_ROM;
    }



  /* Initialize the map to RAM values in conventional RAM area  */
  /* to RAM, the area up to the ROM BIOS as empty, and the ROM  */
  /* BIOS as ROM.  Also, tack an ending null char in the last   */
  /* column of each row so that winWrtStrnAttrib can be used    */
  /* when moving the segment selection bar in the memory browse */
  /* stuff.                                                     */

  /* Fill up the "conventional RAM" in the visual memory map. */
  /*   RAM is assumed to be installed in 16k incriments.      */

  /* Set the maximum line number (ie, limit) to fill with RAM */
  iMemLimit = (int) (pMem_Info->lConv_Mem / (16384L));

  for (iRow = 0; iRow < iMemLimit; iRow++)
    {
      for (iCol = 0; iCol < (NUM_OF_COLS-1); iCol++)
        pMem_Info->abMemoryMap [iRow][iCol] = Ram;
      pMem_Info->abMemoryMap [iRow][iCol] = 0;
    }

  /* Fill from past "conventional RAM" to the ROM BIOS with WOM */

  iMemLimit = (int) (END_SEGMENT / 1024);

  for (; iRow <= iMemLimit; ++iRow)
    {
      for (iCol = 0; iCol < (NUM_OF_COLS-1); iCol++)
        pMem_Info->abMemoryMap [iRow][iCol] = Wom;
      pMem_Info->abMemoryMap [iRow][iCol] = 0;
    }

  /* Fill the ROM BIOS area with ROM */

  for (; iRow < NUM_OF_ROWS; ++iRow)
    {
      for (iCol = 0; iCol < (NUM_OF_COLS-1); iCol++)
        pMem_Info->abMemoryMap [iRow][iCol] = Rom;
      pMem_Info->abMemoryMap [iRow][iCol] = 0;
    }


  /* Set the indexes for the memory map array to point to just past
     the conventional memory limit, first column */

  /* Begin stepping through memory, forcing uSegment to start on
     a true "visual memory map" line number */
  uSegment = (unsigned) (pMem_Info->lConv_Mem / 16L);
  uSegment = uSegment & 0xF800;

  /* Make iMapSegment start on the appropriate line */
  iMapSegment = uSegment / 1024;
  iMapOffset  = 0;

  iValidRom   = iFalse;

  for (; uSegment <= END_SEGMENT; uSegment += SEGMENT_INC)
    {
      for (uOffset = START_OFFSET; uOffset < ENDING_OFFSET;
           uOffset += OFFSET_INC)
        {
          /* Check the value of iValidRom. If it is non-zero, we are in  */
          /*   an Option ROM and iValidRom is the number of 1K blocks    */
          /*   that we still have to mark as being ROM. Mark the current */
          /*   1K block as being ROM and then decrement iValidRom by 1.  */
          /*   If iValidRom is zero, we are not currently inside an      */
          /*   Option ROM area.                                          */

          if (iValidRom)
            {
              pMem_Info->abMemoryMap [iMapSegment][iMapOffset] = Rom;
              iValidRom--;
            }
          else /* Not currently in an Option ROM */
            {
              /* Attempt to determine what is at address uSegment:uOffset.
                 First check for an Option ROM with a valid signature */

              iValidRom = OptionRomCheck (uSegment, uOffset);

              if (iValidRom)
                {
                  /* An Option ROM with a signature was detected with a
                     length of iValidRom */

                  pMem_Info->abMemoryMap [iMapSegment][iMapOffset] = Rom;
                  iValidRom--;
                }
              else /* Try to determine what is there */
                {
                  switch (RamRomWomCheck (uSegment, uOffset, OFFSET_INC, fWindowsRunning))
                    {
                      case 0:
                        pMem_Info->abMemoryMap [iMapSegment][iMapOffset] = Ram;
                        break;

                      case 1:
                        pMem_Info->abMemoryMap [iMapSegment][iMapOffset] = NotCertain;
                        break;

                      case 2:
                        pMem_Info->abMemoryMap [iMapSegment][iMapOffset] = Wom;
                        break;

                      default:
                        pMem_Info->abMemoryMap [iMapSegment][iMapOffset] = NotCertain;
                    }
                }
            }

          /* Set the index for the memory map array to point to the
             next column, same row */

          iMapOffset++;
        }

      /* Set the indexes for the memory map array to point to the
         next row, first column of the array */

      iMapSegment++;
      iMapOffset = 0;
    }

  return (0);
}


/*********************************************************************
 * VisualMapOverlay - Fills the Visual Memory Map's Overlay map.  EMS
 *                    page frames are marked 'E', used UMBs are marked
 *                    'U', and free UMBs are marked 'F'.  Also counts
 *                    up the used and free UMBs.
 *
 * pMem - Pointer to the memory information structure.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL VisualMapOverlay (MEMORY_STRUCT FAR * pMem)
{
  WORD wPageFrameAddress;     /* Segment of EMS Page frame              */
  int iRow, iCol;             /* Row and Column of visual map overlay   */
  int iNumOfChars;            /* Number of characters (or 1K blocks)    */
                              /*   necessary to fill the page fram area */
  TSR_PROGRAMS_STRUCT *pTsr = NULL; /* TSR Program list                 */
  WORD wSize;                 /* Size required to store TSR list        */
  WORD i;                     /* Looping variable                       */


  /* Check to see if an EMS page frame was detected and if we want to */
  /*   display it.  If one was and we do want to show it, fill in the */
  /*   correct area in the memory map with characters to specify the  */
  /*   page frame                                                     */

  wPageFrameAddress = pMem->iPageFrameAddress;

  if (wPageFrameAddress)
    {
      iNumOfChars = 64;
      iRow = wPageFrameAddress / SEGMENT_INC;
      iCol = (wPageFrameAddress - (iRow * SEGMENT_INC)) / (OFFSET_INC >> 4);

      FillMemMapOverlay (pMem, iRow, iCol, iNumOfChars, DISPLAY_EMS);
    }


  /* Now, obtain the Memory Control Block information, to locate the  */
  /*   Upper Memory Blocks.                                           */

  if (wDosMajor >= 5 && wDosMajor < 10)
    {
      /* Obtain the TSR list */
      wSize = GetInfoSize (IDI_TSR_PROGRAMS_RECORD, FALSE);
      if ((pTsr = malloc (wSize)) == NULL)
        {
          OutOfMemory();
          return (TRUE);
        }

      /* Zero out the structure */
      memset (pTsr, '\0', wSize);

      if (GetInfo (IDI_TSR_PROGRAMS_RECORD, pTsr, FALSE, FALSE, FALSE))
        return (TRUE);


      /* Find the first upper memory block, if it exists */
      for (i = 1;
           pTsr[i].wAddress != 0 &&
           pTsr[i].wAddress + (pTsr[i].dwBlockSize >> 4) < 0xA000;
           ++i)
        ;


      /* Set the flag for UMBs available */
      if (pTsr[i].wAddress + (pTsr[i].dwBlockSize >> 4) >= 0xA000)
        pMem->fUmbsAvailable = TRUE;


      /* Begin walking the UMBs */
      for (; pTsr[i].wAddress != 0; ++i)
        {
          CHAR chFillChar;  /* Character to fill the visual map with */

          /* Skip the excluded UMB areas */
          if (strcmp (pTsr[i].szTsrName, pszExcludedUmbArea) == 0)
            continue;


          /* Free areas use 'F', used areas use 'U' */
          if (strcmp (pTsr[i].szTsrName, pszFreeMemory) == 0)
            {
              chFillChar = DISPLAY_FREE_UMB;

              /* Add up the UMB area, if it counts */
              if (pTsr[i].szTsrName[0] != ' ')
                {
                  pMem->dwTotalUmbs += pTsr[i].dwBlockSize;
                  pMem->dwFreeUmbs  += pTsr[i].dwBlockSize;

                  if (pTsr[i].dwBlockSize > pMem->dwLargestFreeUmb)
                    pMem->dwLargestFreeUmb = pTsr[i].dwBlockSize;
                }
            }
          else
            {
              chFillChar = DISPLAY_USED_UMB;

              /* Add up the UMB area, if it counts */
              if (pTsr[i].szTsrName[0] != ' ')
                pMem->dwTotalUmbs += pTsr[i].dwBlockSize;
            }


          /* Fill the region with the correct value */
          iRow        = pTsr[i].wAddress / 0x400;
          iCol        = (pTsr[i].wAddress % 0x400) / 0x40;
          iNumOfChars = (WORD) ((pTsr[i].dwBlockSize + 1023) / 1024);
          FillMemMapOverlay (pMem, iRow, iCol, iNumOfChars, chFillChar);
        }


      /* Free up the TSR structure */
      free (pTsr);
    }

}

/************************************************************************
*
* Function FillMemMap()
*
* This function fills in a specified portion of the memory map array with
* a specific value.
*
*
* Mem_Info_St Fields Set
* ----------------------
*
* abMemoryMap = The array of unsigned characters which holds the values
*               used to display the memory map.
*
*
* Local Variables Used
* --------------------
*
* index       : An integer used to keep track of how many fill chars
*               have been written to the memory map array.
* iRow, iCol  : Integers used to index into the memory map array.
* iNumOfChars : The number of fill characters to write to the map array.
* uchFillChar : The fill character to be written to the map array.
*
************************************************************************/

int FillMemMap (MEMORY_STRUCT FAR * pMem, int iRow, int iCol,
                   int iNumOfChars, unsigned char uchFillChar)

{
  int index = 0;

  while (index < iNumOfChars)
  {
    pMem->abMemoryMap [iRow][iCol] = uchFillChar;
    iCol++;

    if (iCol == NUM_OF_COLS - 1) /* Use (NUM_OF_COLS - 1) so we don't   */
    {                            /*   step on the null char used when   */
      iCol = 0;                  /*   displaying the map on the screen. */
      iRow++;
    }

    index++;
  }

  return (0);
}

/************************************************************************
*
* Function FillMemMapOverlay()
*
* This function fills in a specified portion of the memory map overlay
* array witha specific value.
*
*
* Mem_Info_St Fields Set
* ----------------------
*
* abMemMapOverlay = The array of unsigned characters which holds the
*                   values used to display the memory map.
*
*
* Local Variables Used
* --------------------
*
* index       : An integer used to keep track of how many fill chars
*               have been written to the memory map array.
* iRow, iCol  : Integers used to index into the memory map array.
* iNumOfChars : The number of fill characters to write to the map array.
* uchFillChar : The fill character to be written to the map array.
*
************************************************************************/

int FillMemMapOverlay (MEMORY_STRUCT FAR * pMem, int iRow, int iCol,
                       int iNumOfChars, unsigned char uchFillChar)

{
  int index = 0;

  while (index < iNumOfChars)
  {
    pMem->abMemMapOverlay [iRow][iCol] = uchFillChar;
    iCol++;

    if (iCol == NUM_OF_COLS - 1) /* Use (NUM_OF_COLS - 1) so we don't   */
    {                            /*   step on the null char used when   */
      iCol = 0;                  /*   displaying the map on the screen. */
      iRow++;
    }

    index++;
  }

  return (0);
}

/**************************************************************************
 * RamRomWomCheck - Checks if the memory pointed to by uSegment:uOffset
 *                  is RAM, ROM, or Available (WOM).
 *
 *                  RAM can change.
 *                  WOM is unchangable and has FFH's through the entire
 *                      range.
 *                  ROM is unchangable and is not FFH through the entire
 *                      range.
 *
 * uSegment - Segment to search
 *
 * uOffset  - Offset to search
 *
 * uLength  - Length of search to perform when checking for ROM/WOM
 *
 * fWindowsRunning - If windows is running, disable the RAM test.
 *
 * Returns RAM (0), ROM (1), or WOM (2).
 **************************************************************************/

int _fastcall RamRomWomCheck (unsigned uSegment,
                              unsigned uOffset,
                              unsigned uLength,
                              BOOL     fWindowsRunning)
{
  UCHAR uchHoldByte;            /* Used for storing byte during RAM check */
  int   iReturnValue;           /* Value this routine returns */

  _asm
    {
      push  es                  ;Save the ES register

      push  ax                  ;Put uSegment into ES
      pop   es
      mov   di,dx               ;Put uOffset into DI
      mov   cx,bx               ;Put uLength into CX

      mov   al,es:[di]          ;Store byte in holding area
      mov   [uchHoldByte],al

      cmp   fWindowsRunning, 0  ;Is it safe to perform the RAM test?
      jne   SkipRamTest         ;Skip out if not.

      xor   al,0xFF             ;Toggle all bits in AL

      cli                       ;Clear interrupts for RAM check

      mov   es:[di],al          ;Put new value back in memory

      mov   al,[uchHoldByte]    ;Put the original value back in AL
                                ;  There is logic behind checking the
                                ;  original value instead of the changed
                                ;  value.  Suppose the adapter card at
                                ;  this address does not support all 8
                                ;  bits.  Checking for the changed value
                                ;  would return false, even though seven
                                ;  bits were changed.  (One possibility
                                ;  for the missing bit or bits would be
                                ;  special purpose RAM).

      push  cx                  ;Wait for a moment, to allow memory to
      mov   cx,0x10             ;  stabilize

    Wait001:
      loop  Wait001
      pop   cx

      cmp   al,es:[di]          ;Check to see if it is the original value
      jne   ThisMayBeRAM        ;If not, this might be RAM

    ;---- Now check for the difference between ROM and WOM ----

    SkipRamTest:
      cmp   al,0xFF             ;Was the byte 0FFH?
      jne   ThisIsROM           ;If not, this is definitely ROM

    ;---- This could be WOM.  Check for a bunch of FFHs ----

      repe  scasb               ;Search for the first non 0FFH

      cmp   cx,0                ;Did we get to the end without a non-0FFH?
      jne   ThisIsROM           ;If not, this is ROM

    ;---- WOM was detected, set the return value ----

      mov   iReturnValue,WOM    ;Set the return value
      jmp   MemCheckComplete    ;Exit this routine

    ;---- RAM may have been detected ----

    ThisMayBeRAM:

      mov   bl,es:[di]          ;Now check to see if it is the changed value.
      xor   bl,0xFF             ;  The value may be bus noise or some other
                                ;  unusual case (WYSE 286 w/ 640k).  If it
                                ;  doesn't match the changed value, it is
                                ;  not RAM.

      cmp   al,bl               ;Does it match the changed value
      jne   ThisIsNotRAM        ;If not, this is something else

    ;---- RAM really was detected ----

      mov   al,uchHoldByte      ;Restore the original byte
      mov   es:[di],al

      mov   iReturnValue,RAM    ;Set the return value
      jmp   MemCheckComplete    ;Exit this routine

    ;---- ROM was detected, set the return value ----

    ThisIsROM:

      mov   iReturnValue,ROM    ;Set the return value
      jmp   MemCheckComplete    ;Exit this routine

    ;---- It's not RAM, it's not ROM, it's not WOM, it's something else ----

    ThisIsNotRAM:

      mov   iReturnValue,ROM    ;We'll call it available for now
      jmp   MemCheckComplete    ;Exit this routine

    ;---- Test is completed, return to calling routine ----

    MemCheckComplete:

      sti                       ;Restore interrupts
      pop   es                  ;Restore the original ES register
    }

  return (iReturnValue);
}

/**************************************************************************
 * OptionRomCheck - Checks for the presence of installed Option ROM cards
 *                  (ie, EGA/VGA, Net cards with ROMs, etc).
 *
 *                  Option ROM cards are installed in the range of C000
 *                    (A000?) - EFFF for non-PS/2 computers.  PS/2s use
 *                    E000-FFFF, so option cards can only be installed
 *                    between C000 - DFFF.
 *
 *                  According to the limited documentation we've found,
 *                    (Specifically a Windows 3.00 assembly language
 *                    include file), all Option ROMs in the valid range
 *                    start with the bytes AAH, 55H, followed by a byte
 *                    that tells the length of the ROM card / 512:
 *
 *                    +----------------------+
 *                    |         AAH          |
 *                    +----------------------+
 *                    |         55H          |
 *                    +----------------------+
 *                    | Byte: ROM Size / 512 |
 *                    +----------------------+
 *                    |/ / / / / / / / / / / |
 *                    | /-Option ROM code-/  |
 *                    |/ / / / / / / / / / / |
 *                    +----------------------+
 *                    (Sum of all bytes MOD 100H is 00H).
 *
 *                  I add up all the bytes starting from th AAH into BL,
 *                    allowing BL to overflow (an 8 bit checksum sort
 *                    of arrangement).  If the value is 00H, this was
 *                    a valid option card.  If not, I stumbled across
 *                    a spurious 55 AA in memory.
 *
 * uSegment - Segment to search.
 *
 * uOffset  - Offset to search.
 *
 * Returns: Number of 1K pages for the Option ROM starting at the
 *          specified location, or 0 if no Option ROM was found.
 **************************************************************************/

int _fastcall OptionRomCheck (unsigned uSegment, unsigned uOffset)
{
  int   iReturnValue;           /* Value this routine returns */

  _asm
    {
                                ;uSegment is in AX
                                ;uOffset  is in DX

      push  es                  ;Save the ES register

    ;---- Adjust Segment so Offset can be :0000 ----

      mov   cl,4                ;Shift uOffset 4 bits
      shr   dx,cl
      add   ax,dx               ;Add to uSegment

      push  ax                  ;Put uSegment into ES
      pop   es
      xor   di,di               ;Put zero into DI

    ;---- Check for ROM signature ----

      cmp   es:[di],0xAA55      ;Is the ROM signature present?
      jne   NotOptionRom        ;If not, jump out.

    ;---- ROM signature detected, Set return value in case it's valid ----

      xor   cx,cx               ;Zero out CX register
      mov   cl,es:[di + 2]      ;Load the number of bytes / 512
      test  cl,0x01             ;Is it an odd value?
      jz    SetReturnValue      ;If not, set return value
      inc   cx                  ;If odd, increase CX to even 1K boundry

    SetReturnValue:
      shr   cx,1                ;Divide by 2 to obtain Option ROM size in K
      mov   iReturnValue,cx     ;Set the return value, in case this is
                                ;  a value Option ROM.

    ;---- Prepare for checksum on Option ROM ----

      xor   cx,cx               ;Zero out CX register

;     inc   di                  ;Bump DI three bytes to move around the
;     inc   di                  ;  ROM signature
;                               ;Load the number of bytes in the Option ROM
;                               ;  into CX

      mov   ch,es:[di + 2]      ;CL = byte count / 512 (CX = byte count / 2)
      shl   cx,1                ;CX now = number of bytes

      xor   al,al               ;Zero out the checksum register

    ;---- Perform checksum ----

    CkSumLoop:

      add   al,es:[di]          ;Add byte to AL (overflow is expected)
      inc   di                  ;Bump DI to point to next byte
      loop  CkSumLoop           ;Loop until all bytes have been read

    ;---- Checksum is complete, AL = 0 means this is a valid Option ROM ----

      cmp   al,0                ;Is the checksum zero?
      je    OptionCheckComplete ;If so, the return value has already been
                                ;  set, so we jump out of the routine

                                ;If not, fall through...

    ;---- ROM Signature not Detected ----

    NotOptionRom:

      mov   iReturnValue,0      ;Set the return value to zero.
      jmp   OptionCheckComplete ;Exit this routine

    ;---- Test is completed, return to calling routine ----

    OptionCheckComplete:

      pop   es                  ;Restore the original ES register
    }

  return (iReturnValue);
}


/*********************************************************************
 * SprintMemInfo - Put Memory information into a set of strings to be
 *                 printed or displayed.
 *
 * fMemoryType  - Type(s) of memory to put into strings (MEM_ALL to
 *                show all types of memory).
 * pMem         - Pointer to the structure describing the memory
 *                types.
 * szSumStrings - Summary information string holder.
 *
 * Returns:  NULL if an error occured, or if summary information is
 *           requested.
 *********************************************************************/

QSZ * SprintMemInfo (BOOL fMemoryType,
                     MEMORY_STRUCT *pMem,
                     CHAR szSumStrings[][MAX_SUMM_INFO + 5],
                     BOOL fOverlayFlag)
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD i;                   /* Index variable                        */
  CHAR chBuffer[120];       /* Local string                          */
  QSZ  *pqszStrings;        /* Location for storing string pointers  */
  WORD wAlignColumn;        /* Column to align titles                */


  /* Summary strings */
  if (szSumStrings != NULL)
    {
      i = 0;

      /* Conventional */
      sprintf (chBuffer, "%ldK", pMem->lConv_Mem / 1024L);
      strncpy (szSumStrings[i], chBuffer, MAX_SUMM_INFO + 3);

      /* Extended */
      if (pMem->iCMOSExtended)
        {
          strncat (szSumStrings[i], pszCommaSpace,
                   MAX_SUMM_INFO + 3 - strlen (szSumStrings[i]));

	  sprintf (chBuffer, "%uK ", (unsigned int)pMem->iCMOSExtended);
          strcat (chBuffer, "Ext");

          if (strlen (chBuffer) + strlen (szSumStrings[i]) > MAX_SUMM_INFO + 3)
            {
              i = 1;
              szSumStrings[i][0] = '\0';
            }

          strncat (szSumStrings[i], chBuffer,
                   MAX_SUMM_INFO + 3 - strlen (szSumStrings[i]));
        }

      /* EMS */
      if (pMem->iTotal_Emm_Pages)
        {
          strncat (szSumStrings[i], pszCommaSpace,
                   MAX_SUMM_INFO + 3 - strlen (szSumStrings[i]));

	  sprintf (chBuffer, "%uK ", (unsigned int)pMem->iTotal_Emm_Pages * 16);
          strcat (chBuffer, "EMS");

          if (strlen (chBuffer) + strlen (szSumStrings[i]) > MAX_SUMM_INFO + 3)
            {
              i = 1;
              szSumStrings[i][0] = '\0';
            }

          strncat (szSumStrings[i], chBuffer,
                   MAX_SUMM_INFO + 3 - strlen (szSumStrings[i]));
        }

      /* XMS */
      if (pMem->iXmm_Free_Err == 0 && pMem->iTotal_Free_Xm != 0)
        {
          strncat (szSumStrings[i], pszCommaSpace,
                   MAX_SUMM_INFO + 3 - strlen (szSumStrings[i]));

	  sprintf (chBuffer, "%uK ", (unsigned int)pMem->iTotal_Free_Xm);
          strcat (chBuffer, "XMS");

          if (strlen (chBuffer) + strlen (szSumStrings[i]) > MAX_SUMM_INFO + 3)
            {
              i = 1;
              szSumStrings[i][0] = '\0';
            }

          strncat (szSumStrings[i], chBuffer,
                   MAX_SUMM_INFO + 3 - strlen (szSumStrings[i]));
        }

      return (NULL);
    }


  /* Overestimate the amount of space required for the strings */

  switch (fMemoryType)
    {
      case MEM_ALL:
        wAlignColumn = MEM_MAX_ALIGN;
        wNmbrStrings = MEM_CONV_STRINGS + MEM_EXT_STRINGS  +
                       MEM_EMS_STRINGS  + MEM_XMS_STRINGS  +
                       MEM_VCPI_STRINGS + MEM_DPMI_STRINGS +
                       MEM_LEGEND_STRINGS;
        if (wNmbrStrings < MEM_640K_LIMIT)
          wNmbrStrings = MEM_640K_LIMIT;
        wNmbrChars   = 2450;  /* Beyond the maximum chars required */
        break;

      case MEM_SUMMARY:
        wAlignColumn =  0;
        wNmbrStrings =  1;
        wNmbrChars   = REPORT_WIDTH + 1;
        break;

      case MEM_CONVENTIONAL:
        wAlignColumn = MEM_CONV_ALIGN;
        wNmbrStrings = MEM_CONV_STRINGS;
        wNmbrChars   = MEM_CONV_STRINGS * REPORT_WIDTH + 1;
        break;

      case MEM_EXTENDED:
        wAlignColumn = MEM_EXT_ALIGN;
        wNmbrStrings = MEM_EXT_STRINGS;
        wNmbrChars   = MEM_EXT_STRINGS * REPORT_WIDTH + 1;
        break;

      case MEM_EMS:
        wAlignColumn = MEM_EMS_ALIGN;
        wNmbrStrings = MEM_EMS_STRINGS;
        wNmbrChars   = MEM_EMS_STRINGS * REPORT_WIDTH + 1;
        break;

      case MEM_XMS:
        wAlignColumn = MEM_XMS_ALIGN;
        wNmbrStrings = MEM_XMS_STRINGS;
        wNmbrChars   = MEM_XMS_STRINGS * REPORT_WIDTH + 1;
        break;

      case MEM_VCPI:
        wAlignColumn = MEM_VCPI_ALIGN;
        wNmbrStrings = MEM_VCPI_STRINGS;
        wNmbrChars   = MEM_VCPI_STRINGS * REPORT_WIDTH + 1;
        break;

      case MEM_DPMI:
        wAlignColumn = MEM_DPMI_ALIGN;
        wNmbrStrings = MEM_DPMI_STRINGS;
        wNmbrChars   = MEM_DPMI_STRINGS * REPORT_WIDTH + 1;
        break;

      case MEM_VISUAL_MAP:
        wAlignColumn = 12;
        wNmbrStrings = 65;
        wNmbrChars   = wNmbrStrings * 40;
        break;


      default:
        wAlignColumn =  0;
        wNmbrStrings =  1;
        wNmbrChars   = REPORT_WIDTH + 1;
    }


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the information in place */

  i = 0;

  /* Summary Information */

  if (fMemoryType == MEM_SUMMARY)
    {
      return (NULL);
    }


  /* Include the memory legend */

  if (fReportFlag)
    {
      sprintf (chBuffer, "Legend:  Available \"%c%c\"  RAM \"%c%c\"  ROM \"%c%c\"  Possibly Available \"%c%c\"",
               REPORT_WOM, REPORT_WOM,
               REPORT_RAM, REPORT_RAM,
               REPORT_ROM, REPORT_ROM,
               REPORT_NOT_CERTAIN, REPORT_NOT_CERTAIN);
      Qstrcpy (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);

      sprintf (chBuffer, "  EMS Page Frame \"%c%c\"",
               REPORT_EMS, REPORT_EMS);
      Qstrcpy (pqszStrings[i], chBuffer);

      if (wDosMajor >= 5 && wDosMajor < 10)
        {
          sprintf (chBuffer, "  Used UMBs \"%c%c\"  Free UMBs \"%c%c\"",
                   REPORT_USED_UMB, REPORT_USED_UMB,
                   REPORT_FREE_UMB, REPORT_FREE_UMB);
          Qstrcat (pqszStrings[i], chBuffer);
        }

      if (pMem->fXmsUmbAvailable)
        {
          sprintf (chBuffer, "  Free XMS UMBs \"%c%c\"",
                   REPORT_FREE_XMS_UMB, REPORT_FREE_XMS_UMB);
          Qstrcat (pqszStrings[i], chBuffer);
        }

      PrepNextString (pqszStrings, i++);
    }
  else
    {
      sprintf (chBuffer, "Legend:  Available \"&1%c%c&0\"  RAM \"&1%c%c&0\"  ROM \"&1%c%c&0\"  Possibly Available \"&1%c%c&0\"",
               DISPLAY_WOM, DISPLAY_WOM,
               DISPLAY_RAM, DISPLAY_RAM,
               (fBlackWhite) ? REPORT_ROM : DISPLAY_ROM,
               (fBlackWhite) ? REPORT_ROM : DISPLAY_ROM,
               DISPLAY_NOT_CERTAIN, DISPLAY_NOT_CERTAIN);
      Qstrcpy (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);

      sprintf (chBuffer, "  EMS Page Frame \"&1%c%c&0\"",
               DISPLAY_EMS, DISPLAY_EMS);
      Qstrcpy (pqszStrings[i], chBuffer);

      if (wDosMajor >= 5 && wDosMajor < 10)
        {
          sprintf (chBuffer, "  Used UMBs \"&1%c%c&0\"  Free UMBs \"&1%c%c&0\"",
                   DISPLAY_USED_UMB, DISPLAY_USED_UMB,
                   DISPLAY_FREE_UMB, DISPLAY_FREE_UMB);
          Qstrcat (pqszStrings[i], chBuffer);
        }

      if (pMem->fXmsUmbAvailable)
        {
          sprintf (chBuffer, "  Free XMS UMBs \"&1%c%c&0\"",
                   DISPLAY_FREE_XMS_UMB, DISPLAY_FREE_XMS_UMB);
          Qstrcat (pqszStrings[i], chBuffer);
        }

      PrepNextString (pqszStrings, i++);
    }


  /* Conventional memory */

  if (fMemoryType == MEM_CONVENTIONAL || fMemoryType == MEM_ALL)
    {
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Conventional Titles */
      Qstrcat (pqszStrings[i], paszMemoryTitles[MT_CONV_TITLE]);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Total Conventional Memory */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_CONV_TOTAL],
                   wAlignColumn);
      sprintf (chBuffer, "%ldK", pMem->lConv_Mem / 1024L);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Available Conventional Memory, Line 1 */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_CONV_AVAIL],
                   wAlignColumn);
      sprintf (chBuffer, "%ldK", pMem->lFree_Conv_Mem / 1024L);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Available Conventional Memory, Line 2 */
      QstrcatAlign (pqszStrings[i], pszNull, wAlignColumn);
      sprintf (chBuffer, "%ld bytes", pMem->lFree_Conv_Mem);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Separator when sprinting all types of memory */
      if (fMemoryType == MEM_ALL)
        {
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
        }
    }


  /* Extended memory */

  if (fMemoryType == MEM_EXTENDED || fMemoryType == MEM_ALL)
    {
      /* Extended Titles */
      Qstrcat (pqszStrings[i], paszMemoryTitles[MT_EXT_TITLE]);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Total Extended Memory */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_CONV_TOTAL],
                   wAlignColumn);
      sprintf (chBuffer, "%uK", (unsigned int)pMem->iCMOSExtended);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Separator when sprinting all types of memory */
      if (fMemoryType == MEM_ALL)
        {
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
        }
    }


  /* UMB Information */

  if (pMem->fUmbsAvailable &&
      (fMemoryType == MEM_CONVENTIONAL || fMemoryType == MEM_ALL))
    {
      /* UMB Titles */
      Qstrcat (pqszStrings[i], paszMemoryTitles[MT_UMB_TITLE]);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Total UMBs */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_UMB_TOTAL],
                   wAlignColumn);
      sprintf (chBuffer, "%luK", pMem->dwTotalUmbs / 1024L);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Total Free UMBs */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_UMB_TOTAL_FREE],
                   wAlignColumn);
      sprintf (chBuffer, "%luK", pMem->dwFreeUmbs / 1024L);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Largest Free UMBs */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_UMB_LARGEST_FREE],
                   wAlignColumn);
      sprintf (chBuffer, "%luK", pMem->dwLargestFreeUmb / 1024L);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Separator when sprinting all types of memory */
      if (fMemoryType == MEM_ALL)
        {
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
        }
    }


  /* EMS Information */

  if (fMemoryType  == MEM_EMS ||
      (fMemoryType == MEM_ALL && pMem->iEmm_Is_There))
    {
      /* EMS Titles */
      Qstrcat (pqszStrings[i], paszMemoryTitles[MT_EMS_TITLE]);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* EMS Version */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_EMS_VERSION],
                   wAlignColumn);
      sprintf (chBuffer, "%d.%02d",
               pMem->iEmm_VersionMajor,
               pMem->iEmm_VersionMinor);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* EMS Page Frame */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_EMS_PAGE_FRAME],
                   wAlignColumn);
      if (pMem->iPageFrameAddress)
        {
          sprintf (chBuffer, "%04XH", pMem->iPageFrameAddress);
          Qstrcat (pqszStrings[i], chBuffer);
        }
      else
        Qstrcat (pqszStrings[i], pszNoPageFrame);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Total EMS */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_EMS_TOTAL],
                   wAlignColumn);
      sprintf (chBuffer, "%uK", (unsigned int)pMem->iTotal_Emm_Pages * 16);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Available EMS */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_EMS_AVAIL],
                   wAlignColumn);
      sprintf (chBuffer, "%dK", pMem->iFree_Emm_Pages * 16);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Separator when sprinting all types of memory */
      if (fMemoryType == MEM_ALL)
        {
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
        }
    }


  /* XMS Information */

  if (fMemoryType  == MEM_XMS ||
      (fMemoryType == MEM_ALL && pMem->iXmm_Is_There))
    {
      /* XMS Titles */
      Qstrcat (pqszStrings[i], paszMemoryTitles[MT_XMS_TITLE]);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* XMS Version */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_XMS_VERSION],
                   wAlignColumn);
      sprintf (chBuffer, "%x.%02x",
               pMem->uchXmm_Spec_VersionMajor,
               pMem->uchXmm_Spec_VersionMinor);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* XMS Driver Version */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_XMS_DRIVER_VER],
                   wAlignColumn);
      sprintf (chBuffer, "%x.%02x",
               pMem->uchXmm_Driver_VersionMajor,
               pMem->uchXmm_Driver_VersionMinor);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* A20 Line */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_XMS_A20_LINE],
                   wAlignColumn);
      if (pMem->iA20Status)
        Qstrcat (pqszStrings[i], pszEnabled);
      else
        Qstrcat (pqszStrings[i], pszNotEnabled);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* High Memory Area */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_XMS_HMA],
                   wAlignColumn);
      if (pMem->iXmm_Is_There) /* There is an XMS driver present */
        {
          switch (pMem->iXMSError) /* Check the status of any error codes */
            {                             /* returned when querying the XMS driver */
              case 0:
              case 6: Qstrcat (pqszStrings[i], "Available");
                      break;              /* Case 6 also means there was a problem releasing the HMA */
              case 1: Qstrcat (pqszStrings[i], "Request failed");
                      break;
              case 2: Qstrcat (pqszStrings[i], "VDISK detected");
                      break;
              case 3: Qstrcat (pqszStrings[i], "Not present");
                      break;
              case 4: Qstrcat (pqszStrings[i], "In use");
                      break;
              default: ;
            }
        }
      else
        Qstrcat (pqszStrings[i], "Not available"); /* An XMS driver is not present */
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Available XMS */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_XMS_AVAIL],
                   wAlignColumn);
      if (pMem->iXmm_Free_Err)
        Qstrcat (pqszStrings[i], pszError);
      else
        {
	  sprintf(chBuffer, "%uK", (unsigned int)pMem->iTotal_Free_Xm);
          Qstrcat (pqszStrings[i], chBuffer);
        }
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Largest Available XMS */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_XMS_LARGEST_AVAIL],
                   wAlignColumn);
      if (pMem->iXmm_Free_Err == 0)
        {
	  sprintf(chBuffer, "%uK", (unsigned int)pMem->iLargest_Free_Xm);
          Qstrcat (pqszStrings[i], chBuffer);
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
        }
      else
        Qstrcat (pqszStrings[i], pszError);


      /* Super Extended Memory */
      if (pMem->fSxmsAvailable)
        {
          /* Available SXMS */
          QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_SXMS_AVAIL],
                       wAlignColumn);

          sprintf(chBuffer, "%luK", pMem->dwSxmsTotalFree);
          Qstrcat (pqszStrings[i], chBuffer);
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);


          /* Largest Free SXMS */
          QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_SXMS_LARGEST_AVAIL],
                       wAlignColumn);

          sprintf(chBuffer, "%luK", pMem->dwSxmsLargestFree);
          Qstrcat (pqszStrings[i], chBuffer);
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
        }


      if (pMem->fXmsUmbAvailable)
        {
          /* Largest Available XMS UMB */
          QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_XMS_TOTAL_UMB_AVAIL],
                       wAlignColumn);

          sprintf(chBuffer, "%luK", pMem->dwXmsUmbFree / 1024L);
          Qstrcat (pqszStrings[i], chBuffer);
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

          /* Largest Available XMS UMB */
          QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_XMS_LARGEST_UMB_AVAIL],
                       wAlignColumn);

          sprintf(chBuffer, "%luK", pMem->dwXmsUmbLargestFree / 1024L);
          Qstrcat (pqszStrings[i], chBuffer);
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
        }


      /* Separator when sprinting all types of memory */
      if (fMemoryType == MEM_ALL)
        {
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
        }
    }


  /* VCPI Information */

  if (fMemoryType  == MEM_VCPI ||
      (fMemoryType == MEM_ALL && pMem->iVCPIPresent))
    {
      /* VCPI Titles */
      Qstrcat (pqszStrings[i], paszMemoryTitles[MT_VCPI_TITLE]);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* VCPI Detected */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_VCPI_DETECTED],
                   wAlignColumn);
      if (pMem->iVCPIPresent)
        Qstrcat (pqszStrings[i], pszYes);
      else
        Qstrcat (pqszStrings[i], pszNo);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* VCPI Version */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_VCPI_VERSION],
                   wAlignColumn);
      sprintf (chBuffer, "%d.%02d", pMem->iVCPIMajorVersion,
               pMem->iVCPIMinorVersion);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* VCPI Available */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_VCPI_AVAIL],
                   wAlignColumn);
      sprintf (chBuffer, "%dK", pMem->iVCPIAvailMemory);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* Separator when sprinting all types of memory */
      if (fMemoryType == MEM_ALL)
        {
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
        }
    }


  /* DPMI Information */

  if (fMemoryType  == MEM_DPMI ||
      (fMemoryType == MEM_ALL && pMem->iDPMIPresent))
    {
      /* DPMI Titles */
      Qstrcat (pqszStrings[i], paszMemoryTitles[MT_DPMI_TITLE]);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* DPMI Detected */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_DPMI_DETECTED],
                   wAlignColumn);
      if (pMem->iDPMIPresent)
        Qstrcat (pqszStrings[i], pszYes);
      else
        Qstrcat (pqszStrings[i], pszNo);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);

      /* DPMI Version */
      QstrcatAlign (pqszStrings[i], paszMemoryTitles[MT_DPMI_VERSION],
                   wAlignColumn);
      sprintf (chBuffer, "%d.%02d", pMem->iDPMIMajorVersion,
               pMem->iDPMIMinorVersion);
      Qstrcat (pqszStrings[i], chBuffer);
      PrepNextString (pqszStrings, i++);
      CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
    }


  /* Add the lines for the visual memory map down to the A000 region */
  if (i - 2 < MEM_640K_LIMIT)
    {
      for (; i - 2 < MEM_640K_LIMIT;)
        {
          PrepNextString (pqszStrings, i++);
          CopyVisualMemMap (pMem, pqszStrings[i], i, fOverlayFlag);
        }
    }
  else
    {
      PrepNextString (pqszStrings, i++);
      pqszStrings[i][0] = '\0';
    }


  /* Look for the last nonblank line */
  {
    BOOL fDone = FALSE;   /* Flag for dropping out of the loop */

    while (fDone == FALSE)
      {
        /* Strip extra spaces */
        PrepNextString (pqszStrings, i);

        /* Check to see if this line is blank */
        if (pqszStrings[i][0] == '\0' && i > 1)
          --i;
        else
          fDone = TRUE;
      }
  }

  /* Set the last pointer to NULL */

  pqszStrings[i + 1] = NULL;

  /* Return the pointer to pqszStrings */

  return (pqszStrings);
}


/*********************************************************************
 * CopyVisualMemMap - Copy the visual memory map to the string array.
 *
 * pMem         - Pointer to memory structure which contains the
 *                visual memory map.
 * pszString    - String upon which to copy the current line of the
 *                visual memory map.
 * i            - Current line number of the memory display strings.
 * fOverlayFlag - Places the overlay data (EMS Page frame, etc) onto
 *                the visual memory map, if TRUE.
 *
 * No return value
 *********************************************************************/

VOID CopyVisualMemMap (MEMORY_STRUCT *pMem,
                       QSZ  qszString,
                       WORD i,
                       BOOL fOverlayFlag)
{
  INT  iMapIndex;     /* Index to the visual memory map */
  CHAR chBuffer[80];  /* Local sprintf buffer           */
  WORD u;             /* Looping variable               */

  /* Make sure the string is blank */
  qszString[0] = '\0';

  /* Calculate the appropriate line of the visual mem map */
  /*   for this line in the display strings.              */
  iMapIndex = (NUM_OF_ROWS - 1) - (i - 2);

  if (i + 1 < MEM_640K_LIMIT)
    {
      /* Display decimal equivilent of memory location */
      if (i == 2)
        Qstrcpy (qszString, "1024K ");
      else if (iMapIndex % 4 == 0)
        {
          sprintf (chBuffer, "%4uK ",
                 (WORD) ((DWORD) iMapIndex * (DWORD) 0x4000 / (DWORD) 1024));
          Qstrcpy (qszString, chBuffer);
        }
      else
        Qstrcpy (qszString, "      ");

      /* Display hexadecimal equivilent of memory location */
      sprintf (chBuffer, "%04X ", (WORD) iMapIndex * 0x0400);
      Qstrcat (qszString, chBuffer);

      /* Put in memory map with the memory map overlay */
      Qstrcpy (chBuffer, pMem->abMemoryMap[iMapIndex]);
      for (u = 0; u < NUM_OF_COLS - 1; ++u)
        if (fOverlayFlag && pMem->abMemMapOverlay[iMapIndex][u])
          chBuffer[u] = pMem->abMemMapOverlay[iMapIndex][u];

      /* Add memory map string */
      if (fReportFlag == FALSE)
        Qstrcat (qszString, "&1");

      Qstrcat (qszString, chBuffer);

      if (fReportFlag == FALSE)
        Qstrcat (qszString, "&0");

      /* Display hexadecimal equivilent of memory location */
      sprintf (chBuffer, " %04X", (WORD) ((iMapIndex + 1) * 0x0400) - 1);
      Qstrcat (qszString, chBuffer);

      Qstrcat (qszString, "  ");
    }
  else
    {
      Qmemset (qszString, ' ', 34);
      qszString[34] = '\0';
    }
}
