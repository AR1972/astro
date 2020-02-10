;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1988 - 1991
; *                      All Rights Reserved.
; */

/*
 * MEMBASE.C - MEM routines for determining and displaying memory usage
 *             for conventional memory.
 */

#include "stdio.h"
#include "dos.h"
#include "string.h"
#include "stdlib.h"
#include "msgdef.h"
#include "version.h"
#include "mem.h"

#define EMSGetHandleName 0x5300 	/* get handle name function */
#define EMSGetHandlePages 0x4c00	/* get handle name function */
#define EMSCODE_83	0x83		/* handle not found error */
#define EMSMaxHandles	256		/* max number handles */

/*---------------------------------------------------------------------------*/

	    /*
	     * DataLevel:  0 -     Print nothing here
	     *             1 - /C: Print nothing here
	     *             2 - /D: Print everything available
	     *             3 - /F: Print any line with Owner == 0
	     *             4 - /M: Print any line with OwnerName == ModName
	     *
	     * modulesize, below, is only used with options /F and /M.
	     *
	     */

long   modulesize = 0L;            /* Increases... 0== haven't started. */
char  *gpszDevDriver = NULL;
char   Out_Str1[64];
char   Out_Str2[64];
extern unsigned int MaxMin[2];
int   HandleIndex;		      /* used to step through emm386 handles */
char  HandleName[9];		      /* save area for emm386 handle name */

#define K384   (long)(393216)  // 384k, the size of the upper-memory segment
#define ONEMEG (long)(1048576) // 1MB

unsigned int DisplayBaseDetail()
{
   char   temp[128], t2[128];
   struct ARENA far *ThisArenaPtr;
   struct ARENA far *NextArenaPtr;
   struct ARENA far *ThisConfigArenaPtr;
   struct ARENA far *NextConfigArenaPtr;

   struct DEVICEHEADER far *ThisDeviceDriver;

   int   SystemDataType;
   char	 SystemDataOwner[64];

   unsigned long far *UMB_Head_ptr;
   unsigned far *EnvironmentSegmentPtr;

   unsigned int long    Out_Var1;
   unsigned int long    Out_Var2;
   unsigned int long    Org_IOsize;
   unsigned int long    Org_IOaddr;
   unsigned int long    umb_numb = 0L;

   InRegs.h.ah = (unsigned char) 0x52;
   intdosx(&InRegs,&OutRegs,&SegRegs);

   FP_SEG(SysVarsPtr)   = FP_SEG(UMB_Head_ptr) = SegRegs.es;
   FP_OFF(SysVarsPtr)   = OutRegs.x.bx;

   FP_OFF(UMB_Head_ptr) = 0x8c;          /* ptr to UMB_HEAD in DOS Data */ 
   UMB_Head = (*UMB_Head_ptr) & 0xFFFF;

   if (DataLevel == 2)
      {
      mprintf (NewLineMsg, "");
      mprintf (ConvMemDet, "");
      mprintf (NewLineMsg, "");
      mprintf (Title1Msg,  "");
      mprintf (Title2Msg,  "");
      }

   if (DataLevel == 3)
      {
      mprintf (NewLineMsg,    "");
      mprintf (FreeConvMsg,   "");
      mprintf (NewLineMsg,    "");
      mprintf (FreeTitleMsg,  "");
      mprintf (FreeUScoreMsg, "");
      }

   InRegs.h.ah = (unsigned char) 0x30;
   intdos(&InRegs, &OutRegs);

   if ( (OutRegs.h.al != (unsigned char) 3) ||
        (OutRegs.h.ah < (unsigned char) 40) )
      {
      UseArgvZero = TRUE;
      }
   else
      {
      UseArgvZero = FALSE;
      }

		       /* Display stuff below DOS  */

   Out_Var1 = 0l;
   Out_Var2 = 0x3FFl;
   if (AddMem_to_Table (8, Out_Var1, umb_numb, Out_Var2-16))  return 1;
   DoMainLine (8,&Out_Var1,BlankMsg,&Out_Var2,InterruptVectorMsg,&umb_numb);

   Out_Var1 = 0x40l;
   Out_Var2 = 0xFFl;
   if (AddMem_to_Table (8, Out_Var1, umb_numb, Out_Var2-16))  return 1;
   DoMainLine(8,&Out_Var1,BlankMsg,&Out_Var2,ROMCommunicationAreaMsg,&umb_numb);

   Out_Var1 = 0x50l;
   Out_Var2 = 0x1FFl;
   if (AddMem_to_Table (8, Out_Var1, umb_numb, Out_Var2-16))  return 1;
   DoMainLine(8,&Out_Var1,BlankMsg,&Out_Var2,DOSCommunicationAreaMsg,&umb_numb);

	      /* Display the BIO data location and size */

   Out_Var1 = 0x70l;
   Out_Var2 = (long) (FP_SEG(SysVarsPtr) - 0x70)*16L - 16L;
   if (AddMem_to_Table (8, Out_Var1, umb_numb, Out_Var2))  return 1;
   DoMainLine (8, &Out_Var1, IbmbioMsg, &Out_Var2, SystemDataMsg, &umb_numb);

	  /* Display the Base Device Driver Locations and Sizes */

     /*********************************************************************/
     /* to do this get the starting address of the internal driver header */
     /* chain. Start from the first header and get the address of the     */
     /* first header.  Display the driver name and address by calling     */
     /* "DISPLAYDEVICEDRIVER".  Repeat this for next driver on the chain  */
     /* until the last driver.  Note that driver name is in the header.   */
     /* The driver header addrs is in the system variable table from      */
     /* INT 21H fun 52H call.                                             */
     /*********************************************************************/

   BlockDeviceNumber = 0;

   if (DataLevel == 2)
      {
      for (ThisDeviceDriver = SysVarsPtr->DeviceDriverChain;
	   (FP_OFF(ThisDeviceDriver) != 0xFFFF);
	   ThisDeviceDriver = ThisDeviceDriver->NextDeviceHeader)
	 {
	 if ( FP_SEG(ThisDeviceDriver) < FP_SEG(SysVarsPtr) )
	    {
	    strcpy (temp, GetDeviceDriver (ThisDeviceDriver));
	    mprintf (DeviceLineMsg, "%-8c%-m", temp, SystemDeviceDriverMsg);
	    }
	 }
      }

	      /* Display the DOS data location and size */

   FP_SEG(ArenaHeadPtr) = FP_SEG(SysVarsPtr);         /* ;an004; */
   FP_OFF(ArenaHeadPtr) = FP_OFF(SysVarsPtr) - 2;     /* ;an004; */
   FP_SEG(ThisArenaPtr) = *ArenaHeadPtr;              /* ;an004; */
   FP_OFF(ThisArenaPtr) = 0;                          /* ;an004; */

   Out_Var1 = (long) FP_SEG(SysVarsPtr);
   Out_Var2 = (long) (AddressOf((char far *)ThisArenaPtr) - Out_Var1)*16L -16L;
   if (AddMem_to_Table (8, Out_Var1, umb_numb, Out_Var2))  return (1);
   DoMainLine (8, &Out_Var1, IbmdosMsg, &Out_Var2, SystemDataMsg, &umb_numb);

		    /* Display the memory data */

/******************************************************************************
/*
/* IO.SYS data area contains BUFFERS, FCBs, LAST DRIVE etc.  They are contained
/* in a one huge memory block.  This block has a seg iD 0008.  This seg ID can
/* be found from the block header owner area.  This seg id 0008:0000
/* points to the buffer table as shown below.  If seg id is 0008:000, then
/* using the seg id find the table.  Each entry is contained in a sub block
/* within the main block.  Each sub block has header and this header contains
/* id such as B for BUFFER,  X for FCBs,  I for IFS,  D for external device
/* drivers.  Go through the sub blocks and display the name ans size. that's it.
/*
/* If the block contains D, then it contains external drivers.  The driver name
/* is not in the sub block.  So we have to find the driver name from the driver
/* header chain.  To do this get the address of the driver chain from system
/* variable table from INT 21H FN 52H call.  Go through the chain and find out
/* the name.  Display name from the header and the size we got from the sub
/* block.
/*
/*
/* After this main block, comes other buffer blocks which contains programs
/* such as command.com, doscolor, even MEM.  From these blocks, get the program
/* name and the size and display them too.
/*
/* 0008:000->------------------          -------------------
/*           | BUFFERS        | -------->|B (signature)    | Block header
/*           ------------------          -------------------
/*           | FCBs           | --       |                 |
/*           ------------------   |      | Buffers data    |
/*           | IFSs           |   |      |                 |
/*           ------------------   |      |                 |
/*           | LAST DRIVE     |   |      |                 |
/*           ------------------   |      -------------------
/*           | EXTERN DRIVER 1|   |
/*           ------------------   |          -------------------
/*           | EXTERN DRIVER 2|   | -------->|X (signature)    | Block header
/*           ------------------              -------------------
/*           | EXTERN DRIVER 3|              |                 |
/*           ------------------              | Buffers data    |
/*                                           |                 |
/*                                           |                 |
/*                                           |                 |
/*                                           -------------------
/*
/* For DOS 5.0, there are some additions to the above.	Basically, we have
/* three possible memory maps, to wit:
/*
/*    DOS Loads Low			     DOS loads high
/*    70:0 - BIOS data			     70:0 - BIOS data
/*	     DOS data				    DOS data
/*	     BIOS + DOS code			    Sysinit data (arena name SD)
/*	       (arena owner 8, name "SC")	    VDisk header (arena name SC)
/*	     Sysinit data (arean owner 8, name SD)
/*
/*    DOS tries to load high but fails
/*    70:0 - BIOS data
/*	     DOS data
/*	     Sysinit data (arena name SD)
/*	     DOS + BIOS code (arena name SC)
/*
/*    We have to detect the special arena ownership marks and display them
/*    correctly.  Everything after DOS and BIOS data should have an arena header
/******************************************************************************/

   for (;;)
      {

#ifdef JAPAN
      if (ThisArenaPtr->Owner == 8 || ThisArenaPtr->Owner == 9)
#else
      if (ThisArenaPtr->Owner == 8)
#endif
	 {

	 FP_SEG(NextArenaPtr)=FP_SEG(ThisArenaPtr) +ThisArenaPtr->Paragraphs +1;
	 FP_OFF(NextArenaPtr)=0;

	 Out_Var1 = AddressOf((char far *)ThisArenaPtr);
	 Out_Var2 = (long) (ThisArenaPtr -> Paragraphs) * 16l;
	 if (ThisArenaPtr->OwnerName[0] == 'S' &&
	     ThisArenaPtr->OwnerName[1] == 'C')
	    {
	            /* display message for BIOS and DOS code */

	    if ((long)FP_SEG (ThisArenaPtr) >= (long)UMB_Head)
	       {
	       umb_numb++;

	       if (DataLevel == 2)
		  {
		  mprintf (NewLineMsg, "");

	          if (umb_numb == 1)
		     {
		     mprintf(UpperMemDet, "");
		     mprintf(NewLineMsg,  "");
		     mprintf(Title1AMsg,  "");
		     mprintf(Title2AMsg,  "");
		     }
                  }
	       }
	    else
	       {
	       if (AddMem_to_Table (8,Out_Var1,umb_numb,Out_Var2))
	          return(1);

	       DoMainLine (8, &Out_Var1, IbmdosMsg, &Out_Var2,
		              SystemProgramMsg, &umb_numb);
	       }
	    }
	 else if (ThisArenaPtr->OwnerName[0] == 'H')
	    {

	    if (AddMem_to_Table (0, Out_Var1, umb_numb, Out_Var2))
	       return(1);

/*
 * this block has been hidden via loadhigh for loading mem ... display it as
 * LOADHIGH / Temporarily Restricted
 *
 *          DoMainLine(0,&Out_Var1,LoadHighMsg,&Out_Var2,HiddenMsg,&umb_numb);
 *
 * That line removed in favor of making it all look more like free memory:
 *
 */

            DoMainLine (0,&Out_Var1,LoadHighMsg,&Out_Var2,FreeMsg,&umb_numb); 

	    }
	 else
	    {

/*
 * display message for data (8+SD)
 *
 */

            Org_IOaddr = Out_Var1;
            Org_IOsize = Out_Var2;

	    DoMainLine(8,&Out_Var1,IbmbioMsg,&Out_Var2,SystemDataMsg,&umb_numb);

	    FP_SEG(ThisConfigArenaPtr) = FP_SEG(ThisArenaPtr) +1;
	    FP_OFF(ThisConfigArenaPtr) = 0;

	    while ( (FP_SEG(ThisConfigArenaPtr) > FP_SEG(ThisArenaPtr)) &&
		    (FP_SEG(ThisConfigArenaPtr) < FP_SEG(NextArenaPtr)))
	       {

	       if (ThisConfigArenaPtr->Signature=='D' ||
	           ThisConfigArenaPtr->Signature=='I')
		  {
		  strcpy (SystemDataOwner, OwnerOf (ThisConfigArenaPtr));
		  }
	       else
		  {
		  strcpy (SystemDataOwner, " ");
		  }

	       switch (ThisConfigArenaPtr->Signature)
		  {
		  case 'B':  SystemDataType = ConfigBuffersMsg;          break;
		  case 'D':  SystemDataType = InstalledDeviceDriverMsg;  break;
		  case 'F':  SystemDataType = ConfigFilesMsg;            break;
		  case 'I':  SystemDataType = ConfigIFSMsg;              break;
		  case 'L':  SystemDataType = ConfigLastDriveMsg;        break;
		  case 'S':  SystemDataType = ConfigStacksMsg;           break;
		  case 'T':  SystemDataType = ConfigInstallMsg;          break;
		  case 'X':  SystemDataType = ConfigFcbsMsg;             break;
		  default:   SystemDataType = BlankMsg;                  break;
		  }

/*
 *  Found one, now display the owner name and size
 *
 */

	       if (SystemDataType != BlankMsg)
		  {
		  Out_Var1 = ((long) ThisConfigArenaPtr -> Paragraphs) * 16l;
		  sprintf (temp, "(%ldK)", toK(Out_Var1));
		  NextConfigArenaPtr = ThisConfigArenaPtr;
		  FP_SEG(NextConfigArenaPtr)+=NextConfigArenaPtr->Paragraphs+1;
		  if (ThisConfigArenaPtr->Signature == (char)'D')
		     {
		     FP_SEG(ThisDeviceDriver) = FP_SEG(ThisConfigArenaPtr) + 1;
		     FP_OFF(ThisDeviceDriver) = 0;

		     strcpy (t2, GetDeviceDriver (ThisDeviceDriver));

		     ThisDeviceDriver = ThisDeviceDriver->NextDeviceHeader;
		     }
		  else
		     {
		     strcpy (SystemDataOwner, DriverData (ThisConfigArenaPtr));
                     strcpy (t2,              " ");
		     }

		 if (ThisConfigArenaPtr->Signature=='D' ||
		     ThisConfigArenaPtr->Signature=='I')
		     {
		     Org_IOsize -= Out_Var1;
		     gpszDevDriver = SystemDataOwner;
		     AddMem_to_Table (7, Org_IOaddr, umb_numb, Out_Var1);
		     DoMainLine (7, &Org_IOaddr, 0, &Out_Var1,
		                 SystemDataType, &umb_numb);
		     }

		  if (DataLevel == 2)
		     {
		     mprintf (DriverLineMsg, "%8ld%7c%-8c%-m%-c",
			&Out_Var1, temp, t2, SystemDataType, SystemDataOwner);
		     }
		  }

	       FP_SEG(ThisConfigArenaPtr) += ThisConfigArenaPtr->Paragraphs +1;
	       }

/*
 * Now that we've added memory for each device driver, add whatever's left.
 *
 */

	    AddMem_to_Table (8, Org_IOaddr, umb_numb, Org_IOsize);
	    }
	 }
      else
         {

 /****************************************************************************/
 /* If not BIOS table, it is a program like MEM, etc.			    */
 /* calculate the size of the block occupied by the program and display prog */
 /* name and size                                                            */
 /****************************************************************************/

	 Out_Var1 = AddressOf((char far *)ThisArenaPtr);
	 Out_Var2 = ((long) (ThisArenaPtr -> Paragraphs)) * 16l;
	 strcpy(Out_Str1,OwnerOf(ThisArenaPtr));
	 strcpy(Out_Str2,TypeOf(ThisArenaPtr));
	 gpszDevDriver = Out_Str1;

	 /* We don't want to include mem's environment space into the
	  * computations. Since this environment space is allocated to
	  * running other programs, it really isn't part of free memory.
	  */

	  FP_SEG(EnvironmentSegmentPtr) = ThisArenaPtr->Owner;
	  FP_OFF(EnvironmentSegmentPtr) = 44;

	  if (!((_psp == ThisArenaPtr->Owner) &&
		(*EnvironmentSegmentPtr == FP_SEG(ThisArenaPtr)+1) ))
	     if (AddMem_to_Table (ThisArenaPtr->Owner,Out_Var1,umb_numb,Out_Var2))
		return(1);

	 DoMainLine_a (ThisArenaPtr->Owner, &Out_Var1, Out_Str1,
	               &Out_Var2, Out_Str2, &umb_numb); 
	 }

      if (ThisArenaPtr->Signature == (char)'Z')
         break;

      FP_SEG(ThisArenaPtr) += ThisArenaPtr->Paragraphs + 1;
      }

   if (DataLevel == 4)
      {
      if (! modulesize)
         {
         mprintf (ModNoneMsg, "%-c", ModName);
	 }
      else
         {
	 mprintf (ModBarMsg, "");
	 sprintf (temp, "(%ldK)", toK(modulesize));
	 mprintf (ModSumMsg, "%8ld%7c", &modulesize, temp);
	 }
      }

   if (DataLevel == 3)
      {
      mprintf (NewLineMsg, "");
      sprintf (temp, "(%ldK)", toK(modulesize));
      mprintf (FreeSumMsg, "%7ld%7c", &modulesize, temp);

      DisplayFree();
      }

   return(0);
}

void
DisplayFree ()
{
   char  temp1[30], temp2[30], temp3[30];
   long  i;

   mprintf (NewLineMsg, "");
   if (mem_table.umbs[0].umb_ttl == 0L)
      {
      mprintf (NoUMBAvailMsg, "");
      return;
      }

   mprintf (FreeUpperMsg,   "");
   mprintf (NewLineMsg,     "");
   mprintf (FreeUTitleMsg,  "");
   mprintf (FreeUUScoreMsg, "");

   for (i = 1; i < MAX_CLDATA_INDEX; i++)
      {
      if (mem_table.umbs[i-1].umb_ttl == 0L)  break;

      sprintf (temp1, "(%ldK)", toK (mem_table.umbs[i-1].umb_large));
      sprintf (temp2, "(%ldK)", toK (mem_table.umbs[i-1].umb_free));
      sprintf (temp3, "(%ldK)", toK (mem_table.umbs[i-1].umb_ttl));

      mprintf (MainFULineMsg, "%3ld%7ld%6c%7ld%6c%7ld%6c", &i,
               &mem_table.umbs[i-1].umb_large, temp1,
               &mem_table.umbs[i-1].umb_free,  temp2,
               &mem_table.umbs[i-1].umb_ttl,   temp3);
      }
}

/*---------------------------------------------------------------------------*/

char *GetDeviceDriver    (ThisDeviceDriver)
struct DEVICEHEADER far  *ThisDeviceDriver;
{
   static char  LocalDeviceName[16];
   int          i;

   if (((ThisDeviceDriver->Attributes) & 0x8000 ) != 0)
      {
      for (i = 0; i < 8; i++)  LocalDeviceName[i] = ThisDeviceDriver->Name[i];
      LocalDeviceName[8] = NUL;
      }
   else
      {
      if ((int)ThisDeviceDriver->Name[0] == 1)
         {
	 sprintf (&LocalDeviceName[0], SingleDrive, 'A'+BlockDeviceNumber);
         }
      else
         {
         sprintf (&LocalDeviceName[0], MultipleDrives, 'A'+BlockDeviceNumber,
	       'A'+BlockDeviceNumber + ((int)ThisDeviceDriver->Name[0]) -1);
         }

      BlockDeviceNumber += (int)(ThisDeviceDriver->Name[0]);
      }

   return LocalDeviceName;
}

/*---------------------------------------------------------------------------*/

void GetSummary()
{
   long    extra;
   long    total_mem;

   char	far *CarvedPtr;
   struct PSP_STRUC
      {
      unsigned int	int_20;
      unsigned int	top_of_memory;
      };
   struct PSP_STRUC far *PSPptr;

   /* Adjust for XBDA size */
   /* XBDA size should be added to total mem size reported by INT 12 */
   /* IFF XBDA is placed just at the end of conv.mem */
   /* IF EMM386 or QEMM is loaded, XBDA gets relocated to EMM driver mem */
   /* and int 12 reports correct size of memory in this case */
   InRegs.x.bx = 0;
   InRegs.x.ax = 0xc100;
   int86x(0x15, &InRegs, &OutRegs, &SegRegs);
   if (OutRegs.x.cflag == 0)
      {
      if (mem_table.conv_ttl == (unsigned long)((long)SegRegs.es) * 16l)
         {
	 FP_SEG(CarvedPtr) = SegRegs.es;
	 FP_OFF(CarvedPtr) = 0;
	 mem_table.conv_ttl += ((unsigned long int)(*CarvedPtr) * 1024l);
	 }
      }

   InRegs.h.ah = GET_PSP;
   intdos(&InRegs,&OutRegs);

   FP_SEG(PSPptr) = OutRegs.x.bx;
   FP_OFF(PSPptr) = 0;

	       /* Get total memory in system */

   int86 (MEMORY_DET,&InRegs,&OutRegs);

   mem_table.conv_ttl = (unsigned long int) OutRegs.x.ax * 1024l;
   mem_table.rom_ttl  = 0L;

   GetExtraMemory();  /* Get XMS/EMS information */

   if (mem_table.umb_ttl || mem_table.xms_ttl)
      {
      total_mem = mem_table.conv_ttl + mem_table.umb_ttl + mem_table.xms_ttl;

      if ((extra = ONEMEG - (total_mem % ONEMEG)) <= K384)
	 {
	 mem_table.rom_ttl = extra;
	 }
      }
}

void DispMEMSummary()
{
   char      temp1[40], temp2[40], temp3[40];
   char      far *fpBytes;
   long      used, t_used, t_ttl, t_free, c_used;
   int	     fPooled;

   // EMS is 'pooled' with XMS if this is a recent enough version of
   // EMM386 (fIsPooled) and the min and max EMS pool sizes are different.

   fPooled = fIsPooled() && (MaxMin[0] != MaxMin[1]) && EMSInstalled();

   mprintf (NewLineMsg,  "");
   mprintf (MemSumm1Msg, "");
   mprintf (MemSumm2Msg, "");

   t_used = c_used = 0L;

   used = mem_table.conv_ttl - mem_table.conv_free;
   t_used += used;
   sprintf (temp1, "%ldK", toK (mem_table.conv_ttl));
   sprintf (temp2, "%ldK", toK (used));
   sprintf (temp3, "%ldK", toK (mem_table.conv_free));
   mprintf (MemLineMsg, MemFormat, ConvMsg, temp1, temp2, temp3);

   used = mem_table.umb_ttl - mem_table.umb_free;
   t_used += used;
   sprintf (temp1, "%ldK", toK (mem_table.umb_ttl));
   sprintf (temp2, "%ldK", toK (used));
   sprintf (temp3, "%ldK", toK (mem_table.umb_free));
   mprintf (MemLineMsg, MemFormat, UpperMsg, temp1, temp2, temp3);

   c_used = t_used;
   used = mem_table.rom_ttl;
   t_used += used;
   sprintf (temp1, "%ldK", toK (mem_table.rom_ttl));
   sprintf (temp2, "%ldK", toK (mem_table.rom_ttl));
   sprintf (temp3, "%ldK", toK (0L));
   mprintf (MemLineMsg, MemFormat, AdaptMsg, temp1, temp2, temp3);

   used = mem_table.xms_ttl - mem_table.xms_free;
   t_used += used;
   sprintf (temp1, "%ldK", toK (mem_table.xms_ttl));
   sprintf (temp2, "%ldK", toK (used));
   sprintf (temp3, "%ldK", toK (mem_table.xms_free));
   if (fPooled)
      mprintf (MemLineMsg, MemFormat, XMSMsgPool, temp1, temp2, temp3);
   else
      mprintf (MemLineMsg, MemFormat, XMSMsg, temp1, temp2, temp3);

   mprintf (MemSumm2Msg, "");

   t_ttl  = mem_table.conv_ttl  + mem_table.umb_ttl  + mem_table.rom_ttl +
	    + mem_table.xms_ttl;
   t_free = mem_table.conv_free + mem_table.umb_free +
	    + mem_table.xms_free ;
   sprintf (temp1, "%ldK", toK (t_ttl));
   sprintf (temp2, "%ldK", toK (t_used));
   sprintf (temp3, "%ldK", toK (t_free));
   mprintf (MemLineMsg, MemFormat, TotalMsg, temp1, temp2, temp3);

   mprintf (NewLineMsg, "");
   t_ttl  = mem_table.conv_ttl  + mem_table.umb_ttl;
   t_free = mem_table.conv_free + mem_table.umb_free;
   sprintf (temp1, "%ldK", toK (t_ttl));
   sprintf (temp2, "%ldK", toK (c_used));
   sprintf (temp3, "%ldK", toK (t_free));
   mprintf (MemLineMsg, MemFormat, TtlConvMsg, temp1, temp2, temp3);

   mem_table.conv_large -= 16;  /* They can't use the header area */

   mprintf (NewLineMsg, "");

   /* Always print the ems total and free values from int 67/42 */
   if (mem_table.ems_ttl != 0) {
       sprintf (temp1, "%ldK", toK (mem_table.ems_ttl));
       sprintf (temp2, "(%ld bytes)  ", mem_table.ems_ttl);
       mprintf (TtlEms, "%6c%17c", temp1, temp2);

       sprintf (temp1, "%ldK", toK (mem_table.ems_free));
       sprintf (temp2, "(%ld bytes)  ", mem_table.ems_free);
       mprintf (fPooled ? FreeEMSPool : FreeEms, "%6c%17c", temp1, temp2);
       mprintf (NewLineMsg, "");
   }

   /* If EMS and XMS are shared, display the available EMS may vary disclaimer.
    */

   if (fPooled) {
      mprintf (PoolMsg1, "");
      mprintf (PoolMsg2, "");
      mprintf (NewLineMsg, "");
   }

   InRegs.x.ax = Bites;
   InRegs.h.dh = Utility_Msg_Class;
   sysgetmsg(&InRegs,&SegRegs,&OutRegs);
   FP_OFF(fpBytes) = OutRegs.x.si;
   FP_SEG(fpBytes) = SegRegs.ds;

   sprintf (temp1, "%ldK", toK (mem_table.conv_large));
   sprintf (temp2, "(%ld %s)  ", mem_table.conv_large, fpBytes);
   mprintf (LargeExMsg,  "%6c%17c", temp1, temp2);
   sprintf (temp1, "%ldK", toK (mem_table.umb_large));
   sprintf (temp2, "(%ld %s)  ", mem_table.umb_large, fpBytes);
   mprintf (LargeUMBMsg, "%6c%17c", temp1, temp2);

   switch (mem_table.hma)
      {
      case -2:	break;		 /* HMA doesn't exist, don't print anything */
      case -1:	mprintf (HMANotAvlMsg, "");  break; /* exists, but used */
      case  0:	mprintf (HMAAvlMsg,    "");  break; /* exists and free */
      case  1:	mprintf (HMADOSMsg,    "");  break; /* in use by MSDOS */
      default:	mprintf (ROMDOSMsg,    "");  break; /* in use by ROM DOS */
      }

   return;
}

void DisplaySummary()
{
   unsigned long int HandleMem; 	 /* memory associated w/handle */
   char  TitlesPrinted = FALSE; 	 /* flag for printing titles */

   char  temp1[40], temp2[40], temp3[40];
   long  used, t_used, t_ttl, t_free, c_used;
   int   fPooled;

   // EMS is 'pooled' with XMS if this is a recent enough version of
   // EMM386 (fIsPooled) and the min and max EMS pool sizes are different.

   fPooled = fIsPooled() && (MaxMin[0] != MaxMin[1]) && EMSInstalled();

   mprintf (NewLineMsg,   "");
   mprintf (MemSumMsg,    "");
   mprintf (NewLineMsg,   "");
   mprintf (SumTitleMsg,  "");
   mprintf (SumUScoreMsg, "");

   t_used = c_used = 0L;

   used = mem_table.conv_ttl - mem_table.conv_free;
   t_used += used;
   sprintf (temp1, "(%ldK)", toK (mem_table.conv_ttl));
   sprintf (temp2, "(%ldK)", toK (used));
   sprintf (temp3, "(%ldK)", toK (mem_table.conv_free));
   mprintf (SumLineMsg, SumFormat, ConvMsg,
            &mem_table.conv_ttl, temp1, &used, temp2,
            &mem_table.conv_free, temp3);

   used = mem_table.umb_ttl - mem_table.umb_free;
   t_used += used;
   sprintf (temp1, "(%ldK)", toK (mem_table.umb_ttl));
   sprintf (temp2, "(%ldK)", toK (used));
   sprintf (temp3, "(%ldK)", toK (mem_table.umb_free));
   mprintf (SumLineMsg, SumFormat, UpperMsg,
            &mem_table.umb_ttl, temp1, &used, temp2,
            &mem_table.umb_free, temp3);

   c_used = t_used;

   t_used += mem_table.rom_ttl;
   used = 0L;
   sprintf (temp1, "(%ldK)", toK (mem_table.rom_ttl));
   sprintf (temp2, "(%ldK)", toK (mem_table.rom_ttl));
   sprintf (temp3, "(%ldK)", toK (0L));
   mprintf (SumLineMsg, SumFormat, AdaptMsg,
            &mem_table.rom_ttl, temp1, &mem_table.rom_ttl, temp2,
            &used, temp3);

   used = mem_table.xms_ttl - mem_table.xms_free;
   t_used += used;
   sprintf (temp1, "(%ldK)", toK (mem_table.xms_ttl));
   sprintf (temp2, "(%ldK)", toK (used));
   sprintf (temp3, "(%ldK)", toK (mem_table.xms_free));
   if (fPooled)
      mprintf (SumLineMsg, SumFormat, XMSMsgPool,
	       &mem_table.xms_ttl, temp1, &used, temp2,
	       &mem_table.xms_free, temp3);
   else
      mprintf (SumLineMsg, SumFormat, XMSMsg,
	       &mem_table.xms_ttl, temp1, &used, temp2,
	       &mem_table.xms_free, temp3);

   mprintf (SumUScoreMsg, "");

   t_ttl  = mem_table.conv_ttl  + mem_table.umb_ttl  + mem_table.rom_ttl +
	    + mem_table.xms_ttl;
   t_free = mem_table.conv_free + mem_table.umb_free +
	    + mem_table.xms_free ;
   sprintf (temp1, "(%ldK)", toK (t_ttl));
   sprintf (temp2, "(%ldK)", toK (t_used));
   sprintf (temp3, "(%ldK)", toK (t_free));
   mprintf (SumLineMsg, SumFormat, TotalMsg,
                 &t_ttl, temp1, &t_used, temp2, &t_free, temp3);

   mprintf (NewLineMsg, "");
   t_ttl  = mem_table.conv_ttl  + mem_table.umb_ttl;
   t_free = mem_table.conv_free + mem_table.umb_free;
   sprintf (temp1, "(%ldK)", toK (t_ttl));
   sprintf (temp2, "(%ldK)", toK (c_used));
   sprintf (temp3, "(%ldK)", toK (t_free));
   mprintf (SumLineMsg, SumFormat, TtlConvMsg,
                 &t_ttl, temp1, &c_used, temp2, &t_free, temp3);
   mprintf (NewLineMsg, "");

   /* if ems is install or no NOEMS, then display the handles */
   if (EMSInstalled() && (DataLevel == 2)) {
       HandleName[0] = NUL;	      /* initialize the array	      */

       mprintf (NewLineMsg,   "");

       segread(&SegRegs);

       SegRegs.es = SegRegs.ds;

       for (HandleIndex = 0; HandleIndex < EMSMaxHandles; HandleIndex++)
       {

	   InRegs.x.ax = EMSGetHandleName;     /* get handle name */
	   InRegs.x.dx = HandleIndex;	       /* handle in question */
	   InRegs.x.di = (unsigned int) HandleName;    /* point to handle name */
	   int86x(EMS, &InRegs, &OutRegs, &SegRegs);

	   HandleName[8] = NUL; 	       /* make sure terminated w/nul */

	   if (OutRegs.h.ah != EMSCODE_83)
	   {
	       InRegs.x.ax = EMSGetHandlePages;  /* get pages assoc w/this handle */
	       InRegs.x.dx = HandleIndex;
	       int86x(EMS, &InRegs, &OutRegs, &SegRegs);
	       HandleMem = OutRegs.x.bx;
	       HandleMem *= (long) (16l*1024l);

	       if (!TitlesPrinted)
	       {
		   mprintf (Title3Msg,	 "");
		   mprintf (Title4Msg,	 "");
		   TitlesPrinted = TRUE;
	       }

	       if (HandleName[0] == NUL)
		   strcpy(HandleName,"        ");

	       mprintf (HandleMsg, "%4d%8c%6lx", &HandleIndex, HandleName, &HandleMem);
	   }
       }  /* end   for (HandleIndex = 0; HandleIndex < EMSMaxHandles;HandleIndex++) */

       mprintf (NewLineMsg,   "");
   }

   /* Always print the ems total and free values from int 67/42 */
   if (mem_table.ems_ttl != 0) {
       /* print the total and free ems lines */
       sprintf (temp1, "(%ldK)", toK (mem_table.ems_ttl));
       sprintf (temp2, "%8.8s", temp1);
       NoCR = 1;
       mprintf (SpaceOverMsg, "");
       mprintf (TtlEms,  "%10ld%8c", &mem_table.ems_ttl, temp2);

       sprintf (temp1, "(%ldK)", toK (mem_table.ems_free));
       sprintf (temp2, "%8.8s", temp1);
       NoCR = 1;
       mprintf (SpaceOverMsg, "");
       mprintf (fPooled ? FreeEMSPool : FreeEms,  "%10ld%8c", &mem_table.ems_free, temp2);
   }


   /* If EMS and XMS are shared, display the available EMS may vary disclaimer.
    */

   if (fPooled) {

       mprintf (NewLineMsg,   "");

       NoCR = 1;
       mprintf (SpaceOverMsg, "");
       mprintf (PoolMsg1, "");

       NoCR = 1;
       mprintf (SpaceOverMsg, "");
       mprintf (PoolMsg2, "");

       mprintf (NewLineMsg, "");
   }


   if (DataLevel == 2)
      {
      sprintf (temp1, "(%ldK)", toK (mem_table.int_15h));
      sprintf (temp2, "%8.8s", temp1);
      NoCR = 1;  mprintf (SpaceOverMsg, "");
      mprintf (Int15MemoryMsg, "%10ld%8c", &mem_table.int_15h, temp2);
      }

   mem_table.conv_large -= 16;  /* They can't use the header area */
   sprintf (temp1, "(%ldK)", toK (mem_table.conv_large));
   sprintf (temp2, "%8.8s", temp1);
   NoCR = 1;  mprintf (SpaceOverMsg, "");
   mprintf (LargeExMsg,  "%10ld%8c", &mem_table.conv_large, temp2);

   sprintf (temp1, "(%ldK)", toK (mem_table.umb_large));
   sprintf (temp2, "%8.8s", temp1);
   NoCR = 1;  mprintf (SpaceOverMsg, "");
   mprintf (LargeUMBMsg, "%10ld%8c", &mem_table.umb_large, temp2);

   NoCR = 1;  mprintf (SpaceOverMsg, "");
   switch (mem_table.hma)
      {
      case -2:	break;
      case -1:  mprintf (HMANotAvlMsg, "");  break;
      case  0:  mprintf (HMAAvlMsg,    "");  break;
      case  1:  mprintf (HMADOSMsg,    "");  break;
      default:  mprintf (ROMDOSMsg,    "");  break;
      }

   if (DataLevel == 2)
      {
      used = 0;  /* Okay, a second use for this thing... */
      if (mem_table.xmsMvers != 0)
         {
	 mprintf (NewLineMsg, "");
	 sprintf (temp1, "%d.%02d", mem_table.xmsMvers, mem_table.xmsmvers);
	 sprintf (temp2, "%d.%02d", mem_table.xmsMdrvr, mem_table.xmsmdrvr);
	 mprintf (XMSVersionMsg, "%5c%5c", temp1, temp2);
	 used = 1;
	 }
      if (mem_table.emsMvers != 0)
         {
	 if (! used)  mprintf (NewLineMsg, "");
	 sprintf (temp1, "%d.%02d", mem_table.emsMvers, mem_table.emsmvers);
	 mprintf (EMSVersionMsg, "%5c", temp1);
	 }
      }

   return;
}

/*---------------------------------------------------------------------------*/

char *OwnerOf    (ArenaPtr)
struct ARENA far *ArenaPtr;
{
   char        far *StringPtr;
   char	           *o;
   unsigned    far *EnvironmentSegmentPtr;
   unsigned         PspSegment;
   int	            i, fPrintable;

    o = &OwnerName[0];
   *o = NUL;
   strcpy (o, UnOwned);

   PspSegment = ArenaPtr->Owner;

   if (PspSegment == 0)
      sprintf(o,Ibmdos);
   else
      if (PspSegment == 8)
	 sprintf (o, Ibmbio);
      else
	 {
	 FP_SEG(ArenaPtr) = PspSegment-1;   /* Arena is 16 bytes before PSP */
	 StringPtr = (char far *) &(ArenaPtr -> OwnerName[0]);

	 /* M002 BEGIN
	  * Chars below 0x20 (Space) and char 0x7f are not printable in US
	  * and European Code pages.  The following code checks for them and
	  * does not print such names.  - Nagara 11/20/90
	  */

	 fPrintable = TRUE;

#ifndef DBCS
	 for (i = 0; i < 8;i++,StringPtr++)
	    {
	    if ((*StringPtr < 0x20) | (*StringPtr == 0x7f))
	       {  
		      /* unprintable char ? */	
	       if (*StringPtr)  fPrintable = FALSE;	
	       break;
	       }
	    }
#endif

	 if (fPrintable)
	    {
	    StringPtr = (char far *) &(ArenaPtr->OwnerName[0]);
	    for (i = 0; i < 8;i++)
	       *o++ = *StringPtr++;
	    *o = NUL;
	    }
	 /* M002 END */

	 }

   if (UseArgvZero)  GetFromArgvZero (PspSegment, EnvironmentSegmentPtr);

   return (&OwnerName[0]);
}

/*---------------------------------------------------------------------------*/

void GetFromArgvZero (PspSegment, EnvironmentSegmentPtr)
unsigned              PspSegment;
unsigned                     far *EnvironmentSegmentPtr;
{
   char	    far *StringPtr;
   char	        *OutputPtr;
   unsigned far *WordPtr;

   OutputPtr = &OwnerName[0];

   if (UseArgvZero)
      {
      if (PspSegment < FP_SEG(ArenaHeadPtr))
	 {
	 if (*OutputPtr == NUL)  sprintf (OutputPtr,Ibmdos);
	 }
      else
	 {
	 FP_SEG(EnvironmentSegmentPtr) = PspSegment;
	 FP_OFF(EnvironmentSegmentPtr) = 44;

	      /*  FP_SEG(StringPtr) = *EnvironmentSegmentPtr; */

	 FP_SEG(StringPtr) = FP_SEG(EnvironmentSegmentPtr);
	 FP_OFF(StringPtr) = 0;

	 while ((*StringPtr != NUL) || (*(StringPtr+1) != NUL))  StringPtr++;

	 StringPtr += 2;
	 WordPtr    = (unsigned far *)StringPtr;

	 if (*WordPtr == 1)
	    {
	    StringPtr += 2;
	    while (*StringPtr != NUL)
	       *OutputPtr++ = *StringPtr++;
	    *OutputPtr++ = NUL;

	    while (OutputPtr > &OwnerName[0])
	       {
	       if (*OutputPtr == (char) '.')
	          *OutputPtr = NUL;
	       if ((*OutputPtr == (char) '\\') || (*OutputPtr == (char) ':'))
		  {
		  OutputPtr++;
		  break;
		  }
	       OutputPtr--;
	       }
	    }
	 }
      }

   strcpy (&OwnerName[0], OutputPtr);

   return;
}

/*---------------------------------------------------------------------------*/

char *TypeOf     (Header)
struct ARENA far *Header;
{
   char	        *t;
   unsigned      PspSegment;
   unsigned far *EnvironmentSegmentPtr;
   unsigned int  Message_Number;
   char far     *Message_Buf;
   unsigned int  i;

   t = &TypeText[0];
   *t = NUL;

   Message_Number = 0xff;
   if (Header->Owner == 8)  Message_Number = StackMsg;
   if (Header->Owner == 0)  Message_Number = FreeMsg;

   PspSegment = Header -> Owner;
   if (PspSegment < FP_SEG(ArenaHeadPtr))
      {
      if (Message_Number == 0xff)  Message_Number = BlankMsg;
      }
   else
      {
      FP_SEG(EnvironmentSegmentPtr) = PspSegment;
      FP_OFF(EnvironmentSegmentPtr) = 44;

      if (PspSegment == FP_SEG(Header)+1)
	 Message_Number = ProgramMsg;
      else
         if (*EnvironmentSegmentPtr == FP_SEG(Header)+1)
	    Message_Number = EnvironMsg;
	 else
	    Message_Number = DataMsg;
      }

   InRegs.x.ax = Message_Number;
   InRegs.h.dh = Utility_Msg_Class;
   sysgetmsg(&InRegs,&SegRegs,&OutRegs);

   FP_OFF(Message_Buf)    = OutRegs.x.si;
   FP_SEG(Message_Buf)    = SegRegs.ds;

   i = 0;
   while (*Message_Buf != NUL)
      TypeText[i++] = *Message_Buf++;
   TypeText[i++] = NUL;

   return(t);
}

/************************************************************************/
/* DisplayClassification						*/
/*	Main display proc for /C switch 				*/
/*									*/	
/* ENTRY:	none							*/
/*									*/	
/* EXIT:	none 							*/
/*									*/
/*									*/
/* CAVEATS:								*/
/*	Arenas owned by MEM are marked as FREE				*/	
/*									*/	
/*	display memory break up for conventional and upper memory	*/
/*									*/	
/************************************************************************/

void DisplayClassification()
{

   int               i, msgtype, dd;
   unsigned long     memsize;
   char             *nameptr;
   char              temp1[12], temp2[12], temp3[12];
   struct ARENA far *ArenaPtr;

   mprintf (NewLineMsg,    "");
   mprintf (CTtlTitleMsg,  "");
   mprintf (NewLineMsg,    "");
   mprintf (CTtlNameMsg,   "");
   mprintf (CTtlUScoreMsg, "");

   for (i = 0; i < mem_table.noof_progs; i++)
      {
      if (mem_table.files[i].psp_addr  < 1)  continue; /* Display FREE last */
      if (mem_table.files[i].psp_addr == 7)
         {
         if ((dd = mem_table.files[i].driveridx) == MAX_DDRIVER_REP)
            {
	    continue;
	    }
	 }

      msgtype = 0;
      if (mem_table.files[i].psp_addr == 8)
	 {
	 msgtype = (mem_table.files[i].umb_ttl) ? SystemMsg : IbmdosMsg;
	 }

      memsize = mem_table.files[i].conv_ttl + mem_table.files[i].umb_ttl;
      sprintf (temp1, "(%ldK)", toK (mem_table.files[i].conv_ttl));
      sprintf (temp2, "(%ldK)", toK (mem_table.files[i].umb_ttl));
      sprintf (temp3, "(%ldK)", toK (memsize));

      if (msgtype)
         {
	 mprintf (MainCLineMsg, "%-8m%8ld%7c%8ld%7c%8ld%7c", msgtype,
	      &memsize, temp3, &mem_table.files[i].conv_ttl, temp1,
	                        &mem_table.files[i].umb_ttl, temp2);
	 }
      else
	 {
	 if (mem_table.files[i].psp_addr == 7)
	    {
	    nameptr = ddrivername[dd];
	    }
	 else
	    {
	    FP_SEG(ArenaPtr) = mem_table.files[i].psp_addr -1;
	    FP_OFF(ArenaPtr) = 0;
	    nameptr = OwnerOf(ArenaPtr);
            }

	 mprintf (MainCLineMsg, "%-8c%8ld%7c%8ld%7c%8ld%7c", nameptr,
	      &memsize, temp3, &mem_table.files[i].conv_ttl, temp1,
	                        &mem_table.files[i].umb_ttl, temp2);
	 }
      }

   for (i = 0; i < mem_table.noof_progs; i++)  /* There should be only 1 */
      {
      if (mem_table.files[i].psp_addr)  continue;

      memsize = mem_table.files[i].conv_ttl + mem_table.files[i].umb_ttl;
      sprintf (temp1, "(%ldK)", toK (mem_table.files[i].conv_ttl));
      sprintf (temp2, "(%ldK)", toK (mem_table.files[i].umb_ttl));
      sprintf (temp3, "(%ldK)", toK (memsize));

      mprintf (MainCLineMsg, "%-8m%8ld%7c%8ld%7c%8ld%7c", CFreeMsg,
	   &memsize, temp3, &mem_table.files[i].conv_ttl, temp1,
	                    &mem_table.files[i].umb_ttl,  temp2);
      }
}

/*----------------------------------------------------------------------*/
/*  AddMem_to_Table						        */
/*	Entry:	PSP_ADDR	(to which this mem. should be added)	*/
/*		ARENA_START_ADDR					*/
/*		Region of arena (0 == conv, 1 == UMB#1, 2 == UMB#2...)	*/
/*		Length_of_Arena						*/
/*	Exit:	mem_table updated.			      		*/
/*		returns 1 if more than MAX_CL_ENTRIES in mem_table	*/
/*		   else 0						*/
/*									*/
/* CAVEATS:						  		*/
/* --------								*/
/* 1. any system area (BIOS,SYSINIT,DOS ) code/data is listed as	*/
/*    to PSP 8.							        */
/*									*/
/* 2. We look at the UMB_HEAD in DOS DATA to determine whether an arena */
/*    is in UMB or not; For the Arena at the UMB boundary, we add one   */
/*    para to conv. and remaining to UMB portion of that PSP	        */
/*									*/
/* 3. Any free memory is always added as a new entry in the mem_table   */
/*    instead of just adding the sizes to an existing FREE entry        */
/*    Free memory gets added to the previous free memory if they are    */
/*    contiguous							*/
/*									*/
/* 4. The no of programs/free arenas cannot exceed a max of (100)	*/
/*    (defined by MAX_CLDATA_INDEX )       				*/
/*    If the memory is fragmented and a lot of small TSRs loaded such   */
/*    that we exceed this limit, we TERMINATE				*/
/*									*/
/* 5. Mem occupied by this MEM are also reported as FREE mem		*/
/*									*/
/*----------------------------------------------------------------------*/

/*
 * AddMem_to_Table() should be called for each given primary segment in the
 * detailed display.  By the time the detail is over, all conventional and
 * upper memory summaries of any sort will be finished (neato eh?), save the
 * sorting of programs (if you wanna do that).
 *
 */

unsigned int AddMem_to_Table (psp, addr, region, size)
unsigned int                  psp;
unsigned long                      addr, region, size;
{
   int               dd,            i;
   static long       memsize = 0L,  wasfree = 0L;
   static int        ismem   = 0;
   struct ARENA far *ArenaPtr;

   if (psp == 7)
      {
      dd = (unsigned int)GetDDriverPSP ();
      }
   else
      {
      FP_SEG(ArenaPtr) = (unsigned)addr-1; // Find the arena so we can get name
      FP_OFF(ArenaPtr) = 0;                // to compare with device drivers.

      if ((dd = IsDDriverAround (gpszDevDriver)) < 0) // If there's no DD
         dd = MAX_DDRIVER_REP;                        // with the same
      else                                            // name, it's nrml
         psp = 7;     // Otherwise, pretend we're filling in the DD list.
      }

   size += 16;  /* Header */

   if (ismem && psp == 0)
      {
      wasfree = memsize;
      memsize = 0;
      }

   ismem = 0;
   if (psp == _psp)
      {
      if (wasfree && region == 0L)
         {
	 memsize += wasfree;
	 }

      psp = 0;  /* treat MEM's arenas as FREE */
      ismem = 1;
      wasfree = 0L;
      }
   else
      wasfree = (psp == 0 && region == 0L) ? wasfree+size : 0L;

   for (i = 0; i < mem_table.noof_progs; i++)
      if (mem_table.files[i].psp_addr == psp)
         {
	 if ((psp != 7) || mem_table.files[i].driveridx == dd)
	    break;
	 }

	   /* if psp is not already listed in the table, add it */

   if (i == mem_table.noof_progs)
      {
      if (mem_table.noof_progs == MAX_CLDATA_INDEX)
	 {
	 mprintf(CMemFragMsg, "");
	 return(1);
	 }
      mem_table.files[i].psp_addr  = psp;
      mem_table.files[i].driveridx = dd;
      mem_table.noof_progs ++;
      }

		 /* add the memory to the table entry */

   if (ismem && region==0L)
      memsize += size;

   if (addr < UMB_Head)
      {
      mem_table.files[i].conv_ttl += size;
      if (psp == 0)
         mem_table.conv_free += size;
      if (psp == 0 && size > mem_table.conv_large)
         mem_table.conv_large = size;
      if (ismem && memsize > (long)mem_table.conv_large)
         mem_table.conv_large = size;
      if (wasfree > (long)mem_table.conv_large)
         mem_table.conv_large = wasfree;
      }
   else
      {
      mem_table.umb_ttl += size;
      if (psp == 0)
         mem_table.umb_free += size;

      mem_table.files[i].umb_ttl += size;
      if (psp == 0 && size > mem_table.umb_large)
	mem_table.umb_large = size;
      }

   if (region == 0L)  return 0;

   mem_table.umbs[(int)region-1].umb_ttl += size;
   if (psp == 0)
      {
      mem_table.umbs[(int)region-1].umb_free += size;
      if (size > mem_table.umbs[(int)region-1].umb_large)
	 mem_table.umbs[(int)region-1].umb_large = size;
      }
   if (mem_table.umbs[(int)region-1].umb_addr == 0L ||
       addr < mem_table.umbs[(int)region-1].umb_addr)
      {
      mem_table.umbs[(int)region-1].umb_addr = addr;
      }

  return 0;
}

/*---------------------------------------------------------------------------*/
/*                                                                           */
/* A quick bit about headers:  MEM now reports all headers, or MCBs, as      */
/* space allocated by modules; ie, a module consisting of 2 MCBs, one which  */
/* points to a 50-paragraph block, and one which points to a 25-paragraph    */
/* block, will be listed in MEM/C as using 77 paragraphs... 1 for each MCB   */
/* as well.  MEM/D will list the individual sizes as 51 and 26 paragraphs,   */
/* so that size + address gives the next address.  This is mentioned here    */
/* because things... well... have not always been this way.                  */
/*                                                                           */
/*---------------------------------------------------------------------------*/

void
DoMainLine        (Owner, Address, Name, Size, Type, Region)
int                Owner,          Name,       Type;
unsigned long int        *Address,      *Size,      *Region;
{
   char  temp[40], far *ptr, temp2[9];

   if (Owner != 7)  *Size += 16;  // For display ONLY--see end of this routine

   if (DataLevel == 2 && Owner != 7)
      {
      mainline (Address, Name, Size, Type, Region);
      }

   if (DataLevel == 3 && *Region == 0L && (Owner == 0 || Owner == _psp))
      {
      sprintf (temp, "(%ldK)", toK (*Size));
      mprintf (MainFLineMsg, "%5lx%8ld%7c", Address, Size, temp);
      modulesize += *Size;
      }
   if (DataLevel == 4)
      {
      if (Owner == 7)
         {
	 strcpy (temp2, gpszDevDriver);
	 ptr = temp2;
	 }
      else
	 {
	 InRegs.x.ax = Name;
	 InRegs.h.dh = Utility_Msg_Class;
	 sysgetmsg(&InRegs,&SegRegs,&OutRegs);
	 FP_OFF(ptr) = OutRegs.x.si;
	 FP_SEG(ptr) = SegRegs.ds;
         }

      estrip (ptr);
      if (stricmp (ModName, (char *)ptr))  return;

      if (! modulesize)
         {
	 mprintf (NewLineMsg,   "");
	 mprintf (ModUseMsg,    "%-c", ModName);
	 mprintf (NewLineMsg,   "");
	 mprintf (ModTitleMsg,  "");
	 mprintf (ModUScoreMsg, "");
	 }
      sprintf (temp, "(%ldK)", toK (*Size));
      if (*Region == 0L)
         {
	 if (Owner != 7)
	    {
	    mprintf (MainMLineMsg, "%5lx%8ld%7c%-m", Address,Size,temp,Type);
	    }
	 else
	    {
	    mprintf (MainMDLineMsg, "%5lx%8ld%7c%-m%-c", Address, Size,
	                            temp, Type, ptr);
	    }
         }
      else
         {
	 if (Owner != 7)
	    {
	    mprintf (MainMXLineMsg, "%5lx%3ld%8ld%7c%-m", Address, Region,
		     Size, temp, Type);
	    }
	 else
	    {
	    mprintf (MainMDXLineMsg, "%5lx%3ld%8ld%7c%-m%-c", Address, Region,
		     Size, temp, Type, ptr);
	    }
         }
      modulesize += *Size;
      }

   if (Owner != 7)  *Size -= 16;  // For display ONLY--see top of this routine
}

void
DoMainLine_a      (Owner, Address, Name, Size, Type, Region)
int                Owner;
char                              *Name,      *Type;
unsigned long int        *Address,      *Size,      *Region;
{
   char  temp[40];
   *Size += 16;
   if (DataLevel == 2)  mainline_a (Address, Name, Size, Type, Region);
   if (DataLevel == 3 && *Region == 0L && (Owner == 0 || Owner == _psp))
      {
      sprintf (temp, "(%ldK)", toK (*Size));
      mprintf (MainFLineMsg, "%5lx%8ld%7c", Address, Size, temp);
      modulesize += *Size;
      }
   if (DataLevel == 4)
      {
      estrip ((char far *)Name);
      if (stricmp (Name, ModName))
      if (stricmp (Name, ModName))  return;

      if (! modulesize)
         {
	 mprintf (NewLineMsg,   "");
	 mprintf (ModUseMsg,    "%-c", ModName);
	 mprintf (NewLineMsg,   "");
	 mprintf (ModTitleMsg,  "");
	 mprintf (ModUScoreMsg, "");
	 }
      sprintf (temp, "(%ldK)", toK (*Size));
      if (*Region == 0L)
         mprintf (MainMLineMsg, "%5lx%8ld%7c%-c", Address,
		  Size, temp, Type);
      else
         mprintf (MainMXLineMsg, "%5lx%3ld%8ld%7c%-c", Address, Region,
		  Size, temp, Type);
      modulesize += *Size;
      }
   *Size -= 16;
}

void
estrip   (str)
char far *str;
{
   char far *p;
   if ((p = strchr ((char *)str, ' ')) != NULL)  *p = 0;
}

/*
 * DriverData() returns a character string containing:
 *
 *    Driver == 'F' : Number of files from "FILES=XXX" statement
 *    Driver == 'X' : Number of FCBS from "FCBS=XXX" statement
 *    Driver == 'B' : Number of buffers from "BUFFERS=XXX" statment
 *    Driver == 'L' : Letter of last drive from "LASTDRIVE=?" statement
 *    Driver == 'S' : Proper format from "STACKS=XXX,XXX" statement
 *
 * Thanks go to EricAr for his work here.
 *
 */

char *
DriverData       (ptr)
struct ARENA far *ptr;
{

#define buffer_m (int) (SysVarsPtr->BufferValues & 0x00FF)
#define buffer_n (int)((SysVarsPtr->BufferValues & 0xFF00) >> 8)

   void    far *tmp, far *tmp2;
   static char  buf[40];

   strcpy (buf, "?");

   tmp  = ptr;  FP_SEG(tmp)++;
   tmp2 = ptr;  FP_SEG(tmp2)++;
   FP_OFF(tmp)  = 4L;
   FP_OFF(tmp2) = 6L;
   switch (ptr->Signature)
      {
      case 'F':  sprintf (buf, "%d", *(short far *)tmp +5);
		break;
      case 'X':  sprintf (buf, "%d", *(short far *)tmp);
		break;
      case 'B':  if (! buffer_n)  sprintf (buf, "%d", buffer_m);
                 else             sprintf (buf, "%d,%d", buffer_m, buffer_n);
		break;
      case 'L':  sprintf (buf, "%c", ('A'-1) +SysVarsPtr->CdsCount);
		break;
      case 'S':  FP_OFF(tmp) = 2L;
                 sprintf (buf, "%d,%d", *(short far *)tmp, *(short far *)tmp2);
		break;
      }
   return buf;
}

unsigned int GetDDriverPSP ()
{
   register int  i;

   for (i = 0; i < ddriveridx; i++)
      {
      if (! strcmp (ddrivername[i], gpszDevDriver))
	 break;
      }
   if (i >= MAX_DDRIVER_REP)
      {
      return MAX_DDRIVER_REP;  // Too many?
      }
   if (i == ddriveridx)
      {
      strcpy (ddrivername[i], gpszDevDriver);
      ddriveridx++;
      }
   return i;
}

unsigned int IsDDriverAround (char *name) // Returns -1 if not, else its #.
{
   register int  i;

   for (i = 0; i < ddriveridx; i++)
     {
     if (! strcmp (ddrivername[i], name))
        break;
     }
   return (i < ddriveridx) ? i : -1;
}

/* M003 END */
