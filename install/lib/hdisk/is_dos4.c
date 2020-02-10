/***************************************************************************/
/*                                                                         */
/* IS_DOS4.C                                                               */
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Determines if a BPB in a boot record is compatible with DOS 4.x. The    */
/* boot sector must have already been read in the global sector buffer.    */
/* First checks 512 bytes per sector, then maximum root directory entries  */
/* and then makes sure there is not a conflict in the total number of      */
/* sectors.                                                                */
/*                                                                         */
/* int IsDos4Compat( void )                                                */
/*                                                                         */
/* ARGUMENTS:  NONE                                                        */
/* RETURN:     int   - TRUE is BPB is DOS 4.x compatible else FALSE        */
/*                                                                         */
/* Created 02-07-90 johnhe                                                 */
/***************************************************************************/

#include <alias.h>
#include <disk_io.h>
#include <hdisk.h>

unsigned IsDos4Compat(char *SectorBuf)
{
   struct BPB *Bpb;

   Bpb = (struct BPB *)(SectorBuf + BPB_OFFSET);

   if ((Bpb->uBytesPerSec == (unsigned)BYTES_PER_SECTOR)

/*
** Yank these checks since they unnecessarily restrict us from installing on
** DOS 2.x and 3.x disks.
**
**    &&
**     (Bpb->uRootEntries <= (unsigned)MAX_DIR_ENTRIES)
**    &&
**     ((Bpb->uTotalSectors == 0U && Bpb->ulTotalBigSecs == 0UL) ||
**      (Bpb->uTotalSectors != 0U && Bpb->ulTotalBigSecs == 0UL) ||
**      (Bpb->uTotalSectors == 0U && Bpb->ulTotalBigSecs != 0UL))
*/

      )
      return(TRUE);

   return(FALSE);
}


