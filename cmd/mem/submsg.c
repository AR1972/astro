;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1988 - 1991
; *                      All Rights Reserved.
; */

 /*
  * SUBMSG.C - Message retriever interface functions for MEM command.
  *
  *
  */

#include "conio.h"			/* need for getchar prototype */
#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "msgdef.h"
#include "version.h"
#include "mem.h"
#include "ctype.h"
#include "dos.h"

/*---------------------------------------------------------------------------*/

/*
 * mprintf () requires a message number for a stencil, and a message type
 *   from which to retrieve it.  The third argument, "fmt", is interpereted
 *   as being similar to printf... the following is its syntax:
 *      "[%[-][width]{[l]{x/d}/m/c}]*"
 * That is, the following string:
 *   %-ld%15m%d%lx%-20c
 * Tells mprintf to expect 5 replacable parameters:
 *   1: A left-justified integer double-word (long), 8 characters, decimal
 *   2: A right-justified message (which will be retreived), 15 characters
 *   3: A right-justified integer word (short), 8 characters, decimal
 *   4: A right-justified integer double-word (long), 8 characters, hex
 *   5: A left-justified character string, 20 characters
 *
 */

void
mprintf (int msgnum, char *fmt, ... )
{
   char    *ptr;
   int      count = 0, wid;
   long     argt;
   va_list  arg;

   va_start (arg, fmt);

   for (ptr = fmt; ; )
      {
      argt = 0;
      wid  = 0;
      if (*ptr != '%')  break;
      ptr++;
      if (*ptr == '-')                 { argt |= 1; ptr++; }  /* Left-just   */
      for ( ; isdigit (*ptr); ptr++)
         {
	 wid *= 10;  wid += (*ptr - '0');
	 }
      if (*ptr == 'l' || *ptr == 'L')  { argt |=  2; ptr++; } /* Dword       */
      if (*ptr == 'x' || *ptr == 'X')  { argt |=  4; ptr++; } /* Hexadecimal */
      if (*ptr == 'd' || *ptr == 'D')  {             ptr++; } /* Decimal #   */
      if (*ptr == 'c' || *ptr == 'C')  { argt |=  8; ptr++; } /* Character   */
      if (*ptr == 's' || *ptr == 'S')  { argt |=  8; ptr++; } /* (same ^^^)  */
      if (*ptr == 'm' || *ptr == 'M')  { argt |= 16; ptr++; } /* Message     */

      count++;

      sublist[count].size      = Sublist_Length;
      sublist[count].reserved  = Reserved;
      sublist[count].id        = (char)count;
      sublist[count].max_width = (char)wid;
      sublist[count].min_width = (char)wid;

      if (argt & 16)
         {
	 argt |= 8;
	 InRegs.x.ax = va_arg (arg, int);
	 InRegs.h.dh = Utility_Msg_Class;
	 sysgetmsg(&InRegs,&SegRegs,&OutRegs);
	 FP_OFF(sublist[count].value) = OutRegs.x.si;
	 FP_SEG(sublist[count].value) = SegRegs.ds;
	 sublist[count].pad_char = Blank;
         }
      else if (argt & 8)
	 {
	 sublist[count].value    = (unsigned far *)va_arg (arg, char *);
	 sublist[count].pad_char = Blank;
	 }
      else if (argt & 2)
	 {
	 sublist[count].value    = (unsigned far *)va_arg (arg, long *);
	 }
      else
	 {
	 sublist[count].value    = (unsigned far *)va_arg (arg, short *);
	 }

      if (argt & 8)
	 {
	 sublist[count].flags = Char_Field_ASCIIZ;
	 }
      else if (argt & 4)
	 {
	 sublist[count].pad_char = '0';
	 if (argt & 2)  sublist[count].flags = Bin_Hex_DWord;
	 else           sublist[count].flags = Bin_Hex_Word;
	 }
      else
	 {
	 sublist[count].pad_char = Blank;
	 if (argt & 2)  sublist[count].flags = Unsgn_Bin_DWord;
	 else           sublist[count].flags = Unsgn_Bin_Word;
	 }

      sublist[count].flags += (char)((argt & 1) ? Left_Align : Right_Align);
      }

   InRegs.x.ax = msgnum;
   InRegs.x.bx = STDOUT;
   InRegs.x.cx = count;
   InRegs.h.dl = No_Input;
   InRegs.h.dh = Utility_Msg_Class;
   if (count)  InRegs.x.si = (unsigned int)&sublist[1];

   sysdispmsg (&InRegs, &OutRegs);

   if (! NoCR)
      check_screen();
   NoCR = 0;
}

void
mainline          (Address, Name, Size, Type, Region)
int                         Name,       Type;
unsigned long int *Address,      *Size,      *Region;
{
   char Desc[20];
   sprintf (Desc, "(%ldK)", toK(*Size));
   if (*Region == 0L)
      mprintf (MainLineMsg, "%5lx%8ld%7c%-8m%-m", Address, 
               Size, Desc, Name, Type);
   else
      mprintf (MainXLineMsg, "%5lx%3ld%8ld%7c%-8m%-m", Address, Region,
               Size, Desc, Name, Type);
}

void
mainline_a          (Address, Name, Size, Type, Region)
char                         *Name,      *Type;
unsigned long int   *Address,      *Size,      *Region;
{
   char Desc[20];
   sprintf (Desc, "(%ldK)", toK(*Size));
   if (*Region == 0L)
      mprintf (MainLineMsg, "%5lx%8ld%7c%-8c%-c", Address, 
               Size, Desc, Name, Type);
   else
      mprintf (MainXLineMsg, "%5lx%3ld%8ld%7c%-8c%-c", Address, Region,
               Size, Desc, Name, Type);
}

/*---------------------------------------------------------------------------*/

/*
 * check_screen() is called after every message is displayed; if /P was
 * specified, it causes the user to hit a key after every screenful.
 *
 */

void
check_screen ()
{
   if (! PageBreak)  return;
   if (++num_lines >= (PageBreak-1))
      {
      num_lines = 0;
      mprintf (KeyPressMsg, "");
      getch();
      mprintf (NewLineMsg, "");
      num_lines = 0;
      }
}

/* M003 END */
