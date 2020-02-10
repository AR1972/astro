/* File: util.h - Forward references & constants for module util.asm.   */
/* NOTE: When making changes to this file, be sure to make equivalent	*/
/*			changes to file util.inc													*/

#undef UTIL_H
#define UTIL_H ON				/* to prevent duplicate #include's */

VOID FAR ZeroFill(char *, ushort);
VOID FAR FillUndef(ushort *, ushort);
VOID FAR CopyBlk(char *, char *, ushort);
ushort FAR CbSz(char *);
