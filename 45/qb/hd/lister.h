/* File: lister.h - Defines which apply to the QBI Lister               */
/* NOTE: When making changes to this file, be sure to make equivalent   */
/*       changes to file LISTER.INC                                     */

#undef LISTER_H
#define LISTER_H ON        /* remember that this file has been included */

/**======================================================================**
 **==           External Interface to Lister Component                 ==**
 **==           This file is included by non-lister-component modules  ==**
 **======================================================================**/

ushort FAR ListLine(ushort, bd *);

extern ushort otxListNext;
   /* text offset to opBol for next line to be listed by ListLine().
      Set by ListLine on exit. */
extern ushort otxFindStmt;	/* See ListLine header for description */
extern ushort colFindStmt;	/* See ListLine header for description */
extern ushort colFindStmtEnd;	/* See ListLine header for description */

