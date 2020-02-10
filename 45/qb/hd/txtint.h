/* File: txtint.h - Defines which apply to the QBI Text Manager         */
/* NOTE: When making changes to this file, be sure to make equivalent   */
/*       changes to file TXTINT.INC                                     */

#undef TXTINT_H
#define TXTINT_H ON        /* remember that this file has been included */

/**======================================================================**
 **==           Internal Interface to Text Manager Component           ==**
 **======================================================================**/

ulong NEAR EtDiff(char *, char *, ushort);
ushort NEAR GetPrsField(ushort, ushort);
boolean NEAR LoadEnterProc(ushort);
boolean NEAR LoadExitProc(void);
void NEAR UpdateLinks(ushort, ushort);
void NEAR TxtFlushCache(void);
boolean NEAR TxtDelete(ushort, ushort);

