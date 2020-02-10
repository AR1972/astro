/*** 
* names.h - external decl.s and constants associated with the name manager
*
*	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
*
*******************************************************************************/

#undef NAMES_H
#define NAMES_H ON


/* external declarations */
VOID NEAR SetONamMask(ushort, uchar);
VOID NEAR ResetONamMaskTmp(ushort, uchar);
VOID NEAR SetONamSpace(ushort, uchar);
VOID NEAR CheckONamSpace(ushort, uchar);	/* [7] */
ushort NEAR FlagOfONam(ushort);
ushort NEAR ONamOfPsd(sd *);
ushort NEAR GetVarNamChar(ushort);
ushort FAR CopyONamBd(ushort, bd *);

ushort FAR BdAppendOgNam(bd *, ushort); /* [1] */
ushort FAR OgNamOfPsd (sd *);					/* [1] */
ushort NEAR OgNamOfPbCb(char *, ushort);	/* [1] */
ushort FAR CopyOgNamPb(char *, ushort);	/* [1] */
ushort NEAR ONamOfOgNam(ushort);				/* [1] */
ushort FAR ONamOfOgNamFar(ushort);				/* [8] */
char FAR CharOfOgNam(ushort);					/* [1] */
ushort FAR CbOfOgNam(ushort);					/* [1] */


/* flag bit constants used in the flags word of a name table entry 				*/
#define NM_fShared 0x01		/* bit 0 - TRUE (set) if exists a module SHARED 
											  	  variable of this name							*/
#define NM_fLineNum 0x02	/* bit 1 - TRUE if symbol entry is for a line number
												  this flag can only be set by the NameMgr
													and can never be reset. */
#define NM_fLineNumLabel 0x04
									/* bit 2 - TRUE if symbol entry is either a line
												  number or a label - this bit is used
												  by the text mgr to detect duplicate
												  label definitions. */
#define NM_fAs 0x08			/* bit 3 - TRUE if "x AS" clause exists in module	*/
/* unused:			0x0010	bit 4	*/
/* unused:			0x0020	bit 5	*/

/* CONSIDER: if we run out of bits here, the following enumerated 2 bits would
	CONSIDER: be an ideal candidate for moving in as the high two bits of the
	CONSIDER: NAM_SIZE field (only 6-bits are needed for the size of the largest
	CONSIDER: legal name). A definite cost to this, however, is name table
	CONSIDER: search speed (speed cost of masking the NAM_SIZE field).
									/* enumerated constants for bits 6 & 7					*/
#define NMSP_UNDEFINED	0x00
									/* initial value of this 2-bit field					*/
#define NMSP_Procedure	0x40
									/* passed by txtmgr when it inserts a ref. to a
										prs in the module											*/
#define NMSP_Variable	0x80
									/* set by MakeVariable when creating a variable 
										table entry that's not a constant or a proc		*/
#define NMSP_MASK			0xC0
									/* used to isolate this 2-bit enumerated field		*/

#define CB_MAX_NAMENTRY	255d	/* [3] max. length of a name in the name table.
											[3] do NOT confuse this with CB_IDNAM_MAX!	*/

/* Non-RELEASE and DEBUG name table macros */

#define DbOoNam(oNam) 
#define DbOogNam(ogNam) 			/* [2] */
#define DbOtNam 
#define DbOtgNam 						/* [2] */
#define DbChkoNam(oNam) 
#define DbChkogNam(ogNam) 			/* [2] */
#define DbChktNam 
#define DbChktgNam 					/* [2] */
