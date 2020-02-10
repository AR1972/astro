	TITLE	ssdata	- Scanner specific data declarations
;***
;ssdata - Scanner specific data declarations
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module contains scanner specific data declarations only.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
SSDATA_ASM = ON
	.list


assumes ds, DATA
assumes es, NOTHING
assumes SS, DATA

sBegin	CODE
foo	label	byte
sEnd	CODE


subttl	Static data area definitons.
sBegin	DATA

public	SegCode
SegCode		dw	seg foo

public	SsErrOTx
SsErrOTx	DW 0		;Text offset of pcode which was replaced by opEot

public	SsErr
SsErr		DW 0		;Error code to be returned by scanner.

public	SsErrOpcode
SsErrOpcode	DW 0		;Opcode which was replaced by opEot

public	SsDelayErr,SsDelayLoc,SsDelayCnt
SsDelayErr	dw 0		;Error code of delayed error
SsDelayLoc	dw 0		;oTx of delayed error
SsDelayCnt	dw 0		;Count of pending delayed errors

public	ScanRet
ScanRet 	DW 0		;Scan loop return address

	public	f_Static	
f_Static	DB 0		;TRUE if $STATIC in effect

	public	f_StaticCalc	
f_StaticCalc	DB 0		;Move to temporary for calcualtion

public	SsExec,SsExecFlag,SsExecTmp
SsExec	label	word		;Flags below referenced as one word
SsExecFlag	DB 0		;OPA_fExecute ORed in if can't allow COMMON
SsExecTmp	DB 0		;Temporary hold for SsExecFlag during CONST

public	ScannerFlags,SsBosFlags,SsFlags
ScannerFlags	label	word	;Flags below referenced as one word
SsBosFlags	DB 0		;Scanner begin of statement flags
SsFlags 	DB 0		;General scanner flags

	public	SsOTxPatchBos	
SsOTxPatchBos	DW 0		; Address of word to be patched at next Bos

	public	SsOTxStart	
SsOTxStart	DW 0		; Address of point where stack is empty

public	SsOTxBOS
SsOTxBOS	DW 0		;oTx for BOS for statement being scanned

public	SsCbTxExpand
SsCbTxExpand	DW 0		;Count of bytes by which the text table has expanded.

public	SsLinkCtl
SsLinkCtl	DW 0		;Address of Label control structure

public	SsCbFrameTemp
SsCbFrameTemp	DW 0		;Bytes of temp space for current statement

public	SsStackSave
SsStackSave	DW 0		;Save location for sp from start of scan

public	SsLineCount
SsLineCount	DW  0		;No. of lines scanned

;Descan uses these to keep track of return address on the stack
public	SsNextOTx,SsReturnBp
SsNextOTx	DW 0		;oTx of return address
SsReturnBp	DW 0		;offset in stack of return frame

;ScanAndExec needs the following
public	SsScanExStart,SsScanExSrc
SsScanExStart	DW 0		;Starting address of execution
SsScanExSrc	DW 0		;oTx of original source

	;Ss_StCall uses these to count parameter bytes for CDECL calls

	public	SsCbParmCur,SsParmCnt
SsCbParmCur	dw 0		;cb of all params so far
SsParmCnt	dw 0		;Number of parameters

public	SsOtxHeapMove
SsOtxHeapMove	dw 0		;oTx of last possible heap movement


public	SsBosStack
SsBosStack	dw 0		;SP-2 at BOS


sEnd	DATA
end
