;***
;intmac.inc - 20-Mar-86
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

;       8086 Interrupt Handling Macros
;       MS-DOS 1.0 does not have INT 21 to GET a vector
;       hence SAVINT just moves interrupts around.

	SVINT	MACRO   savloc,intloc,reg
	IFB	<reg>
	SVINT	savloc,intloc,AX
	ELSE
	MOV	reg,intloc
	MOV	savloc,reg
	MOV	reg,intloc+2
	MOV	savloc+2,reg
	ENDIF
	ENDM

	savint	macro savloc,intvec 
	MOV	AX,3500H+INTVEC/4 ;;AH=get int - AL=interrupt number
	int	21h
	mov	word ptr savloc,bx ;;savloc = offset
	mov	word ptr savloc+2,es ;;savloc+2 = segment
	endm

;	The following macros use the MS-DOS INT 21 call to change
;	interrupt vectors.

setvec	macro	interrupt,offset_adr ;;set interrupt vector function call
	mov	dx,offset offset_adr
	MOV	AX,2500H+INTERRUPT ;;AH=set int call - AL=interrupt
	int	21h
	endm

rstvec	macro	interrupt,save_adr ;;restore interrupt from saved
	lds	dx,dword ptr save_adr
	MOV	AX,2500H+INTERRUPT ;;AH=set int call - AL=interrupt
	int	21h
	endm

XFRINT	MACRO	TOINT,FROMINT	;;move FROMINT to TOINT
	PUSH	ES		;;save register...
	MOV	AX,3500H+FROMINT ;;want to get FROMINT
	INT	21H		;;vector now in ES:BX
	MOV	AX,ES		;;keep segment around
	POP	ES		;;restore register...
	MOV	DX,BX		;;move offset for put
	PUSH	DS		;;save register...
	MOV	DS,AX		;;segment for put
	MOV	AX,2500H+TOINT	;;want to put TOINT
	INT	21H		;;vector now in DS:DX
	POP	DS		;;restore register
	ENDM

	.LIST
