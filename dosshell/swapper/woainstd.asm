;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This file will hold all fuctions necessary for doing instance data         ;
; management.								     ;
;									     ;
; History:								     ;
;									     ;
;        Mon Sept-17-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created. (Added the History legend)      			     ;
;----------------------------------------------------------------------------;


	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include macros.mac
	include	njmp.mac
	include woaswapi.inc
	.list

;----------------------------------------------------------------------------;
; declare any public function or varaiable names here.			     ;
;----------------------------------------------------------------------------;


createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin	StubSeg

	assumes	cs,StubSeg
	assumes	ds,StubSeg

;----------------------------------------------------------------------------;
; define structures and constants needed for the instance data calls.	     ;
;----------------------------------------------------------------------------;

Instance_Info_Struc STRUC

SIS_Version		db	3,0
SIS_Next_Dev_Ptr	dd	?
SIS_FILLER		db	8 dup (?)
SIS_Instance_Data_Ptr	dd	?

Instance_Info_Struc ENDS

Instance_Item_Struc STRUC

IIS_Ptr			dd	?
IIS_Size		dw	?

Instance_Item_Struc ENDS


;----------------------------------------------------------------------------;
; define the external function calls.		          		     ;
;----------------------------------------------------------------------------;

	;-------------------------------------------------------;
	; external OLDAPP procedures.			        ;
	;-------------------------------------------------------;

 	externNP	SwitchAPICallIn		;(WOASWAPI.ASM)

;----------------------------------------------------------------------------;
; declare the variables which are defined elsewhere.			     ;
;----------------------------------------------------------------------------;


;----------------------------------------------------------------------------;
; define variables.							     ;
;----------------------------------------------------------------------------;

GlobalW	InstDataSeg,0			;segment for instance data management

InstDataBlockPtr	dd	?	;pointer to instance data block nodes

;----------------------------------------------------------------------------;
; GetSizeOfInstanceBuffer:						     ;
;									     ;
; This routine makes the 'InstanceData' INT 2Fh call and returns the size of ;
; the buffer that will be needed to hold all the instanece data. For each    ;
; block of instance data we will need an additional 6 bytes to store the     ;
; address and size of the block.				             ;
;									     ;
; Entry:								     ;
;	 None.								     ;
; Exit:									     ;
;	 AX - size in paras of buffer needed for instance data management.   ;
;	      (InstDataSeg reset to 0 if AX is retruned as 0).		     ;
; Uses:									     ;
;	 All but ES,DS,BP.						     ;
;----------------------------------------------------------------------------;

cProc	GetSizeOfInstanceBuffer,<NEAR,PUBLIC,PASCAL>,<es,bx,cx,dx,si,di>

cBegin

; set up registers for the int 2fh call.

	mov	ax,SWAPI_GET_INST_DATA	;code for the call
	xor	bx,bx			;need lots of zeros.
	mov	es,bx			;es:bx = 0:0
	mov	cx,cs			;cx:dx = call in address of ours.
	mov	dx,StubSegOFFSET SwitchAPICallIn
	int	2fh			;make the call.

; save the block address.

	mov	wptr [InstDataBlockPtr+2],es
	mov	wptr [InstDataBlockPtr],bx

; walk all the nodes and the instance data subnodes accumulating the node sizes
; remember we need an extra 6 bytes for each data block.

; Also note that the app's IDT will be saved as a part of this block. Set up
; AX to the size needed for the IDT area.

	mov	ax,SIZE Instance_Item_Struc + 256 * 4 

InstanceDataSizeMainLoop:

	mov	cx,es			;is the pointer 0 ?
	or	cx,bx
	jz	InstanceDataSizeObtained;yes, AX has the size.

; load the pointer to the instance data descriptor block.

	pushem	es,bx			;save
	les	bx,es:[bx.SIS_Instance_Data_Ptr]
	mov	cx,es			;is the pointer 0 ?
	or	cx,bx
	jz	NextInstBlockNode	;yes, go to the next block.

InstanceDataSizeSubLoop:

	mov	cx,wptr es:[bx.IIS_Ptr]	;is this the terminator ?
	or	cx,wptr es:[bx.IIS_Ptr.2]
	jz	NextInstBlockNode	;yes.

; accumulate the size.

	add	ax,es:[bx.IIS_Size]	;include the size
	add	ax,SIZE Instance_Item_Struc;for the header

; point ES:BX to the next node in the array.

	add	bx,SIZE Instance_Item_Struc
	jmp	short InstanceDataSizeSubLoop

NextInstBlockNode:

	popem	es,bx			;restore pointer to current node.
	les	bx,es:[bx.SIS_Next_Dev_Ptr]
	jmp	short InstanceDataSizeMainLoop

InstanceDataSizeObtained:

	add	ax,SIZE Instance_Item_Struc;for the terminator
	add	ax,15			;for para alignment
	shiftr	ax,4			;number of paras

GSOIB_Ret:

; AX has the para size of the buffer needed for instance data management.

cEnd
;----------------------------------------------------------------------------;
; GetInstanceDataSnapShot:						     ;
; 									     ;
; This routine prepares the InstanceData buffer by taking a snap shot of the ;
; buffer before starting up the app.					     ;
;									     ;
; The address of the buffer is InstDataSeg:0, and  InstanceDataBlockPtr is   ;
; assumed to still point to the linked list of Instance_Info structures.     ;
;									     ;
; Entry:								     ;
;	 InstDataSeg - valid segment.					     ;
;	 InstanceDataBlockPtr - points to linked list of Instance_Info blocks;
;									     ;
; Exit:									     ;
;	InstDataSeg - will have a snap shot of instance areas. Each instance ;
;		      area is preeceeded by a 4 byte address and a 2 byte    ;
;		      size.						     ;
;									     ;
; USES: all except ES,DS,BP.						     ;
;----------------------------------------------------------------------------;
cProc	GetInstanceDataSnapShot,<NEAR,PUBLIC,PASCAL>,<es>

cBegin
	
	push	ds			;save
	mov	es,InstDataSeg		;get the segment for the save area
	xor	di,di			;es:di points to save area.

; set up the first node for the IDT.

	xor	si,si			;IDT address = 0:0
	mov	wptr es:[di.IIS_Ptr],si	;set address to 0:0
	mov	wptr es:[di.IIS_Ptr.2],si
	mov	es:[di.IIS_Size],256*4	;size of IDT
	add	di,SIZE Instance_Item_Struc

; copy out the IDT.

	push	ds			;save
	assumes	ds,nothing
	mov	ds,si			;ds:si points to IDT start
	mov	cx,256*4/2		;number of words to copy
	cld				;set proper direction
	rep	movsw			;global IDT saved
	pop	ds			;restore
	assumes	ds,StubSeg

; now take snap shots of the other areas.

	lds	si,InstDataBlockPtr	;ds:si points to Instance_Info structure
	assumes	ds,nothing

GIDSS_MainLoop:

	mov	cx,ds			;is the pointer 0 ?
	or	cx,si
	jz	GIDSS_StoreTerminator	;yes we are done.

; load the pointer to the instance data descriptor block.

	pushem	ds,si			;save
	lds	si,[si.SIS_Instance_Data_Ptr]
	mov	cx,ds			;is the pointer 0 ?
	or	cx,si
	jz	GIDSS_NextInstBlockNode	;yes, go to the next block.

GIDSS_SubLoop:

; check for termination sub-node.

	mov	cx,wptr [si.IIS_Ptr]	;is this the terminator ?
	or	cx,wptr [si.IIS_Ptr.2]
	jz	GIDSS_NextInstBlockNode	;yes.

; copy the header part over.

	mov	cx,SIZE Instance_Item_Struc
	push	si			;save
	cld				;clear direction flag
	rep	movsb			;copy the header over
	pop	si			;back to the start

; copy the current block image.

	pushem	ds,si			;save
	mov	cx,[si.IIS_Size]	;size of the block.
	lds	si,[si.IIS_Ptr]		;address of the block.
	cld				;clear direction flag
	rep	movsb			;block is copied.
	popem	ds,si			;restore pointer to sub node

; point DS:SI to the next node in the array.

	add	si,SIZE Instance_Item_Struc
	jmp	short GIDSS_SubLoop

GIDSS_NextInstBlockNode:

	popem	ds,si			;restore pointer to current node.
	lds	si,[si.SIS_Next_Dev_Ptr]
	jmp	short GIDSS_MainLoop

GIDSS_StoreTerminator:

	xor	ax,ax			;for the terminator
	cld				;set proper direction.
	stosw				;first word of terminator
	stosw				;next word of terminator
	stosw				;for the size
	.errnz SIZE Instance_Item_Struc - 6

	pop	ds			;restore
	assumes	ds,StubSeg

GIDS_Ret:

cEnd
;----------------------------------------------------------------------------;
; SwapInstanceDataBlocks:						     ;
;									     ;
; This routine swaps the current contents of the instance data area with     ;
; what is saved in the local buffer.					     ;
;									     ;
; Entry:							             ;
;        InstDataSeg - has a copy of the saved instance area with each block ;
;		       of data being preeceeded by 4 bytes of address and a  ;
;		       word of size.					     ;
; Exit:									     ;
;	 Swaps the current and saved copies of instance data areas.	     ;
;									     ;
; Uses:  all but ES,DS,BP. Interrupts disabled during the Swap.	     	     ;
;----------------------------------------------------------------------------;
cProc	SwapInstanceDataBlocks,<NEAR,PUBLIC,PASCAL>,<es>

cBegin

	cmp	InstDataSeg,0		;any instance data area ?
	jz	SIDB_Ret		;no.

	pushf				;save interrupt flag bit
	cli				;trun off interrupts.
	push	ds			;save
	mov	si,InstDataSeg		;address of saved buffer
	mov	ds,si
	assumes	ds,nothing
	xor	si,si			;DS:SI points to start of saved area

SIDB_NodeWalkLoop:

	les	di,[si.IIS_Ptr]		;pointer to first data area

; test for termination.	For termination all the bytes in the header must be
; 0 since we include the IDT as a part of the swap area.

	mov	cx,es			;is es:bx 0:0 & size = 0?
	or	cx,di
	or	cx,[si.IIS_Size]
	jz	SIDB_SwapDone		;yes, we are done.

; swap the block.

	mov	cx,[si.IIS_Size]	;get the size of the block

; position DS:SI to the start of the current saved area

	add	si,SIZE Instance_Item_Struc

SIDB_SwapLoop:

	mov	al,[si]			;get a byte
	xchg	es:[di],al		;swap the saved copy
	mov	[si],al			;restore the saved copy
	inc	si			;next byte in source
	inc	di			;next byte in destination
	loop	SIDB_SwapLoop		;swap the complete block

; progress to the next block. DS:SI is already pointing to the header of the
; next instance block.

	jmp	short SIDB_NodeWalkLoop	;continue

SIDB_SwapDone:

	pop	ds			;restore
	assumes	ds,StubSeg
	popf				;restore saved interrupt state

SIDB_Ret:

cEnd
;----------------------------------------------------------------------------;

sEnd StubSeg
end


	





	




