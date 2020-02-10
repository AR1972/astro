;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This file has file handling routine to be used for the windows environment ;
; These routines for OPEN,CLOSE,READ and LSEEK are similar to the undocume-  ;
; -nted file i-o routines but will take care of segment crossing and >64k    ;
; bytes i-o. 						                     ;
;									     ;
; History:	  							     ;
;									     ;
;	 Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Adapted for the Dos Task Switcher.				     ;
;									     ;
;        Tue June-20-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created for Windows. (Added the History legend)	   	     ;
;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include macros.mac
	.list


;	The jmpnext macro and associated symbols are used to generate
;	the fall-through chain and generate the labels required for
;	error checking.

??ji	=	0			;;Initial index value

jmpnext macro e
jn %??ji,%(??ji+1),e			;;Set next label
endm

jn macro i,j,e
.sall
??ji&i:
.xall
ifb <e> 				;;If not the end of the chain
	db	03dh			;;mov bx, next two bytes
errn$	??ji&j,+2			;;mext lable must be two bytes away
endif
??ji=j					;;increment counter
endm


sBegin	Data


sEnd 	Data
;----------------------------------------------------------------------------;
sBegin	Code

;----------------------------------------------------------------------------;
; declare all external functions here.					     ;
;----------------------------------------------------------------------------;


;-----------------------------------------------------------------------------;

assumes	cs,Code
assumes ds,Data

;----------------------------------------------------------------------------;
;                  OpnFile (LPSTR,BYTE)			             ;
;----------------------------------------------------------------------------;

cProc	OpnFile,<NEAR,PUBLIC,PASCAL>
	parmD	FileNameString
	parmB	AccessMode

cBegin
	lds	dx,FileNameString	; load the pointer to the string
	assumes	ds,nothing

	mov	al,AccessMode		; type of i-o desired
	mov	ah,3dh			; OPEN FILE MSDOS code
	int	21h			; do int21
	jnc	open_good		; successful
	mov	ax,-1			; error code
open_good:				; go back to caller

cEnd

;----------------------------------------------------------------------------;
;                  CreateFile (LPSTR,BYTE)			             ;
;----------------------------------------------------------------------------;

cProc	CreateFile,<NEAR,PUBLIC,PASCAL>

	parmD	FileNameString
	parmW	Attribute

cBegin
	lds	bx,FileNameString	; load the pointer to the string
	assumes	ds,nothing

	mov	dx,bx			; DS:DX points to ASCIIZ name string
	mov	cx,Attribute		; type of i-o desired
	mov	ah,3ch			; OPEN FILE MSDOS code
	int	21h			; open the file
	jnc	create_good		; successful
	mov	ax,-1			; error code
create_good:				; go back to caller

cEnd
;----------------------------------------------------------------------------;
;                  DeleteFile (LPSTR)				             ;
;----------------------------------------------------------------------------;

cProc	DeleteFile,<NEAR,PUBLIC,PASCAL>
	parmD	FileNameString

cBegin

	lds	bx,FileNameString	; load the pointer to the string
	assumes	ds,nothing

	mov	dx,bx			; DS:DX points to ASCIIZ name string
	mov	ah,41h			; delete file code
	int	21h			; open the file
	jnc	delete_good       	; successful
	mov	ax,-1			; error code
delete_good:				; go back to caller

cEnd
;----------------------------------------------------------------------------;
;                  LseekFile (HANDLE,DWORD,BYTE)			     ;
;----------------------------------------------------------------------------;

cProc	LseekFile,<NEAR,PUBLIC,PASCAL>

	parmW	FileHandle
	parmD	Position
	parmB	Origin

cBegin
	
	mov	cx,seg_Position		; get hiword of offset
	mov	dx,off_Position		; get low word of offset
	mov	bx,FileHandle		; the file handle
	mov	al,Origin
	mov	ah,42h			; lseek code in MSDOS
	int	21h
	jnc	seek_good		; successful
	mov	dx,-1
	mov	ax,-1			; error code
seek_good:

cEnd

;----------------------------------------------------------------------------;
;                  ReadFile (HANDLE,LPSTR,DWORD)		             ;
;----------------------------------------------------------------------------;

cProc	ReadFile,<NEAR,PUBLIC,PASCAL>,<ds>

;	parmW	FileHandle
;	parmD	Buffer
;	parmD	Count

cBegin	nogen

	mov	cl,3fh			; read code
	jmp     short ReadWriteFile	;prepare to merge into FAR proc

cEnd	nogen			   
	
	;--------------------------------------------;
	; near entry point WriteFile.		     ;
	;--------------------------------------------;

cProc	WriteFile,<NEAR,PUBLIC,PASCAL>,<ds>

;	parmW	FileHandle
;	parmD	Buffer
;	parmD	Count

cBegin	nogen


	mov	cl,40h			;write code
	jmp     short ReadWriteFile	;prepare to merge into the FAR proc

cEnd	nogen			   

;----------------------------------------------------------------------------;
;                  ReadWriteFile (HANDLE,LPSTR,DWORD)		             ;
;----------------------------------------------------------------------------;

cProc	ReadWriteFile,<NEAR,PUBLIC,PASCAL>,<ds>

	parmW	FileHandle
	parmD	Buffer
	parmD	Count

	localB	rwcode
	localW	EntrySP		

cBegin	

	
	mov	EntrySP,sp		;save in case we hit an error
	mov 	rwcode,cl
	mov	bx,FileHandle
	mov	di,seg_Count
	mov	cx,off_Count		;DI:CX has the count of bytes to read/write
	lds	si,Buffer		;DS:SI points to buffer
	assumes	ds,nothing

; DI:CX has the number of bytes to read/write
; DS:SI points to next position in buffer

; if we are not at the start of a segment, complete the i/o for the segment

	or	si,si			;start of a segment ?
	jz	iterate			;yes.
	mov	ax,0ffffh		;last offset in segment
	sub	ax,si			
	inc	ax			;no of bytes till end of segment
	or	di,di			;how much do we have to read ?
	jnz	special_io		;lots, do a special io
	cmp	ax,cx			;is no to read less than till seg end ?
	jae	last_io			;one i/o will do

special_io:

	push	cx			;save count
	push	di			;save
	push	ax			;save amount of io being done
	mov	dx,si			;ds:si points to buffer
	mov	cx,ax			;amount to read till segment end
	mov	ah,rwcode		;read or write
	int	21h			;do the io
	call	GetOutOnError		;exit in case of error
	mov	ax,ds			;get the segment
	add	ax,1000h		;next segment
	mov	ds,ax			;next segment
	xor	si,si			; DS:SI points to next segment
	pop	ax			;restore amount of io done
	pop	di
	pop	cx			;restore counts
	sub	cx,ax			;update count left
	sbb	di,0			;update high word



iterate:

; test to see if the read/write can be satisfied in the current segment

	or	di,di			;no left >= 64k
	jnz	will_cross_segment	;have to break up in chunks of 65535
	cmp	cx,0fff0h		
	ja	will_cross_segment

last_io:

; the last read/write will complete the job

	mov	dx,si			; DS:DX has buffer
	mov	ah,rwcode		; read/write code
	int	21h	
	call	GetOutOnError		;exit in case of error
	jmp	short read_ret		; go back

will_cross_segment:

; read one hunk of 65,535 bytes

	push	di
	push	cx			; save the counts

	mov	dx,si			; DS:DX points to buffer
	mov	cx,0fff0h		; read/write 65535 bytes
	mov	ah,rwcode
	int	21h
	call	GetOutOnError		;exit on error

reduce_count:

;----------------------------------------------------------------------------;
; now we need to go to the next segment.				     ;
;----------------------------------------------------------------------------;

	mov	ax,ds			;get the segment
	add	ax,0fffh		;next segment
	mov	ds,ax
	xor	si,si			; DS:SI points to next segment

; get back counts and reduce no of bytes to read/write

	pop	cx
	pop	di

	sub	cx,0fff0h		;we just read 65,535 bytes
	sbb	di,0			;update high word

; DI:CX has bytes left to read/write
; DS:SI has the pointer to the next byte to read/write

	jmp	iterate			; iterate till all bytes are read/write

GetOutOnError proc near

	jc	ExitWithError		;cannot proceed
	cmp	ax,cx			;was the required amout read or written
	jb	ExitWithError		;no.
	ret				;all is OK

GetOutOnError endp

ExitWithError:

; close the file first

	mov	ah,3eh			;BX has the handle
	int	21h			;closes the file
	stc				;error while read/write
	mov	sp,EntrySp		;clean up stack

read_ret:
cEnd

;----------------------------------------------------------------------------;
;		    CloseFile (HANDLE)	         		             ;
;----------------------------------------------------------------------------;

cProc	CloseFile,<NEAR,PUBLIC,PASCAL>
	
 	parmW	FileHandle

cBegin
	
	mov	bx,FileHandle
	mov	ah,3eh			; close code
	int	21h

cEnd
;----------------------------------------------------------------------------;
;		   SetNormalAttributes (LPSTR)				     ;
;----------------------------------------------------------------------------;

cProc	SetNormalAttributes,<NEAR,PUBLIC,PASCAL>,<ds>

;	parmD	lpFileName

cBegin	nogen
 
	mov	cl,0			;normal attributes
	jmpnext

cEnd	nogen
;----------------------------------------------------------------------------;
;		   SetHiddenAttributes (LPSTR)				     ;
;----------------------------------------------------------------------------;


cProc	SetHiddenAttributes,<NEAR,PUBLIC,PASCAL>,<ds>

;	parmD	lpFileName

cBegin	nogen

	mov	cl,3			;hidden and readonly attributes
	jmpnext	stop

cEnd	nogen
;----------------------------------------------------------------------------;
;		SetAttributes (LPSTR)					     ;
;----------------------------------------------------------------------------;

cProc	SetAttributes,<NEAR,PUBLIC,PASCAL>,<ds>

	parmD	lpFileName

cBegin

	mov	ds,seg_lpFileName	;load the seg of the pointer
	mov	dx,off_lpFileName	;ds:dx points to file name
	mov	ax,4301h		;set file attributes function code
	xor	ch,ch			;cl has bits to set
	int	21h			;file attributes set

cEnd
;----------------------------------------------------------------------------;

sEnd  	Code

end

