;	TITLE	FMAKERES - Command entry point for MSHERC.
;***
;FMAKERES
;
;	Copyright <C> 1987 - 1990, Microsoft Corporation
;
;Purpose:
;	Assumes control upon entry of MSHERC.COM to initialize the driver
;	and install the INT 10 hook.  Then terminate and stay resident.
;
;******************************************************************************

	include	hgcdefs.inc

code            segment para public 'code'
                assume  cs:code,ds:code

Public   Save_Old_Int10h

Extrn   Start_Of_New_Routine:word	;offset of start of HGC INT 10H
Extrn   Old_Int10h_Routine:dword	;old INT 10H address
Extrn	GetHGCStatus:near
Extrn	ConfigMode:byte			;[1] FULL or HALF

Save_Old_Int10h:

	; Parse the command line first, so that we can display
	; help if it is needed.

	cld				;[2] make sure scanning forward
	mov	si,81h			;[2] start at begining of Cmd Line
GetNextByte:				;[2]
	lodsb				;[2] Grab a character
	cmp	al,' '			;[2] is it a space?
	je	GetNextByte		;[2] brif so, this is valid seperator
	cmp	al,9			;[2] is it a tab?
	je	GetNextByte		;[2] brif so, also a valid seperator
	cmp	al,0dh			;[2] cmd line terminator?
	je	Parsed			;[2] brif so, done parsing
	cmp	al,'/'			;[2] is this a switch char?
	jne	GiveHelp		;[2] brif not, give help
	lodsb				;[2] get switch character
	and	al,0DFh 		;[2] convert to upper case
	mov	[ConfigMode],HALF	;[2] assume it is /H
	cmp	al,'H'			;[2] is it /H
	je	SwitchEnd		;[2] brif so, parsed successful

GiveHelp:
	mov	dx, offset HelpMsg	;[2] point to the help message
	jmp	short DontLoadIt	;[2] go print it and exit


	; Skip to the end of the current switch.  We could also
	; make sure that it is /HALF or some subset of it, but
	; that would be inconsistent with previous versions and
	; would probably take more code.

SwitchEnd:				;[2]
	lodsb				;[2] get next character
	cmp	al,0dh			;[2] end of cmd line?
	je	Parsed			;[2] brif so, end of switch
	cmp	al,' '			;[2] seperator character?
	je	GetNextByte		;[2] brif so, end of switch
	cmp	al,9			;[2] seperator character
	je	GetNextByte		;[2] brif so, end of switch
	jmp	short SwitchEnd 	;[2] keep looking for end of switch

Parsed:					;[2]

;first confirm that our INT 10h has not already been loaded
	mov	dl,-1			;DL will not change if no driver
	mov	ah,InquireFuncNo	;test for Hercules INT10 driver
	int	10H
	cmp	dl,-1			;DL returns -1 if no driver
	mov	dx,offset already	;Fetch offset of string
	jne	DontLoadIt		;there is driver, don't load it
	call	GetHGCStatus		;status request
	cmp	dl,-1			;DL returns -1 if no HGC card
	jne	LoadIt			;there is an HGC, go load it
	mov	dx,offset noHGC 	;Fetch offset of string

;announce that int 10h has already been installed
DontLoadIt:
	mov	ah,9			;print string DOS function
	int	21H
	xor	ax,ax			;terminate
	int	20H
	
;------- save the original interrupt routine
LoadIt:
	 push	es
         xor    ax,ax
         mov    es,ax
         mov    bx, es:[INT10HVECTOR_OFFSET]
         mov    word ptr Old_Int10h_Routine,bx
         mov    bx, es:[INT10HVECTOR_SEGMENT]
         mov    word ptr Old_Int10h_Routine[2],bx

;------- point to this interrupt routine

         cli
         mov    word ptr es:[INT10HVECTOR_OFFSET],offset Start_Of_New_Routine
         mov    es:[INT10HVECTOR_SEGMENT],cs
	 sti
	 pop	es

;------ Write the HGC INT 10H Message------
	mov	ah,9
	mov	dx,offset sign_on	;Fetch offset of string
	int	21H			;Write the string

;------- make this routine resident

         mov     dx, offset Save_Old_Int10h
         mov     cl,4
         shr     dx,cl
         inc     dx
         mov     ax,3100h
         int     21h

sign_on db	'Hercules Resident Video Support Routines. Version 1.12'
	db	0DH,0AH,'$'
already db	'Hercules Video Support Routines are already installed.'
	db	0DH,0AH,'$'
noHGC	db	'Hercules Video Card not present.',0DH,0AH
	db	'Hercules Video Support Routines not installed.'
	db	0DH,0AH,'$'
HelpMsg db	'Install MSHERC.COM before running programs with graphics requiring', 0DH, 0AH
	db	'the Hercules graphics card.', 0DH, 0AH, 0DH, 0AH
	db	'MSHERC [/HALF]', 0DH, 0AH
	db	'  /HALF Required when a color adapter is also installed.', 0DH, 0AH, '$'
code	ends
	end
