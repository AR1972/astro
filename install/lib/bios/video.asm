; ========================================================

COMMENT #

	VIDEO.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	Video initialization function. This function
	must be called before any of the other vio
	library functions.


	johnhe - 03/03/89

END COMMENT #

;========================================================

include	VIDEO.INC
include	MODEL.INC

;========================================================

;
; These display values MUST match the manifest constants in include\bios_io.h!
;
MONO        EQU     0
CGA         EQU     1
EGA         EQU     2
EGA_MONO    EQU     3
VGA         EQU     4
VGA_MONO    EQU     5
HERC        EQU     6
MCGA        EQU     7
GAD_8514    EQU     8
ifdef JAPAN
JEGA        EQU     9
endif

HercStatusPort       equ   03BAH       ; Hercules status port
idmask_112           equ   00110000B   ; ID mask for GB112
idcode_112           equ   00010000B   ; ID code for GB112

idmask_222           equ   01110000B   ; ID mask for GB222
idcode_222           equ   01010000B   ; ID code for GB222

IO_Delay macro
         jmp       $+2
         jmp       $+2
endm

;========================================================

SCREEN   SEGMENT AT 0B800H
SCREEN   ENDS

;========================================================

BIOS_DATA SEGMENT AT 40h
	ORG	87h

CursorEmulation	db	?

BIOS_DATA ENDS

;========================================================

.DATA

;========================================================

PUBLIC  ScreenBuffer, ScreenWidth, ScreenLength
PUBLIC  DisplayType, VideoAddress, ScreenPage

VideoAddress	dd	0b800000h	; Default to mono

BackGrColor	db	00	; Default background color
TextColor	db	07	; Default text color
ReverseText	db	00	; Default reverse text color
ReverseBkGr	db	07	; Default reverse background color
ScreenColor	db	07	; Current attribute

ScreenBuffer	dw	?	; Address of display buffer
ScreenWidth	db	80	; Current screen width, columns
ScreenLength	db	25	; Current number of rows on screen
ScreenMode	db	3	; Current video mode
ScreenPage	db	0	; Current video page

CursorSize	dw	?	; Current cursor size

;; Next three lines are for VideoSaveMode and VideoRestoreMode
SaveScreenMode	db	3	; Saved video mode
SaveScreenRows	db	25	; Saved number of rows on screen
SaveCursorSize	dw	?	; Save cursor size
EmulationSave	db	?	; Cursor emulation value for EGA

DisplayType	db	0	; 0 = MONO
				; 1 = CGA
				; 2 = EGA
				; 3 = EGA MONO
				; 4 = VGA
				; 5 = VGA MONO
				; 6 = HERC
				; 7 = MCGA
                            ; 8 = 8514

MonitorType	db	0	; 0 == mono and 1 == color

IsBadATI	db	0	; m112

;========================================================

.CODE

;========================================================
; Simple video detection which only looks at the
; current video mode to detect either CGA or MDA.
;========================================================

MinVideoInitial PROC			; M100 minimum video detection

	mov	AH,0fh
	int	10h
	cmp	AL,07			; See if monochrome video mode
	je	IsMDA			; If it is return not CGA

IsCGA:
	mov	DisplayType,CGA
	mov	MonitorType,1
	mov	AX,03
	jmp	SHORT SetDefaultVideoMode

IsMDA:
	mov	DisplayType,MONO
	mov	MonitorType,0
	mov	AX,07

SetDefaultVideoMode:
	mov	AH,SET_VIDEO_MODE
	int	10h
	mov	AX,SET_VIDEO_PAGE SHL 8	; Mov AH value and AL 0
	int	10h			; Set video page to 0

	call_M	<VideoNormalCurs>	; Make a normal cursor
	call_M	<VideoGetMode>		; Save all video info

	ret

MinVideoInitial	ENDP			; M100 - end of this change

;========================================================
; void	VideoInitial( void );
; Initializes the video data, must be called before any
; other video function. Will also set screen to 25 lines
; if EGA or VGA and set page to 1 for color systems.
;========================================================

VideoInitial	PROC USES ES SI

	mov	MonitorType,0

Step1:
	mov	AX,1a00h		; read display combination code
	int	10h
	cmp	AL,1ah			; function supported ?
	jne	Step2			; No, Try other tests.

		; BL contains active display code. I have however, seen that
		; on some vga cards this call will return the active display
		; in BH. For this reason need to chk BL for zero, if BL is
		; zero we place BH into BL and assume that the only display
		; attached to the system is the active display.

	or	BL,BL			; Do we have an active display ?
	jnz	ChkEga			; Yes, then continue on normally.
	mov	BL,BH			; No.

ChkEga:
	mov	SI,EGA			; Assume it's EGA
	cmp	BL,4			; Chk for EGA color
	je	GotColorVideoType

	inc	SI			; Now assume EGA mono
	cmp	BL,5			; Chk for EGA mono
	je	GotVideoType

ChkVga:
	inc	SI			; Not EGA mono so assume VGA color
	cmp	BL,8			; Chk for VGA color
	je	TestFor8514

	inc	SI			; Not VGA color so assume VGA MONO
	cmp	BL,7			; Chk for VGA mono
	je	TestFor8514
	jmp	SHORT McgaCheck

TestFor8514:
    ;
    ; Before testing for 8514 lets make sure there is no Arcnet
    ; Card at I/O address 2e8h which would be reset by the 8514
    ; detection.
    ;
    call    ArcnetCardPresent
    or      ax, ax
    jnz     Test_ATI

    call	NEAR PTR Detect8514	; Adjust for 8514

    cmp     si, GAD_8514
	je	GotColorVideoType

		; m112  At this point we've detected VGA so now
		; m112  check for ATI Wonder Card or ATI VIP Card
Test_ATI:
	call	ATI_Check		; m112 Return AX == 1 if bad card
	mov	IsBadATI,AL		; m112 else AX == 0

	cmp	SI,VGA			; Decide if Mono or Color display
	je	GotColorVideoType
	jmp	SHORT GotVideoType

McgaCheck:
	mov	SI,MCGA
	mov	AL,BL
	cmp	AL,0ah			; Test for digital color
	je	GotColorVideoType
	cmp	AL,0bh			; Test for analog mono
	je	GotVideoType
	cmp	AL,0ch			; Test for analog color
	je	GotColorVideoType

ChkCga:
	mov	SI,CGA			; Assume it may be CGA
	cmp	BL,1			; Make sure it's not mono
	jne	SHORT GotColorVideoType

	mov	SI,MONO			; Must now assume it's mono
	jmp	SHORT GotVideoType

Step2:
	mov	SI,EGA			; First assume EGA
	mov	AH, 12h
	mov	BL, 10h			; Get EGA info
	int	10h
	cmp	BL, 10h
	je	Step3			; Not an EGA
	or	BH,BH			; If BH==0 then EGA color system
	jz	GotColorVideoType
	inc	SI
	jmp	SHORT GotVideoType	; Else BH==1 and EGA_MONO system

Step3:
	mov	SI,HERC			; Must check for HERC before CGA
	call	NEAR PTR HercCheck	; because a herc will pass the
	or	AX,AX			; the CGA test and not be detected
	jnz	GotVideoType

	call	NEAR PTR CgaCheck	; Returns true if SI is correct
	or	AX,AX
	jnz	GotColorVideoType

	xor	SI,SI			; Must be MONO or HERC
	jmp	SHORT GotVideoType
	

GotColorVideoType:
	inc	MonitorType		; Signal to use colors

GotVideoType:
	mov	AX,SI
	mov	DisplayType,AL		; Save the working display type

SetMode:
	mov	AL,07			; Assume Mono
	or	SI,SI			; Is this a mono system
	jz	DoInt
	cmp	SI,HERC			; Herc also uses mode 7
	je	DoInt

	cmp	SI,CGA
	je	SetCO80
					; Will only get here is EGA or VGA
	cmp	SI,VGA
	je	SetVgaScan
    cmp     SI,GAD_8514
	je	SetVgaScan
	cmp	SI,VGA_MONO
	je	SetVgaScan

SetEgaScan:
	mov	AH,12h			; Get video EGA configuration
	mov	BL,10h
	int	10h
	test	BL,11b			; See if only 64K of video memory
	jz	FakeCga

TestSwitches:
	cmp	CL,8
	je	FakeCga			; I002
	cmp	CL,7			; I002 CGA Display on EGA w/256k memory?
	jne	DisableCursorEmulation	; I002

FakeCga:
	mov	SI,CGA			; Since only 64K assume CGA system
	jmp	SHORT SetCO80

DisableCursorEmulation:
	mov	AX,BIOS_DATA		; Need to update bit 1 of 40h:87h
	mov	ES,AX
	ASSUME	ES:BIOS_DATA

	mov	AL,ES:CursorEmulation
	mov	EmulationSave,AL
	and	EmulationSave,1		; Mask off emulation bit
	
	and	AL,0feh			; Disable ROM BIOS cursor emulation
	mov	ES:CursorEmulation,AL	; Replace the updated value
	mov	AX,1201h		; Set scan lines to 350
	jmp	SHORT SetScanLines

SetVgaScan:
	cmp	IsBadATI,0		; m112 See if bad ATI card
	jnz	SetCO80			; m112 If ATI don't set scan lines

	mov	AX,1200h		; AL == disable cursor emulation
	mov	BL,34h			; BL == Cursor emulation function
	int	10h
	mov	AX,1202h		; Set scan lines to 400

SetScanLines:
	mov	BL,30h			; This sets the scan lines but we
	int	10h			; have to fall thru and do a video
					; set mode for it to take effect
SetCO80:
	mov	AL,03			; Set for color 80 columns text

DoInt:
	mov	AH,SET_VIDEO_MODE
	int	10h

	mov	AX,1111h		; If EGA load 8x14 character set
	cmp	SI,EGA
	je	SetFont
	cmp	SI,EGA_MONO
	je	SetFont

	cmp	IsBadATI,0		; m112 See if bad ATI card
	jnz	SetPage			; m112 If ATI don't set scan lines

	mov	AX,1114h		; If VGA load 8x16 character set
	cmp	SI,VGA
	je	SetFont
	cmp	SI,GAD_8514
	je	SetFont
	cmp	SI,VGA_MONO
	jne	SetPage

SetFont:
	xor	BL,BL
	int	10h

SetPage:
	mov	AX,SET_VIDEO_PAGE SHL 8	; Mov AH value and AL 0
	int	10h			; Set video page to 0


SetCursorType:
	cmp	SI,EGA_MONO		; Final check to see if we have
	je	@f			; EGA or VGA mono and need to
	cmp	SI,VGA_MONO		; use mono text attributes
	jne	CursorSet
@@:
	mov	MonitorType,0		; 

CursorSet:
	call_M	<VideoNormalCurs>	; Make a normal cursor
	call_M	<VideoGetMode>		; Save all video info
	ret

VideoInitial ENDP

;***************************************************************************
;*
;* ArcnetCardPresent - Detects the presence of an Arcnet Card
;*
;* ENTRY: None.
;*
;* EXIT: Boolean
;*       AX = 1 , Arcnet Card Probably present
;*       AX = 0 , Arcnet Card Probably NOT Present
;*
;***************************************************************************
ArcnetCardPresent PROC NEAR USES SI
LOCAL InputBytes[4]:BYTE
LOCAL ACP_First:BYTE

      ; Read a BYTE from four different 16-bit I/O addresses that have
      ; the same lower 10 bits (2E0h).  2E0h is the address of the Status
      ; register for the Arcnet Cards that tend to be reset easily
      ; (a Read or write to I/O addresses 2E8, 2E9, 2EA, OR 2EB).
      ; Store the BYTE in InputBytes
      ;
      mov   dx, 56E0h
      IO_Delay
      in    al, dx
      mov   InputBytes, al

      mov   dx, 0AAE0h
      IO_Delay
      in    al, dx
      mov   InputBytes+1, al

      mov   dx, 0E2E0h
      IO_Delay
      in    al, dx
      mov   InputBytes+2, al

      mov   dx, 1EE0h
      IO_Delay
      in    al, dx
      mov   InputBytes+3, al

      xor   si, si                  ; Clear the loop counter

ACP_Loop:
      mov   al, [InputBytes+si]     ; Read a byte from table

      cmp   al, 0FFh                ; If it is FF or E0 (low Byte of I/O
      je    NoArcNet                ;  address we will assume that we read
                                    ;  air.
      cmp   al, 0E0h
      je    NoArcNet

      and   al, 78h                 ; bits 6 through 3 (0XXX X000) should
                                    ;  be constant

      cmp   si, 0h                  ; Is it first byte read ?
      je    ACP_First_Pass

      cmp   ACP_First, al           ; is it equal to the first Byte read
      jne   NoArcNet

      inc   si                      ; point to next byte in table
      cmp   si, 4d                  ; have we done all four bytes
      je    ArcNetFound             ; Yes, we have found an Arcnet Card

      jmp   ACP_Loop

ACP_First_Pass:
      mov   ACP_First, al           
      inc   si                      ; point to next byte in table
      jmp   ACP_Loop
      

ArcNetFound:
      mov   ax, 1                   ; return TRUE
      jmp   short ACP_End

NoArcNet:
      xor   ax, ax                  ; return FALSE

ACP_End:
    ret
ArcnetCardPresent ENDP

;========================================================
; void pascal VideoGetMode( void );
; Returns current video mode and does setup of the
; static variables containing screen setup
;========================================================

VideoGetMode  PROC

	mov	AH,GET_VIDEO_MODE
	int	10h
	mov	ScreenPage,BH
	mov	ScreenMode,AL
	mov	ScreenWidth,AH
	mov	ScreenBuffer,0B000h ; Assume mono
	mov	WORD PTR VideoAddress[2],0B000h
	cmp	AL,07
	je	ReturnMode
	mov	ScreenBuffer,0B800h ; Set to address of color memory
	mov	WORD PTR VideoAddress[2],0B800h
ReturnMode:
	mov	WORD PTR VideoAddress,0B000h	; Alway on page 1
	cbw			    ; Convert AL to integer
	ret

VideoGetMode ENDP

;========================================================
; VideoSetMode() attempts to set the video mode
; passed as the argument. Returns new current mode
;========================================================

VideoSetMode	PROC	NewMode:BYTE

	mov	AL,NewMode	    ; Get new mode in AX
	mov	AH,SET_VIDEO_MODE
	int	10h
	call_M	<VideoGetMode>	    ; Update static video variables
	ret			    ; Returns new video mode in AX

VideoSetMode ENDP

;========================================================
; VideoSaveMode() saves the number of lines currently
; displayed.  The information is saved in global storage
; so that it can later be restored. The video mode along
; with the cursor size is also saved so it can later be
; restored.
;
;========================================================

VideoSaveMode	PROC

	mov	ah,0Fh			; (AH) = 0Fh
	int	10h			; transfer to bios
	mov	SaveScreenMode,al	; save video mode
        push	es			; save ES since next call trashes it
        push	bp			; save BP since next call trashes it
	mov	ax,1130h	        ; (AX) = 1130h
	xor	bx,bx			; (BX) = 0 
	int 	10h                     ; get current char generator info
	mov	SaveScreenRows,dl	; save the number of rows (DL)
        pop	bp			; restore BP
        pop	es			; restore ES

	mov  	ah,03h			; (AH) = 03h
	xor  	bx,bx                   ; (BX) = 0
	int  	10h                     ; get cursor size
	mov  	SaveCursorSize,cx	; save the cursor size (CX)
	ret

VideoSaveMode ENDP

;========================================================
; VideoRestoreMode() restores the video mode to the state
; it was in when VideoSaveMode was called.
;========================================================

VideoRestoreMode PROC

	mov	ah,0			; (AH) = 0
	mov	al,SaveScreenMode	; (AL) = attribute byte
	int	10h			; transfer to bios

	mov	ah,SaveScreenRows	; (AH) = number of rows
	cmp	ah,25                   ; if previously 43 line mode then
	jbe	short VRestoreDone	;   restore that mode

	mov	ax,1112h		; (AX) = 1112h, 8 X 8 char defs
	mov	bl,0                    ; (BL) = 0
	mov	dl,SaveScreenRows	; (DL) = number rows, 2bh = 43 decimal
	int	10h			; load graphics char def
	mov	ax,1200h		; (AX) = 1200h
	mov	bl,20h			; (BL) = 20h
	int	10h			; print screen code must find correct
					;   number of rows to print
VRestoreDone:
	cmp	DisplayType,EGA		; May need to restore cursor
	jne	SetCursSize		; emulation on EGA systems

	push	ES
	mov	AX,BIOS_DATA		; Need to update bit 1 of 40h:87h
	mov	ES,AX
	ASSUME	ES:BIOS_DATA
	and	ES:CursorEmulation,0feh	; Disable ROM BIOS cursor emulation
	mov	AL,EmulationSave	; EmulationSave has only has lsb
	or	ES:CursorEmulation,AL	; so it only affects lsb of 
	pop	ES			; CursorEmulation

SetCursSize:
	mov  ah,01h			; (AH) = 1
	mov  cx,SaveCursorSize		; (CX) = cursor size
	int  10h                        ; restore the cursor
	ret

VideoRestoreMode ENDP

;========================================================
; Returns true if a color card is active
;========================================================

VideoIsColor  PROC

	mov	AL,MonitorType		; Monitor type == 0 if mono or 1
	cbw				; for color
	ret

VideoIsColor ENDP

;========================================================
; Sets active display page
;========================================================

VideoSetPage PROC  ScrPage:BYTE

	mov	AL,ScrPage
	mov	AH,SET_VIDEO_PAGE
	int	10h
	call_M	<VideoGetMode>	    ; Update static video variables
	ret

VideoSetPage ENDP

;========================================================
; Get current cursor position. The top byte of the
; return value holds the row, the bottom by the
; column.
;========================================================

VideoGetRowCol PROC

	mov	AH,GET_CURS_POS
	mov	BH,ScreenPage
	int	10h
	mov	CursorSize,CX		; Save current cursor size
	mov	AX,DX			; Put cursor pos in AX for return value
	ret

VideoGetRowCol ENDP

;========================================================
; Modify current cursor position.
;========================================================

VideoSetRowCol  PROC Row:BYTE, Column:BYTE

	mov	AH,SET_CURS_POS
	mov	BH,ScreenPage
	mov	DH,BYTE PTR Row		; Put Row in CL
	mov	DL,BYTE PTR Column	; Put Column in DL
	int	10h
	ret

VideoSetRowCol ENDP

;========================================================
; Functions to set cursor size
;========================================================

VideoSetCursSize PROC CurSize:WORD

	mov	BH,ScreenPage
	mov	CX,CurSize
	mov	AH,SET_CURS_SIZE
	int	10h
	mov	CursorSize,CX	     ; Save the new cursor size
	ret

VideoSetCursSize ENDP

;========================================================
;========================================================

VideoGetCursSize  PROC
	mov	AX,CursorSize
	ret
VideoGetCursSize ENDP

;========================================================
;========================================================

VideoCursOff  PROC

	cmp	ScreenMode,4		; Make sure not in graphics mode
	jl	CutOff
	cmp	ScreenMode,7
	je	CutOff
	jmp	SHORT CutOffReturn

CutOff:
	mov	AL,DisplayType
	cmp	AL,EGA
	je	IsHighRes
	cmp	AL,VGA
	je	IsHighRes
	cmp	AL,GAD_8514
	je	IsHighRes
	cmp	AL,EGA_MONO
	je	IsHighRes
	cmp	AL,VGA_MONO
	je	IsHighRes

	mov	CX,0f00h		; Put cutoff size in CX
	jmp	SHORT TurnItOff

IsHighRes:

	mov	CX,1000h
TurnItOff:
	mov	AH,SET_CURS_SIZE
	mov	BH,ScreenPage
	int	10h
	mov	CursorSize,CX		; Save the new cursor size
CutOffReturn:
	ret

VideoCursOff ENDP

;========================================================
; Make the cursor a block curser
;========================================================

VideoBlockCurs  PROC

	mov	CX,0007h		; Assume color screen
	cmp	ScreenMode,7
	jne	SetBlock
	mov	CX,000bh		; Was mono screen
SetBlock:
	mov	AH,SET_CURS_SIZE
	mov	BH,ScreenPage
	int	10h
	mov	CursorSize,CX		; Save the new cursor size
	ret

VideoBlockCurs ENDP

;========================================================
; Make it an underline cursor
;========================================================

VideoNormalCurs  PROC

	mov	CX,0607h		; Assume color screen
	cmp	ScreenMode,7
	jne	SetNormal
	mov	CX,0a0bh		; Was mono screen
SetNormal:
	mov	AH,SET_CURS_SIZE
	mov	BH,ScreenPage
	int	10h
	mov	CursorSize,CX		; Save the new cursor size
	ret

VideoNormalCurs ENDP

; =======================================================
; void VideoPutChar( char Character);
;
; Displays a character at the current cursor position on the screen and
; advances the cursor to the next character position.
; =======================================================

VideoPutChar PROC Character:BYTE

	mov	AH,WRITE_TTY		; BIOS tty video output
	mov	AL,BYTE PTR Character	; Put the character in AL
	mov	BH,ScreenPage		; Put page number in BH
	int	10h			; BIOS video call
	ret				; Return to caller

VideoPutChar ENDP

; =======================================================
; void VideoPutCharRowCol( int Row, int Col, int Character )
;
; Displays a character at the specified row and column on the screen.
; Cursor will end up at the specified row and column position.
; =======================================================

VideoPutCharRowCol  PROC Row:BYTE, Col:BYTE, Character:BYTE
					; Set the cursor position
	mov	AH,SET_CURS_POS		; Put function number in AH
	mov	AL,BYTE PTR Character	; Put the character in AL
	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	mov	BH,ScreenPage		; Put page number in BH
	push	BP
	int	10h			; Let BIOS set the cursor position
	pop	BP
	mov	AL,BYTE PTR Character	; Put the character to display in AL
	mov	AH,WRITE_CHAR_ONLY	; Display the character in AL
	mov	CX,1			; Set to display 1 character
	int	10h			; Let BIOS set the cursor position
	ret				; Return to caller

VideoPutCharRowCol  ENDP

; =======================================================
;   VideoPutCharAttr( Row, Col, Character, Color )
;
; Displays a character at the specified row and column on the screen  with
; the specifed screen attribute. Cursor will end up at the specified row
; and column position.
; =======================================================

VideoPutCharAttr  PROC Row:BYTE, Col:BYTE, Character:BYTE, Color:BYTE

					; Set the cursor position
	mov	AH,SET_CURS_POS		; Put function number in AH
	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	mov	BH,ScreenPage		; Put page number in BH
	push	BP
	int	10h			; Let BIOS set the cursor position
	pop	BP
	mov	AH,WRITE_CHAR_ATTR	; Display the character in AL
	mov	AL,BYTE PTR Character	; Put the character to display in AL
	mov	BH,ScreenPage		; Put display in BH
	mov	BL,BYTE PTR Color	; Put display attribute in BL
	mov	CX,1			; Set to display 1 character
	int	10h			; Let BIOS set the cursor position
	ret				; Return to caller

VideoPutCharAttr  ENDP

ifdef JAPAN
;
;	for Double Byte Character
;
VideoPutDBCharAttr  PROC Row:BYTE, Col:BYTE, Character:WORD, Color:BYTE

	mov	AX,Character		; Display the character in AX
	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	mov	BL,BYTE PTR Color	; Put display attribute in BL
	call	NEAR PTR write_dbchar	; display it
	ret				; Return to caller

VideoPutDBCharAttr  ENDP

;
;	Write Double Byte Character
;
;	input:	AX = character
;		BL = color
;		DH = cursor row
;		DL = cursor column
;	output:	DL = next column
;
write_dbchar PROC NEAR
	push	ax
	mov	ah,SET_CURS_POS
	mov	bh,ScreenPage
	int	10h			; set cursor in DX
	pop	ax
	push	ax
	xchg	al,ah
	mov	ah,WRITE_CHAR_ATTR
	mov	bh,ScreenPage
	mov	cx,1
	int	10h			; display lead byte
	mov	ah,SET_CURS_POS
	mov	bh,ScreenPage
	inc	dl			; next column
	int	10h
	pop	ax
	push	ax
	mov	ah,WRITE_CHAR_ATTR
	mov	bh,ScreenPage
	mov	cx,1
	int	10h			; display tail byte
	inc	dl
	pop	ax
	ret
write_dbchar	endp

endif


; =======================================================
; void	VideoPuts( char *String )
; =======================================================

IF @DataSize
  VideoPuts PROC USES DS SI, String:PTR
ELSE
  VideoPuts PROC USES    SI, String:PTR
ENDIF

	mov	BH,ScreenPage		; Put current display page in BH
;	lds	SI,String		; DS:SI --> String to display
	LoadPtr	DS, SI, String		; DS:SI --> String to display

NextChar:
	mov	AH,WRITE_TTY		; Put tty write function in AH
	cld				; KEEP IN THE LOOP
	lodsb				; Put character into AL
	and 	AL,AL			; See if EOL character
	jz	EndPuts 		; If AL == 0 we're finished
	int	10h			; Display the character
	jmp	SHORT NextChar

EndPuts:
	ret

VideoPuts ENDP

; =======================================================
; void  VideoPutsRowCol( int Row, int Col, char *String )
; =======================================================

IF @DataSize
  VideoPutsRowCol PROC USES DS SI, Row:BYTE, Col:BYTE, String:PTR
ELSE
  VideoPutsRowCol PROC USES    SI, Row:BYTE, Col:BYTE, String:PTR
ENDIF
					; Set cursor to Row,Col
					; Access all variables in .DATA
					; before changing DS
	mov	AH,SET_CURS_POS		; BIOS set cursor function
	mov	BH,ScreenPage		; Put current display page in BH
	mov	DH,Row			; Put screen row in DH
	mov	DL,Col			; Put screen column in DL
	int	10h			; BIOS video call

        mov     BH,ScreenPage           ; Load screen page again.  Paranoia
                                        ; in case some video BIOS changes BH.
                                        ; Probably not necessary.
GetPtrToString:
;	lds	SI,String		; DS:SI --> String to display
	LoadPtr	DS, SI, String		; DS:SI --> String to display

LoadNextChar:
	mov	AH,WRITE_TTY		; Put tty write function in AH
	cld				; KEEP IN THE LOOP
	lodsb				; Put character into AL
	and 	AL,AL			; See if EOL character
	jz	EndPutsRowCol		; If AL == 0 we're finished
	int	10h			; Display the character
	jmp	SHORT LoadNextChar

EndPutsRowCol:
	ret

VideoPutsRowCol ENDP

; =======================================================
; void  VideoPutsAttrRowCol( int Row, int Col, char *String, int Attr )
; =======================================================

VideoPutsAttrRowCol PROC USES ES SI, Row:BYTE, Col:BYTE, \
				     String:PTR, Attr:BYTE

					; Set cursor to Row,Col
	mov	AH,SET_CURS_POS		; BIOS set cursor function
	mov	BH,ScreenPage		; Put current display page in BH
	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	push	BP
	int	10h	; BIOS video call
	pop	BP
SetUpForStringDisplay:
;	les	SI,String		; Put pointer to string in DS:SI
	LoadPtr	ES, SI, String		; ES:SI --> String to display

PutNextCharAttr:
	cld				; Set direction to forward
	mov	AL,ES:[SI]		; Put next character into AL
	inc	SI			; Increment string pointer
	or	AL,AL			; Check for EOL marker ('\0')
	jz	EndPutsAttrRowCol	; If EOL then we're finished

	cmp	AL,ASCII_LF		; Check for line feed char ('\n')
	jnz	NormalPutsAttrRowCol	; Ok to display the character

DisplayLfCr:
;	inc	SI			; Skip over this character
	mov	AL,ASCII_CR		; Use tty output to scroll screen
	mov	AH,WRITE_TTY		; BIOS tty video output
	mov	BH,ScreenPage		; Put page number in BH
	push	BP
	int	10h			; BIOS video call
	pop	BP
	mov	AL,ASCII_LF		; Use tty output to scroll screen
	mov	AH,WRITE_TTY		; BIOS tty video output
	int	10h			; BIOS video call

UpdateScreenPosition:
	mov	BH,ScreenPage		; Put page number in BH
	mov	AH,GET_CURS_POS		; Get curs position function
	push	BP
	int	10h			; BIOS video call
	pop	BP
	jmp	SHORT PutNextCharAttr	; Go back and do next character

NormalPutsAttrRowCol:
	mov	AH,WRITE_CHAR_ATTR	; Display char & attr function
	mov	BH,ScreenPage		; Put page number in BH
	mov	BL,BYTE PTR Attr	; Put char attribute in BL
	mov	CX,1			; Put # of chars to display in CX
	push	BP
	int	10h			; BIOS video call
	pop	BP
	inc	DL			; Increment DL to next col position
	cmp	DL,ScreenWidth		; Check for last screen column
	jc	SetScreenLocation	; If not past last column move curs
	mov	DL,0			; Else do a cariage return
	inc	DH			; and increment the row
	cmp	DH,ScreenLength 	; See if past bottom of screen
	jc	SetScreenLocation	; If not move the cursor
	jmp	SHORT DisplayLfCr	; Else do a CR-LF
SetScreenLocation:
	mov	AH,SET_CURS_POS		; BIOS set cursor function
	mov	BH,ScreenPage		; Put current display page in BH
	push	BP
	int	10h			; BIOS video call
	pop	BP
	jmp	SHORT PutNextCharAttr
EndPutsAttrRowCol:
	ret				; Finished displaying the string

VideoPutsAttrRowCol ENDP

; =======================================================
; void  VideoDupAttr( int Row, int Col, int Attr, int DupFactor )
; =======================================================

VideoDupAttr PROC USES ES DI, Row:BYTE, Col:BYTE, Attr:BYTE, Count:WORD

	mov	AH,GET_CURS_POS		; Save the initial cursor position
	mov	BH,ScreenPage		; Put the display page in BH
	int	10h			; BIOS video call
	push	DX			; Save the row and column
LoopSetUp:
	mov	CX,Count		; Put duplication factor in CX
StartOfLoop:
	push	CX

	mov	AH,SET_CURS_POS		; Put set curs. function in AH
	mov	DH,BYTE PTR Row		; Put screen row in DH
	mov	DL,BYTE PTR Col		; Put screen column in DL
	mov	BH,ScreenPage		; Put page number in BH
	push	BP
	int	10h			; BIOS video call
	pop	BP
ReadFromScreen:
	mov	AH,READ_CHAR_ATTR	; Put read from screen function in AH
	mov	BH,ScreenPage		; Put page number in BH
	push	BP
	int	10h			; BIOS video call
	pop	BP
WriteToScreen:
	mov	BL,BYTE PTR Attr	; Put new attribute in BL
	mov	CX,1			; Put character count in CX
	mov	AH,WRITE_CHAR_ATTR	; Display the char & new attribute
	push	BP
	int	10h			; BIOS video call
	pop	BP
UpdateCursor:
	inc	Col			; Increment the display column
	pop	CX
	loop	StartOfLoop
EndOfLoop:
	pop	DX			; Get original curs pos. from stack
	mov	AH,GET_CURS_POS		; Save the initial cursor position
	mov	BH,ScreenPage		; Put the display page in BH
	int	10h			; Restore original curs. position

	ret

VideoDupAttr ENDP

; =======================================================
; VideoDupCharAttr( int Row, int Col, int DupChar, int Attrib, int DupFactor )
; =======================================================

VideoDupCharAttr PROC USES ES DI, Row:BYTE, Col:BYTE, DupChar:BYTE, \
                                     Attrib:BYTE, DupFactor:WORD

	mov	AH,SET_CURS_POS		; Put function number in AH
	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	mov	BH,ScreenPage		; Put page number in BH
	int	10h			; Let BIOS set the cursor position
	mov	AH,WRITE_CHAR_ATTR	; Display the character in AL
	mov	AL,BYTE PTR DupChar	; Put the character to display in AL
	mov	BH,ScreenPage		; Put display in BH
	mov	BL,BYTE PTR Attrib	; Put display attribute in BL
	mov	CX,DupFactor		; Put # times to dup in CX
	int	10h			; Let BIOS set the cursor position
	ret

VideoDupCharAttr ENDP

ifdef JAPAN
;
;	for Double Byte Character
;
VideoDupDBCharAttr PROC USES ES DI, Row:BYTE, Col:BYTE, DupChar:WORD, \
                                     Attrib:BYTE, DupFactor:WORD

	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	mov	AX,DupChar		; Put the character to display in AX
	mov	BL,BYTE PTR Attrib	; Put display attribute in BL
	mov	CX,DupFactor		; Put # times to dup in CX
@@:
	push	cx
	call	NEAR PTR write_dbchar
	pop	cx
	loop	@b
	ret

VideoDupDBCharAttr ENDP

endif

; =======================================================
; VideoVertDupCharAttr( int Row, int Col, int DupChar,
;                       int Attrib, int DupFactor )
; =======================================================

VideoVertDupCharAttr PROC USES ES DI, Row:BYTE, Col:BYTE, DupChar:BYTE, \
                                     Attrib:BYTE, DupFactor:WORD

	mov	CX,DupFactor		; Put duplication count in CX
        jcxz    EndVerticalLine         ; Fall out and do nothing
                                        ; if CX is zero

VerticalLineLoop:
	push	CX			; Save loop count
	mov	AH,SET_CURS_POS		; Put set cursor function in AH
	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	mov	BH,ScreenPage		; Put page number in BH
	push	BP
	int	10h			; Let BIOS set the cursor position
	pop	BP
	mov	AH,WRITE_CHAR_ATTR	; Put display function in AH
	mov	BH,ScreenPage		; Put page number in BH
	mov	BL,BYTE PTR Attrib	; Put attribute in BL
	mov	AL,BYTE PTR DupChar	; Put character in AL
	mov	CX,1			; Put # of chars in CX
	push	BP
	int	10h		; Video BIOS call
	pop	BP
	inc	Row			; Increment the screen row
	pop	CX			; Restore loop count
	loop	VerticalLineLoop
EndVerticalLine:
	ret

VideoVertDupCharAttr ENDP

ifdef JAPAN
;
;	for Double Byte Character
;
VideoVertDupDBCharAttr PROC USES ES DI, Row:BYTE, Col:BYTE, DupChar:WORD, \
                                     Attrib:BYTE, DupFactor:WORD

	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	mov	BL,BYTE PTR Attrib	; Put attribute in BL
	mov	AX,DupChar		; Put character in AX
	mov	CX,DupFactor		; Put duplication count in CX
        jcxz    vd_end		        ; Fall out and do nothing
                                        ; if CX is zero

vd_loop:
	push	cx
	push	dx
	call	NEAR PTR write_dbchar
	pop	dx
	pop	cx
	inc	dh			; next row
	loop	vd_loop
vd_end:
	ret

VideoVertDupDBCharAttr ENDP

endif

; =======================================================
; VideoVertDupAttr( int Row, int Col, int Attrib, int DupFactor )
; =======================================================

VideoVertDupAttr PROC USES ES DI, Row:BYTE, Col:BYTE, Attrib:BYTE, \
				  DupFactor:WORD

	mov	CX,DupFactor		; Put duplication count in CX

VertAttrLoop:
	push	CX			; Save loop count
	mov	AH,SET_CURS_POS		; Put set cursor function in AH
	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	mov	BH,ScreenPage		; Put page number in BH
	push	BP
	int	10h			; Let BIOS set the cursor position
	pop	BP

ReadFromScreen2:
	mov	AH,READ_CHAR_ATTR	; Put read from screen function in AH
	mov	BH,ScreenPage		; Put page number in BH
	push	BP
	int	10h			; BIOS video call
	pop	BP
WriteToScreen2:
	mov	BL,BYTE PTR Attrib	; Put new attribute in BL
	mov	CX,1			; Put character count in CX
	mov	AH,WRITE_CHAR_ATTR	; Display the char & new attribute
	push	BP
	int	10h			; BIOS video call
	pop	BP
UpdateCursor:
	inc	Row			; Increment the display column
	pop	CX
	loop	VertAttrLoop
	ret

VideoVertDupAttr ENDP

; =======================================================
; void VideoScrollDn( int StartRow, int StartCol, int EndRow, int EndCol,
;                     int Lines, int Attrib )
; =======================================================

VideoScrollDn PROC StartRow:BYTE, StartCol:BYTE, EndRow:BYTE, EndCol:BYTE, \
		   Lines:BYTE, Attrib:BYTE

	mov	AH,SCROLL_DOWN		; BIOS scroll down function
	mov	AL,BYTE PTR Lines	; # of lines to scroll
	mov	BH,BYTE PTR Attrib	; Attribute for erased lines
	mov	CH,BYTE PTR StartRow	; Put starting row in CH
	mov	CL,BYTE PTR StartCol	; Put starting column in CL
	mov	DH,BYTE PTR EndRow	; Put ending row in DH
	mov	DL,BYTE PTR EndCol	; Put ending column in DL
	int	10h			; BIOS video call
	ret

VideoScrollDn ENDP

; =======================================================
; void VideoScrollUp( int StartRow, int StartCol, int EndRow, int EndCol,
;                     int Lines, int Attrib )
; =======================================================

VideoScrollUp PROC StartRow:BYTE, StartCol:BYTE, EndRow:BYTE, EndCol:BYTE, \
		   Lines:BYTE, Attrib:BYTE

	mov	AH,SCROLL_UP		; BIOS scroll down function
	mov	AL,BYTE PTR Lines	; # of lines to scroll
	mov	BH,BYTE PTR Attrib	; Attribute for erased lines
	mov	CH,BYTE PTR StartRow	; Put starting row in CH
	mov	CL,BYTE PTR StartCol	; Put starting column in CL
	mov	DH,BYTE PTR EndRow	; Put ending row in DH
	mov	DL,BYTE PTR EndCol	; Put ending column in DL
	int	10h			; BIOS video call
	ret				; Finished with this call

VideoScrollUp ENDP

; =======================================================
; void VideoCls( int Attrib )
; =======================================================

VideoCls PROC Attrib:BYTE

	mov	AH,SCROLL_DOWN		; Put function request in AH
	xor	AL,AL			; Set AL for clear screen
	mov	BH,BYTE PTR Attrib	; Put screen attribute in BH
	mov	CX,0			; Put starting row-col & 0,0
	mov	DH,ScreenLength		; Put starting row in DH
	dec	DH			; Decrement to start at 0
	mov	DL,ScreenWidth		; Put ending column in DL
	dec	DL			; Decrement to start at 0
	int	10h			; BIOS video call
	ret				; Finished with clear screen

VideoCls ENDP
;===============================================================
; void VideoGetBlock( int Row, int Col, char *awDest, int Count );
;===============================================================

VideoGetBlock PROC USES ES DI SI, Row:BYTE, Col:BYTE, awDest:PTR, Count:WORD

SetUpForBlockMove:
	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	mov	BH,ScreenPage		; Put the screen page in the
	xor	BL,BL			; high byte of SI
	mov	SI,BX

;	les	DI,awDest		; Put pointer to destin. in ES:DI
	LoadPtr	ES, DI, awDest		; ES:DI --> Destination buffer

	mov	CX,Count		; Put number words to move in CX
MoveToNextGetPosition:
	mov	BX,SI
	mov	AH,SET_CURS_POS		; BIOS set cursor function
	push	BP
	int	10h			; BIOS video call
	pop	BP
GetNextCharAttr:
	mov	BX,SI
	mov	AH,READ_CHAR_ATTR	; Bios get char & attribute function
	push	BP
	int	10h			; BIOS video call
	pop	BP
	cld				; Set direction to forward
	stosw
	inc	DL			; Increment the column
	loop	MoveToNextGetPosition
	ret

VideoGetBlock ENDP

;===============================================================
; void VideoPutBlock( int Row, int Col, char *awSource, int Count );
;===============================================================

IF @DataSize
VideoPutBlock PROC USES DS DI SI, Row:BYTE, Col:BYTE, awSource:PTR, \
			          Count:WORD
ELSE
VideoPutBlock PROC USES    DI SI, Row:BYTE, Col:BYTE, awSource:PTR, \
			          Count:WORD
ENDIF

SetUpForBlockDisplay:
	mov	DH,BYTE PTR Row	     	; Put screen row in DH
	mov	DL,BYTE PTR Col	     	; Put screen column in DL
	mov	BH,ScreenPage		; Put the screen page in the
	xor	BL,BL			; high byte of DI
	mov	DI,BX

;	lds	SI,awSource		; Put pointer to source in DS:DI
	LoadPtr	DS, SI, awSource	; DS:SI --> Source buffer

	mov	CX,Count		; Put number words to move in CX
MoveToNextColPosition:
	push	CX			; Save the count
	mov	BX,DI
	mov	AH,SET_CURS_POS		; BIOS set cursor function
	push	BP
	int	10h			; BIOS video call
	pop	BP
PutNextCharAttr:
	mov	BX,DI			; Put page number in BH
	cld				; Set direction to forward
	lodsw				; Get char in AL & attrib in AH
	mov	BL,AH			; Move attribute in BL
	mov	AH,WRITE_CHAR_ATTR	; Bios get char & attribute function
        mov	CX,1			; Set number char & attrib to write
	push	BP
	int	10h			; BIOS video call
	pop	BP
	inc	DL			; Increment the column
	pop	CX			; Restore the loop count in CX
	loop	MoveToNextColPosition
	ret

VideoPutBlock ENDP

; =======================================================
; Check for a Hercules. We are checking for a Herc102, 112, 222.
; This is the best possible test since the Hercules does not have any
; standard detection mechanism built into it.
; Taken from WORD code.
; Returns in AX
;       0 - if it is not a Hercules.
;       1 - Herc 112
;       2 - Herc 222
;       3 - Herc 102
; =======================================================

HercCheck PROC NEAR USES ES
	 LOCAL locvar1:WORD

         MOV       locvar1,0		; initiliase local var

	mov	AX,0b000h
	mov	ES,AX			; ES --> MDA video memory segment
	mov	AX,ES:[00]		; Get first byte of MDA memory
	mov	BX,AX

	not	AX			; M100 - Use xchg to fix floating bus
	xchg	WORD PTR ES:[00],AX	; Flip the bits in memory
	not	AX

	cmp	AX,ES:[00]		; See memory bits really flipped
	mov	ES:[00],BX		; Retore memory location
	jne	HercDone		; If memory & AX are not equal it means
					; the memory wasn't real

         MOV       DX, HercStatusPort   ; Herc status port is 03BAh
         XOR       BX, BX               ; Start with no 112 or 222 successes.
         MOV       CX, 100                ; Get a majority of 100 tries

Check:

         IN       AL, DX
         AND       AL, idmask_222         ; strip to just the 222 bits
         CMP       AL, idcode_222         ; check for GB222 code
         JNE       Check_112              ; No, Skip to test for 112
         INC        BH                    ; Count another 222 success

Check_112:

         AND       AL, idmask_112         ; strip further to just the 112 bits
         CMP       AL, idcode_112         ; check for the GB112 id code
         JNE       CheckAgain             ; No skip
         INC        BL                    ; Count another 112 success

CheckAgain:

         LOOP       Check                 ; Loop back and check again

; 100 tries are over.
         CMP       BL, 50                 ; Did we get a majority of 112 hits ?
         JBE       CheckGB102             ; If not check for a Herc GB102

; every time we get a 222 hit we get a 112 hit too.
; Hence it can be either a 112 or a 222 at this stage.
         MOV       locvar1, 1             ; indicate it is a 112, this may be
                                          ; over written later
         CMP       BH, 50                 ; Did we get a majority for the 222 ?
         JBE       HercDone               ; it is a Herc 112 only.
         MOV       locvar1, 2             ; It is a Herc 222, overwrite locvar1.
	 JMP	   SHORT HercDone

CheckGB102:

; Bit 7 at port HercStatusPort changes pretty often on a Hercules. Check to see
; if it flips.

			MOV		 DX, HercStatusPort
			MOV		 CX, 6FFFh					; m119: # of times we read from port 03BA
         IN        AL, DX
         MOV       BL, AL

ReadAgain:

         IN        AL, DX                 ; Get current value of port 03BA
         XOR       AL, BL                 ; XOR to see if it changes
         OR        AL, AL                 ; Did it ?
         JS        Yes102                 ; Yes it is a Hercules
         LOOP      ReadAgain              ; No, try again

	 JMP	   SHORT HercDone
Yes102:
         MOV       locvar1, 3             ; indicate it is a Herc GB102

HercDone:

         MOV       AX, locvar1
	 ret

HercCheck ENDP

; =======================================================
;
; int GetOemDisplayID( void )															*/
; 																								*/
;	Will return display type.
; 																								*/
; ENTRY: None 																				*/
; EXIT:	Returns Video adapter type 													*/
;			0 = MONO																				*/
;			1 = CGA																				*/
;			2 = EGA																				*/
;			3 = VGA																				*/
;			4 = HERCULES																		*/
;
; =======================================================

GetOemDisplayID PROC

	mov	AL,DisplayType

ifdef JAPAN
	cmp	al,EGA
	jnz	@f			; if not EGA
	xor	bx,bx
	mov	ah,50h
	mov	al,1			; get display mode
	int	10h
	mov	al,EGA
	cmp	bx,81
	jnz	@f			; if not JAPAN
	mov	al,JEGA
@@:
endif

	cbw
	ret

GetOemDisplayID ENDP


; =======================================================
; Returns AX != 0 if CGA adapter and SI is set to CGA 
; =======================================================

CgaCheck PROC NEAR USES ES

	mov	AX,0b800h
	mov	ES,AX			; ES --> CGA memory segment

	mov	AX, ES:0000h		; Get first byte of CGA memory
	mov	BX, AX			; and save it for replacement

	not	AX			; M100 - Use xchg to fix floating bus
	xchg	AX,ES:0000h
	not	AX			; If memory is real AX should
					; equal ES:0000h
	cmp	AX, ES:0000h		; See if it really changed
	mov	ES:0000,BX		; Restore original value

	jne	MonoRet			; If zero memory was real

		; We found CGA memory so now see if monochrome memory
		; also exists

	mov	AX,0b000h
	mov	ES,AX			; ES --> Monochrome memory segment

	mov	AX, ES:0000h		; Get first byte of Monochrome memory
	mov	BX, AX			; and save it for replacement

	not	AX			; M100 - Use xchg to fix floating bus
	xchg	AX,ES:0000h
	not	AX			; If memory is real AX should
					; equal ES:0000h

	cmp	AX, ES:0000h		; See if it really changed
	mov	ES:0000,BX		; Restore original value
	jne	CgaRet			; If zero memory was real

		; M100 - We found both CGA and Monochrome memory so no get the
		; video mode to determine the active video type

	mov	AH,0fh
	int	10h
	cmp	AL,07			; See if monochrome video mode
	je	MonoRet			; If it is return not CGA


CgaRet:
	mov	SI,CGA			; Assume memory was real
	mov	AX,1			; Signal CGA adapter was detected.
	jmp	SHORT @f

MonoRet:
	mov	AX,0			; Signal not a CGA adapter.
	mov	SI,MONO			; Not CGA so must be MONO

@@:
	ret

CgaCheck ENDP

; =======================================================
;
; This function detects the presence of an 8514 display card.
;
;The way we do this is to first write to the Error Term Register and then
;make sure we can read back the value we wrote. Next I read in the value of
;the subsystem status register and check bit 7 to determine if the 8 plane
;memory configuration is present. Only if both these conditions are
;satisfied will we install the 8524 display driver.
;
;Entry:
;       SI has current video mode
;Exit:
;	SI has been updated to VGA or VGA_MONO if 8514 is detected
;       if display card is present and contains the proper memory
;	configuration.
;
; =======================================================

; Equates for 8514 register ports.

ERR_TERM             equ   92e8h       ; 8514 error term register.
SUBSYS_STAT          equ   42e8h       ; 8514 Subsystem status register.

Detect8514 PROC NEAR

	mov	dx, ERR_TERM	; load DX with port address (error term port).
	mov	ax, 5555h	; load AX with value to write to port.
	out	dx, ax		; Write the data.
	IO_Delay
	IO_Delay
	in	ax, dx		; Now read the data back in.
	cmp	ax, 5555h	; Q: is 8514 present ?
	jne	fn8514Done	;   N: indicate 8514 not present.
				;   Y: 8514 is present, now check monitor.
    ;
    ; Now we need to determine what type of monitor is attached to the
    ; 8514 card. To do this we check ID bits 6,5,4 of the subsystem
    ; status register. Depending on the Monitor attached we return:
    ;
    ; There are 4 valid monitor types:
    ; 
    ; bits 6-4       Type
    ;
    ;   001          Ascot (mono)
    ;   010          Henley (color)
    ;   011          Invalid (color) This is supposedly valid in spite
    ;                of what the spec says.
    ;   101          Surrey (mono)
    ;   110          Conestoga/Crown (color)
    ;
    mov  dx,SUBSYS_STAT      ; Now, we have the adapter. Check monitor.
    in   ax,dx               ; Get word from SUBSYS_STAT
    and  ax,0070h            ; Mask out bits 6-4
    cmp  ax,0020h            ; Is it Henley type? 
    je   Disp_8514           ; Yes, then it is an 8514 type
    cmp  ax,0060h            ; Is it Conestoga/Crowwn type?
    je   Disp_8514           ; Yes, then it is an 8514 type
    cmp  ax,0030h            ; Is it Korys' machine
    je   Disp_8514           ; Yes, then it is an 8514 type
    cmp  ax,0010h            ; Is it Ascot type?
    je   Disp_8503           ; Yes, then it is a VGA Mono type
    cmp  ax,0050h            ; Is it Surrey type?
    je   Disp_8503           ; Yes, then it is a VGA Mono type

	mov	SI,VGA
	jmp	short fn8514Done

Disp_8503:
	mov	SI,VGA_MONO
	jmp	short fn8514Done
         
Disp_8514:
	mov	SI,GAD_8514
         
fn8514Done:
	ret

Detect8514	ENDP

; =======================================================

ifdef DBCS
;
;	Check if the specified cursor is at lead byte
;
CheckLead PROC USES ES SI, Row:BYTE, Col:BYTE
	mov	al, byte ptr Row
	mul	ScreenWidth
	mov	si,ax
	shl	si,1			; set start address of the row
	add	al,Col
	adc	ah,0
	mov	bx,ax
	shl	bx,1			; get cursor address
	mov	es,ScreenBuffer
	call	NEAR PTR aCheckDBCSTailByte
	jz	cl_no			; if this is tail byte
	mov	al,es:[bx]
	call	NEAR PTR aIsDBCSLeadByte
	jnz	cl_no			; if this is not lead byte
	mov	ax,1
	jmp	short cl_ret
cl_no:
	mov	ax,0
cl_ret:
	ret
CheckLead	ENDP

;
;	Check if the specified cursor is at tail byte
;
CheckTail PROC USES ES SI, Row:BYTE, Col:BYTE
	mov	al, byte ptr Row
	mul	ScreenWidth
	mov	si,ax
	shl	si,1			; get start address of the row
	add	al,Col
	adc	ah,0
	mov	bx,ax
	shl	bx,1			; get cursor address
	mov	es,ScreenBuffer
	mov	ax,0
	call	NEAR PTR aCheckDBCSTailByte
	jnz	ct_ret			; if not at tail byte
	inc	ax
ct_ret:
	ret
CheckTail	ENDP

;
;	Test if the character is DBCS Lead Byte
;
;	input:	AL = character to check
;	outpit:	ZF = 1 if DBCS Lead Byte
;

DBCSLeadByteTable	dd	0

aIsDBCSLeadByte	PROC NEAR
	push	ax
	push	si
	push	ds
	lds	si,cs:DBCSLeadByteTable
	cmp	word ptr cs:DBCSLeadByteTable+2,0
	jnz	idlb_check		; if table is already set
	push	ax
	mov	ax,6300h
	int	21h			; get DBCS lead byte table
	pop	ax
	mov	word ptr cs:DBCSLeadByteTable,si
	mov	word ptr cs:DBCSLeadByteTable+2,ds
idlb_check:
	cmp	word ptr [si],0
	jz	idlb_not		; if end of table
	cmp	al,[si]
	jb	idlb_next		; if below low value
	cmp	al,[si+1]
	jbe	idlb_yes		; if below high value
idlb_next:
	add	si,2			; do next
	jmp	short idlb_check
idlb_not:
	or	al,1			; reset ZF
	jmp	short idlb_end
idlb_yes:
	and	al,0			; set ZF
idlb_end:
	pop	ds
	pop	si
	pop	ax
	ret
aIsDBCSLeadByte		endp

;
;	Check if the character position is at Tail Byte of DBCS
;
;	input:	es:si = start address of the string
;		es:bx = character position to check
;	output:	ZF = 1 if at Tail Byte
;
aCheckDBCSTailByte PROC NEAR
	push	ax
	push	bx
	push	cx
	mov	cx,bx			; save character position
cdtb_check:
	cmp	bx,si
	jz	cdtb_next		; if at the top
	sub	bx,2			; go back
	mov	al,es:[bx]		; get character
	call	NEAR PTR aIsDBCSLeadByte
	jz	cdtb_check		; if DBCS lead byte do next
	add	bx,2			; adjust
cdtb_next:
	shr	cx,1
	shr	bx,1
	sub	cx,bx			; if the length is odd then
	xor	cl,1			; the character position is
	test	cl,1			; at the tail byte
	pop	cx
	pop	bx
	pop	ax
	ret

aCheckDBCSTailByte	endp
endif

; ========================================================

.DATA

; ========================================================

ATI_SigBuf	db	'         '
ATI_SigStr	db	'761295520'	; m112 ATI Generic Signature

ATI_Table	db	'21CQ'		; m112 Wonder Rev 1
		db	'21NS'		; m112 Wonder Rev 2
		db	'22NS'		; m112 Wonder Rev 3
		db	'23NS'		; m112 Wonder Rev 4 
		db	'21SC'		; m112 Wonder 800 Rev 0-6 & VIP
		db	'22SC'		; m112 Wonder 801
		db	'23SC'		; m112 VIP PGA
		db	'24SC'		; m112 Wonder 480
		db	'25SC'		; m112 Wonder 880 Rev 7
		db	0,0		; m112 End of table marker

ATI_SIG_LEN	EQU	9
NUM_ATI_CODES	EQU	9

; ========================================================

.CODE

; ========================================================
; Checks for problematic ATI video cards and returns
; Zero flag set if found else NZ
; ========================================================

ATI_Check PROC NEAR USES ES SI DI


	push	DS
	pop	ES			; DS & ES --> .DATA seg
	mov	AX, ATI_ROM
	mov	DS, AX			; DS --> ATI ROM seg

	assume	DS: ATI_ROM
	assume	ES: @DATA

	mov	SI, OFFSET DS:ATI_RomSig ; DS:SI --> Signature string in ROM
	mov	DI, OFFSET ES:ATI_SigBuf ; ES:DI --> Local buffer
	mov	CX, ATI_SIG_LEN		; CX == length of signature in bytes

	cld
	rep	movsb			; Copy signature to local buffer

	push	ES
	pop	DS			; DS --> .DATA seg
	assume	DS: @DATA

	mov	SI, OFFSET ATI_SigStr	; DS:SI --> ATI_SigStr
	mov	DI, OFFSET ATI_SigBuf	; ES:DI --> ATI_SIGBuf
	mov	CX, ATI_SIG_LEN		; CS == length of signature in bytes

	cld
	repe	cmpsb			; DOS ROM have an ATI signature?
	jne	NotBadATI

		; At this point we've determined this is an ATI
		; producte and now we look at the card type

	mov	AX, ATI_ROM
	mov	ES, AX			; ES --> ATI ROM seg
	ASSUME	ES: ATI_ROM
	
	mov	DX,WORD PTR ES:CardType	  ; Get first word of card type
	mov	CX,WORD PTR ES:CardType[2]; Get 2nd word of card type

	mov	SI, OFFSET ATI_Table	; DS:SI --> Table of card types

CheckTableEntry:
	lodsw				; Get first word of this entry
	or	AX,AX			; See if at end of the table
	jz	NotBadATI		; If zero no more entries
	mov	BX,AX			; BX == First word of entry

	lodsw				; Grab 2nd word for this entry
	cmp	BX,DX			; See if entries match
	jne	CheckTableEntry		; If no match go get next entry
	cmp	AX,CX			; See if entries match
	jne	CheckTableEntry		; No match on 2nd words

FoundBadATI:				; Fall thru if card type found in table
	mov	AX,1			; Signal a bad card
	jmp	SHORT ATI_CheckReturn

NotBadATI:
	xor	AX,AX			; Signal didn't find a bad card

ATI_CheckReturn:
	ret	

ATI_Check ENDP

; ========================================================

ATI_ROM SEGMENT	AT 0c000h

; m114	ORG	30h
	ORG	31h			; m114
ATI_RomSig	db	?

	ORG	40h
CardType	db	?


ATI_ROM ENDS

; ========================================================

	END

; ========================================================

