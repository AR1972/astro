        TITLE   LLLPT - GW-BASIC Printer Interface
;***
; LLLPT - GW-BASIC Printer Interface
;
;       Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
        INCLUDE switch.inc
	INCLUDE rmacros.inc	

	useSeg	_DATA		
	useSeg	DV_TEXT 	

	INCLUDE seg.inc 	
        INCLUDE ibmunv.inc

sBegin	_DATA			

LPTNAM  DB      "LPT"           ;name for use in printer open
LPTNUM  DB      " ",0           ;number of printer to open (1 to 3)

sEnd	_DATA			


sBegin	DV_TEXT 		

externNP B$DOS3CHECK		; Check for dos 3

assumes CS,DV_TEXT		

;***
;B$OPNLPT - OPEN selected printer
;OEM-interface routine
;
;Purpose:
;	This routine will open a communications channel to a printer
;	and return a file handle to this channel and a status flag.
;
;	There are two ways to implement the three LPT routines
;	(B$OPNLPT, B$SNDLPT, and B$CLSLPT). They are based
;	on the fact that the file handle used by all three is
;	never checked or used outside of these routines.  The rest
;	of the runtime will just store it away and pass it back to
;	these routines.
;
;	1.) Direct output to a printer.  In this case, there would be
;	    no file handle, and the value passed can be ignored.
;	    B$OPNLPT would have no work to do, and could just return
;	    with AH=0.	B$CLSLPT would just return.  The disadvantage
;	    to this is that it will not work over networks or for
;	    redirected output.
;
;	2.) Use DOS file handles.  This is probably the most standard
;	    method as it allows for redirected printers and network
;	    printing.  In this case, B$OPNLPT would open the printer
;	    as a file, possibly do hardware checks for the existence
;	    of the printer if it is not redirected, and return the
;	    handle for use in other two routines.  B$SNDLPT would
;	    use the file for writing, and B$CLSLPT would close the
;	    file.
;
;	If the B$OPNLPT returns [AH] = 1, the runtime will signal
;	a Device Not Available Error.
;
;
;Entry:
;	[AH]  = printer number (0-2)
;
;Exit:
;	[BX]  = file handle
;	[AH]  = 0 if open successful, 1 if open failed.
;
;Uses:
;	Per convention
;
;Preserves:
;	CX,DX
;
;Exceptions:
;	None.
;******************************************************************************
cProc	B$OPNLPT,<NEAR,PUBLIC>,<DX,SI> 
cBegin				

        CMP     AH,2            ;test against upper limit
        JA      OPNLPT_UNAVAIL  ;if too high, then not available
        MOV     BL,AH           ;move index into register
        XOR     BH,BH           ;make it a 16-bit value
        SHL     BX,1            ;shift to make it a word index
        MOV     SI,BX           ;keep word index for later

        ADD     AH,"1"          ;map 0 to "1", 1 to "2", etc.
        MOV     LPTNUM,AH       ;put unit number in device name
        MOV     DX,OFFSET DGROUP:LPTNAM ;define name string to open
        MOV     AX,3D01H        ;ready to open file for writing
        INT     21H             ;open the file
        JC      OPNLPT_UNAVAIL  ;jump if device not available

        MOV     BX,AX           ;move handle
        MOV     AX,4400H        ;get device status
        INT     21H             ;perform the action
        TEST    DL,80H          ;test if character device
        JZ      OPNLPT_CLOSE    ;jump if error to close and exit

        MOV     AX,4401H        ;IOCTL to set device status
        OR      DL,20H          ;change to raw mode
        XOR     DH,DH           ;clear upper byte...
        INT     21H             ;set the mode

	cCall	B$DOS3CHECK	; See if DOS 3
        JB      OPNLPT_DOS2     ;jump if DOS2

        MOV     AX,440AH        ;get IOCTL call for redirection
        INT     21H             ;flag is in DH
        TEST    DH,80H          ;flag set if redirected
        JNZ     OPNLPT_SUCCESS  ;jump if redirected, done

OPNLPT_DOS2:
        PUSH    DS              ;save BASCOM data segment
        MOV     AX,40H          ;get BIOS data segment
        MOV     DS,AX           ;establish addressability
        TEST    DS:8[SI],0FFFFH ;test if adapter exists
        POP     DS              ;restore data segment
        JNZ     OPNLPT_SUCCESS  ;if so, success, jump

OPNLPT_CLOSE:
        MOV     AH,3EH          ;system code for closing
        INT     21H             ;close the file opened

OPNLPT_UNAVAIL:
        MOV     AH,1            ;error code for device unavailable
        XOR     BX,BX           ;clear BX for error condition
	JMP	SHORT OPNLPT_DONE ;jump to exit

OPNLPT_SUCCESS:
        XOR     AH,AH           ;clear error code for success
OPNLPT_DONE:

cEnd				

;***
;B$SNDLPT - Send a character to selected printer
;OEM-interface routine
;
;Purpose:
;	This routine takes a character and a file handle from
;	B$OPNLPT and sends the character to the printer specified
;	by the file handle.  If there are any errors, they are
;	returned in AH.
;
;	See the documentation for B$OPNLPT for more information.
;
;Entry:
;	[AL]  = character
;	[BX]  = file handle
;
;Exit:
;	[AH]  = 0: success
;		1: device not available
;		2: time out
;		3: out of paper
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;
;******************************************************************************
cProc	B$SNDLPT,<NEAR,PUBLIC>,<DX,CX> 
cBegin				
	PUSH	AX		; store byte in stack
	MOV	DX,SP		; [DS:DX] points to data to be output
	.ERRE	ID_SSEQDS	; assumes DS=SS
        MOV     CX,1            ;[CX] = # of bytes to be written
        MOV     AH,40H          ;read operation
        INT     21H             ;success reflected in carry
	POP	DX		; even stack
        JC      SNDLPT_UNAVAIL  ;jump if error on write
        XOR     AH,AH           ;return 0 in AH for no error
        JMP     SHORT SNDLPT_DONE ;jump to finish up
SNDLPT_UNAVAIL:
        MOV     AH,1            ;set error code
SNDLPT_DONE:

cEnd				

;***
; B$CLSLPT - Close selected printer
;OEM-interface routine
;
;Purpose:
;	This routine takes a file handle prepared by B$OPNLPT
;	for a line printer and closes the associated communications
;	channel.  All needed termination for the printer should be
;	done at this point.  All errors are ignored.
;
;	See the documentation for B$OPNLPT for more information.
;
;Entry:
;	[BX]  = file handle
;
;Exit:
;	None
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None.
;******************************************************************************
cProc	B$CLSLPT,<NEAR,PUBLIC>,AX 
cBegin				

        MOV     AH,3EH          ;system code for closing
        INT     21H             ;close the file opened

cEnd				

sEnd	DV_TEXT 		

        END
