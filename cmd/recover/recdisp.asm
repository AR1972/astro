;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
page	,132				; ;AN000;bgb
;;*****************************************************************************
; Include files
;*****************************************************************************
;
.xlist
	include pathmac.inc							;an028;bgb
	include recseg.inc		;AN000;bgb
	include dossym.inc
	INCLUDE SYSMSG.INC
	INCLUDE RECEQU.INC
	INCLUDE RECMSG.INC
.list
;
;*****************************************************************************
; external data
;*****************************************************************************
data	segment public para 'DATA'      ;AC000;bgb
	EXTRN	command_line_buffer:byte					;an031;bgb
	extrn	DrvLet1:Byte		;AN000;bgb
	extrn	DrvLet:Byte	       ;AN000;bgb
	extrn	rec_num:word	       ;AN000;bgb
	Extrn	Drive_Letter_Msg:Byte	;AN000;BGB
	extrn	fname_buffer:byte	;AN000;BGB
	EXTRN	x_value_lo:WORD 	;AN000;bgb	   ; AC000;SM
	EXTRN	x_value_hi:WORD 	;AN000;bgb	   ; AC000;SM
	EXTRN	y_value_lo:WORD 	;AN000;bgb	   ; AC000;SM
	EXTRN	y_value_hi:WORD 	;AN000;bgb	   ; AC000;SM
	extrn	DriveLetter:Byte

	inflag		db	0
;*****************************************************************************
; Message Services
;*****************************************************************************
.xlist
MSG_UTILNAME  <RECOVER>
Msg_Services	<MSGDATA>
Msg_Services	<INPUTmsg>
data	ends


;
;***************************************************************************
; Message Structures
;***************************************************************************
;
code	segment PUBLIC para 'CODE'
Message_Table struc				;				;AN000;
						;
Entry1	dw	0				;				;AN000;
Entry2	dw	0				;				;AN000;
Entry3	dw	0				;				;AN000;
Entry4	dw	0				;				;AN000;
Entry5	db	0				;				;AN000;
Entry6	db	0				;				;AN000;
Entry7	dw	0				;				;AN000;
Message_Table ends				;				;AN000;

pathlabl	msgret								;an028;bgb
Msg_Services	<NEARmsg>
Msg_Services	<LOADmsg>
Msg_Services	<DISPLAYmsg,CHARmsg,NUMmsg>					;an029;bgb
pathlabl	msgret								;an028;bgb
Msg_Services	<RECOVER.CLA,RECOVER.CL1,RECOVER.CL2,RECOVER.CTL>
.list
;
;*****************************************************************************
; Public Routines
;*****************************************************************************
	Public	SysDispMsg
	Public	SysLoadMsg
	Public	Display_Interface

;*****************************************************************************
;Routine name&gml Display_Interface
;*****************************************************************************
;
;DescriptioN&gml Save all registers, set up registers required for SysDispMsg
;	      routine. This information is contained in a message description
;	      table pointed to by the DX register. Call SysDispMsg, then
;	      restore registers. This routine assumes that the only time an
;	      error will be returned is if an extended error message was
;	      requested, so it will ignore error returns
;
;Called Procedures: Message (macro)
;
;Change History&gml Created	   4/22/87	   MT
;
; More change history:  this routine, until 6/90, had no means of displaying
; messages which would generate & need to return a response.  (E.g. the one
; I added, in case the user specifies *.*, which says "are you sure" and wants
; a y/n response.)  Since I can't find the code for the macro which builds the
; data structures which get passed to sysdispmsg, I can't verify that a word
; actually gets reserved and pointed by di for those msgs which expect no
; responses.  Thus it might be harmful to assume that upon return from
; sysdispmsg, you can always put the value from AX into [di] (which would be
; the thing to do if you actually expected a response, and would also be
; harmless to do if not but if we knew di pointed at some unimportant place).
; Since we don't, the system I use is to check the message number - if it
; is greater than MIN_INPUT_VAL (currently 400), put ax into [di] when you
; come back from sysdispmsg.  THIS MEANS THAT ANY NEW INPUT MESSAGES SHOULD
; BE NUMBERED GREATER THAN 400 IN THE .SKL FILES AND IN USA_MS.MSG AND NO
; NON-INPUT MSGS SHOULD BE !!!!!    LeaF, June 1990
;
;Input&gml ES&gmlDX = pointer to message description
;
;Output&gml None
;
;Psuedocode
;----------
;
;	Save all registers
;	Setup registers for SysDispMsg from Message Description Tables
;	CALL SysDispMsg
;	Restore registers
;	ret
;*****************************************************************************

Display_Interface   proc			;				;AN000;

	 push	 ds				 ;				 ;AN000;
	 push	 ax				 ;Save registers		 ;AN000;
	 push	 bx				 ; "  "    "  "                  ;AN000;
	 push	 cx				 ; "  "    "  "                  ;AN000;
	 push	 dx				 ; "  "    "  "                  ;AN000;
	 push	 si				 ; "  "    "  "                  ;AN000;
	 push	 di				 ; "  "    "  "                  ;AN000;
	 mov	 di,dx				 ;Change pointer to table	 ;AN000;
	 mov	 dx,DG				 ;Point to data segment
	 mov	 ds,dx				 ;
	 mov	 ax,[di].Entry1 		 ;Message number		 ;AN000;
	 cmp	 ax, MIN_INPUT_VAL
	 jl	 skip
	 mov	 inflag, 1
skip:	 mov	 bx,[di].Entry2 		 ;Handle			 ;AN000;
	 mov	 si,[di].Entry3 		 ;Sublist			 ;AN000;
	 mov	 cx,[di].Entry4 		 ;Count 			 ;AN000;
	 mov	 dh,[di].Entry5 		 ;Class 			 ;AN000;
	 mov	 dl,[di].Entry6 		 ;Function			 ;AN000;
	 mov	 di,[di].Entry7 		 ;Input 			 ;AN000;
	 call	 SysDispMsg			 ;Display the message		 ;AN000;
	 cmp	 inflag, 1
	 jne 	 skip2
	 mov	 word ptr [di], ax               ; SAVE RETURN VALUE !  leaf
skip2:	 pop	 di				 ;Restore registers		 ;AN000;
	 pop	 si				 ; "  "    "  "                  ;AN000;
	 pop	 dx				 ; "  "    "  "                  ;AN000;
	 pop	 cx				 ; "  "    "  "                  ;AN000;
	 pop	 bx				 ; "  "    "  "                  ;AN000;
	 pop	 ax				 ; "  "    "  "                  ;AN000;
	 pop	 ds				 ;				 ;AN000;
	 ret					 ;All done			 ;AN000;
Display_Interface      endp			;				;AN000;

include msgdcl.inc

code	ends
	end


;code	 segment public PARA 'CODE'      ;AC000;bgb
;code	 ends
;
;const	 segment public para		 ;AC000;bgb
;const	 ends
;
;cstack  segment stack word 'stack'
;cstack  ends
;
;
;data	 segment public para 'DATA'      ;AC000;bgb
;data	 ends
;
;dg	 group	 code,const,data,cstack
;
;code	 segment public para 'code'      ;AC000;bgb
;code	 ends
;	 assume  cs:dg,ds:dg,es:dg,ss:cstack
;
;
;;;;;;;;;;code	  segment PUBLIC para 'CODE'
;;;;;;;;;;;;;code    ends
