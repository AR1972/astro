;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


MAX_NUM_PROGRAMS equ 16
MAX_PROGRAM_LENGTH equ 80
MAX_TITLE  equ 30

; DONOT CHANGE THE SIGNIFICANCE OF THE BIT FLAGS WITHOUT MAKING CORRESPONDING
; CHANGES IN THE SWITCHER INCLUDE FILE WOAPIF.INC

F_NO_SWITCH	   equ 0000000000000001b ;program can not be switched away from
F_GRAPHICS	   equ 0000000000000010b ;graphics mode
F_NO_PAUSE         equ 0000000000000100b ;Pause after exit
F_DTS_API_INITED   equ 0000000000001000b ;1=>DTS init has been posted by shell
F_SWAP_NOT_OK	   equ 0000000000010000b ;1=>external tasker incompatble; set by shell
F_UNUSED_3	   equ 0000000000010000b ;
F_UNUSED_4	   equ 0000000000100000b ;
F_SHELL		   equ 0000000001000000b ;program is the shell

F_NO_ALT_TAB	   equ 0000000100000000b ;ALT+TAB to be ignored
F_NO_ALT_ESC	   equ 0000001000000000b ;ALT+ESC to be ignored
F_NO_CTRL_ESC	   equ 0000010000000000b ;CTRL+ESC to be ignored
F_UNUSED_5	   equ 0000100000000000b ;
F_UNUSED_6	   equ 0001000000000000b ;
F_UNUSED_7	   equ 0010000000000000b ;
F_UNUSED_8	   equ 0100000000000000b ;
F_FREE		   equ 1000000000000000b ;free program slot

; equates for global flags

GF_INT_6F_TOBE_DONE	equ	00000001b ;INT 6f to be done
GF_IRQ9_GLOBAL		equ	00000010b ;IRQ 9 to be globally handled
GF_NET_ASYNC_SWITCH_OK	equ	00000100b ;OK to switch out on async net request
GF_UNUSED_1		equ	00001000b ;
GF_UNUSED_2		equ	00010000b ;
GF_UNUSED_3		equ	00100000b ;
GF_UNUSED_4		equ	01000000b ;
GF_UNUSED_5		equ	10000000b ;

Switch_Entry   STRUC
       Program_Name    db MAX_PROGRAM_LENGTH  DUP(?); Program name
       Program_Title   db MAX_TITLE DUP (?)	 ; switcher screen title
       Conv_Req	       dw ?			 ; min conv mem required
       XMS_Req	       dw ?			 ; XMS required
       XMS_Want	       dw ?			 ; XMS desired
       HK_Scan_Code_1  db ?			 ; 1st directed hot key scan code
       HK_Scan_Code_2  db ?			 ; 2nd directed hot key scan code
       HK_Shift_State  db ?			 ; ALT/CTRL/SHIFT states
       Program_Flags   dw ?			 ; Special flags for program
       Next_In_List    db ?			 ; Next program in Z order list
       Program_Id      dw ? 			 ; internal program ID
       Path_Id	       db ?			 ; internal id of swap path
       Shell_Cookie    dw ?			 ; magic cookie for shell		
Switch_Entry   ENDS

Switch_Info    STRUC
       Switcher_Id     db   ?
       CPU_Type	       dw   ?			 ; CPU type flag bits
       SFT_Size	       dw   ?			 ; SFT SIZE
       Parameters      db   130 dup (?) 	 ; parameters to the program
       Grabber_Name    db   80 dup (?) 		 ; grabber path and name
       Swap_Path1      db   68 dup (?) 		 ; first swap drive and path 
       Swap_Path2      db   68 dup (?)		 ; second swap drive & path
       Min_Path1       dw   ?			 ; min k space to be left on drv1
       Min_Path2       dw   ?			 ; min k space to be left on drv2
       XMS_Handle      dw   ?			 ; Handle of locked XMS block
       XMS_Size	       dd   ?			 ; size of block in bytes
       Int_15_Users_Id dw   ?			 ; ID of app using INT 15 memory
       Id_Serial       dw   ?			 ; running app serial number
       Exit_Code       dw   ?			 ; switcher exit code
       Num_Lines       db   ?                    ; start up screen lines
       Global_Flags    db   ?			 ; various global descisions
       Screen_Back     db   ?			 ; back ground screen color
       Title_Fore      db   ?			 ; Title text color
       Title_Back      db   ?			 ; Title back ground color
       Num_Programs    db   ?			 ; number of programs in the list
       First_In_List   db   ?			 ; first program in Z order list
       Program_List    db SIZE Switch_Entry *MAX_NUM_PROGRAMS DUP(?)
Switch_Info    ENDS

CINIT_PROGRAM_LIST	  equ 0
CADD_PROGRAM_TO_LIST	  equ 1
CGO_Z_NEXT		  equ 2
CGO_Z_PREV		  equ 3
CDELETE_PROGRAM_FROM_LIST equ 4
CGO_NEXT		  equ 5
CGET_ITH_PROGRAM_STRING   equ 6
CGET_LIST_LENGTH	  equ 7
CGET_GLOBAL_SWITCH_DATA   equ 8
CGET_ITH_ENTRY_DATA	  equ 9
MAX_HANDLER_CALL	  equ 9
CADD_PARAMS		  equ 10
CGET_EXITCODE		  equ 11
CTOTOP_ITH_LIST_PE 	  equ 12

