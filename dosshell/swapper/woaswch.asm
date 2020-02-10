;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This module has all the routines necessary for implementing the hot key    ;
; switch functions of windoldap. It takes three parameters on entry:	     ;
;									     ;
;      1.   SwitchType	   --- ALT_TAB,SHIFT_ALT_TAB,ALT_ESC etc.            ;
;      2.   CurrentWindow  --- window which was executing till now.          ;
;      3.   WindowState    --- whether to mark the window as dead or not.    ;
;									     ;
; This function module returns 3 information:				     ;
;									     ;
;      1.   NextWindow     --- handle of window to activate next	     ;
;      2.   SwapPathId	   --- swap path ID for the next window		     ;
;      3.   CarryFlag      --- set to indicate, the window belongs to OldApp ;
;									     ;
; The functions that this module has to do are:				     ;
;								             ;
;     . If CTRL_ESC is to be processed, simply return the a NULL handle	     ;
;     . Load the windows title information table from the windows swap file  ;
;	into the area pointed to by the low heap selector.                   ;
;     . Locate the node belonging to the current window in that table and if ;
;       it is to be marked as dead do so.			             ;
;     . Depending on whether we have to go back or forward, locate the next  ;
;       or the previous window node.					     ;
;     . If the switch type is ALT_ESC or SHIFT_ALT_ESC, return with the wind-;
;       -ow handle and set carry flag if the app is an old app.		     ;
;     . If we have to process a TAB type of switch, get into a loop, display-;
;       -ing a switcher screen with the current title and current set of     ;
;       colors and loop back when a TAB is pressed and ALT is still down     ;
;     . return with the handle and type of app in the current node once the  ;
;       ALT key is released.						     ;
;									     ;
; History:								     ;
;									     ;
;	 Tue Nov-113-1990.      -by-  Amit Chatterjee [amitc]		     ;
;	 * UnHookIn09 uses 'Hint09ChainAddr' instead of 'ActualInt09'	     ;
;									     ;
;        Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 * Modified the switcher interface to work with the shell interface. ;
;	 * Modified the code to use the StubSeg's INT 9 hook.		     ;
;									     ;
;        Tue June-20-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created. (Added the History legend)				     ;
;----------------------------------------------------------------------------;



	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include	njmp.mac
	include woaerr.inc
	include woakeys.inc
	include woaswch.inc
	include macros.mac
	.list

	.8086

sBegin	Data

;----------------------------------------------------------------------------;
; declare the global variables defined elsewhere	  		     ;
;----------------------------------------------------------------------------;

externB	SwitcherColors			;colors for switcher screen
externW	hApp				;ID of the app
externB NodeToSwitchTo			;for directed hot key switch
externB	ReturnToShellCode		;code returned to the shell

	;-------------------------------------------------------;
	; define any locally used variables here.		;
	;-------------------------------------------------------;

TrackTabNode	dw	?		;cuurent position of TAB task pointer
NumApps		dw	0		;number of apps in the list

	;-------------------------------------------------------;
	; define public names here.				;
	;-------------------------------------------------------;


	;-------------------------------------------------------;
	; define any locally used constants			;
	;-------------------------------------------------------;


	;-------------------------------------------------------;
	; define any external contants.				;
	;-------------------------------------------------------;


;---------------------------------------------------------------------------;
; now define the other global variables that will be needed.		    ;
;---------------------------------------------------------------------------;

sEnd	Data
;----------------------------------------------------------------------------;
; declare code and data in the StubSegment that are referenced here.	     ;
;----------------------------------------------------------------------------;

sBegin	StubSeg

	externFP  Int09ISR		;(WOARLM.ASM)

externD		HInt09ChainAddr		;original INT 09 handler
externB		WoaHotKeyState		;INT 9 hook keeps key state here

sEnd	StubSeg
;----------------------------------------------------------------------------;

sBegin	Code

	assumes	cs,Code
	assumes	ds,Data
	assumes	es,nothing

;---------------------------------------------------------------------------;
; define the external function calls.					    ;
;---------------------------------------------------------------------------;

	;-------------------------------------------------------;
	; external OLDAPP procedures.			        ;
	;-------------------------------------------------------;

	externNP OpnFile		;(WOAFILE.ASM)
	externNP CloseFile		;(WOAFILE.ASM)
	externNP ReadFile		;(WOAFILE.ASM)
	externNP WriteFile		;(WOAFILE.ASM)
	externNP SetNormalAttributes	;(WOAFILE.ASM)
	externNP SetHiddenAttributes	;(WOAFILE.ASM)

;----------------------------------------------------------------------------;

cProc	SwitchManager,<NERA,PUBLIC,PASCAL>,<es,ds,si,di>

	parmW	SwitchType		;type of switch
	parmW	hWindow			;window which is active

	localB  FreshTitle		;back ground to be drawn or not

cBegin

	mov	TrackTabNode,0		;TAB pointer always starts at top
	mov	FreshTitle,0ffh		;back ground need to be drawn

	cmp	NodeToSwitchTo,-1	;normal switch ?
	jz	ConventionalSwitch	;yes

; we have a directed hot key switch.

	xor	bh,bh
	mov	bl,NodeToSwitchTo	;get the logical node num
	mov	ax,4a05h		;opcode
	mov	si,CTOTOP_ITH_LIST_PE	;get it to the top
	int	2fh			;dx:ax has the long pointer to node
	mov	es,dx
	mov	di,ax			;get pointer in es:di
	jmp	short DisplaySwitchTitle;display the title and be done

ConventionalSwitch:


; test for a valid Z-Order list.

	mov	ax,4a05h		;opcode
	mov	si,CGET_LIST_LENGTH	;get the no of programs.
	int	2fh			;dx:ax has the long pointer
	or	ax,ax			;0 means 1
	jnz	@f			;more than 1 (can't be empty)
	jmp	SwitchManagerExit	;return back, carry is clear
@@:

	mov	NumApps,ax		;save number of apps.
	test	SwitchType,WOA_CTRL_ESC ;is it a control esc type ?
	jz	@f			;no.

; we need to do a control esc type of switch, just return 0 to invoke
; the desktop manager. However before that we should bring the shell to
; the top of the list.

	xor	ax,ax			;internal ID for the shell.
	cCall	AppToTheTop,<ax>	;bring shell to the top
	xor	ax,ax			;return NULL
	clc				;not a old app
	jmp	SwitchManagerExit	;return back
@@:

	cCall	HookInt09		;hook the int 9 vector

DoTheSwitch:

; update the list depending on switch type. Returns pointer of Node to 
; display title from in DX:AX

	cCall	UpdateTaskList,<SwitchType>
	mov	es,dx			
	mov	di,ax			;es:[di] -> top entry
	jmp	short DisplaySwitchTitle

MoveTabTaskToTop:

; if the switch type is ALT_TAB type then we must move the original top node
; into the second position.

	test	SwitchType,WOA_SWITCH_TAB
	jz	SwitchDone		;not TAB type

; move the app pointed by the TAB pointer to the top.

	mov	bx,TrackTabNode		;current position of original top
	or	bx,bx			;currently at top ?
	jz	SwitchDone		;leave it there.
	mov	ax,4a05h		;opcode
	mov	si,CTOTOP_ITH_LIST_PE	;get it to the top
	int	2fh			;dx:ax has the long pointer to node
	mov	es,dx
	mov	di,ax			;get pointer in es:di

SwitchDone:

; we must return with the handle and the application type of the one to 
; switch to. If the program flags say it is the shell, we must go back to 
; shell.

	test	es:[di].Program_Flags,F_SHELL
	jz	@f			;not shell.
	clc				;go back to shell
	jmp	short SwitchManagerRet	;return back to effect switch
@@:
	mov	ax,es:[di].Program_Id	;get program id

; if the program ID is 0 we are trying to restart an app that has never been
; started before. In this case we will return to shell with an error code.

	or	ax,ax			;valid ID ?
	jnz	@f			;yes.
	mov	al,ER_APP_NOT_STARTED_YET
	mov	ReturnToShellCode,al	;save as return code
	clc				;go back to shell
	jmp	short SwitchManagerRet	;return back to effect switch
@@:
	mov	bl,es:[di].Path_Id	;get swap path id	
	stc				;can resume app.
	jmp	short SwitchManagerRet	;return back to effect switch

DisplaySwitchTitle:

; get to the start of the title string and it's length.

	lea	si,[di].Program_Title	;get to the name of the program.
	mov	bx,si			;save it here
	xor	cx,cx			;initialize 

;get to the length of the string.

@@:
	mov	al,es:[si]		;load the next byte
	inc	si			;next byte
	inc	cx			;one more character
	or	al,al			;NULL recahed ?
	jnz	@b			;no.

; cx has the length of the string

	mov	ax,cx			;get the length of the title
	mov	cx,DataOFFSET SwitcherColors

; display the title of the window

	cCall	DisplayTitleBar,<es,bx,ax,cx,FreshTitle>
	mov	FreshTitle,0		;no more back ground drawing

; if this is not a TAB type of switch we are all done and the top node address
; is also in es:di

	test	SwitchType,WOA_SWITCH_TAB
	jz	CompleteTheSwitch	;done with the switch

; now wait till ALT is released, or tab is pressed

	cCall	StallOnSwitch		;wait for event
	jnc	CompleteTheSwitch	;switch finished

; update the switch type and start all over again.

	mov	SwitchType,ax		;reset new switch type
	jmp	DoTheSwitch		;process switch

CompleteTheSwitch:

	cCall	GetTopAppPtr		;get a long pointer to the top app
	mov	es,dx			
	mov	di,ax			;es:[di] -> top entry
	jmp	MoveTabTaskToTop	;process switch

SwitchManagerRet:

	push	ax
	pushf				;save the resturn value
	cCall	UnHookInt09		;unhook the vector
	popf
	pop	ax			;restore return values

SwitchManagerExit:

cEnd
;----------------------------------------------------------------------------;
; UpdateTaskList:							     ;
;									     ;
; Updates the task list based on  the type of the switch. Done by INT 2FH    ;
; calls to the shell stub. For ALT_TAB type of switch the list is not altered;
; at all. This routine returns the address of the next node in DX:AX	     ;
;----------------------------------------------------------------------------;

cProc UpdateTaskList,<NEAR,PUBLIC,PASCAL>,<si,di,cx>

	parmW	SwitchType		;type of the switch

cBegin

; move the next or previous guy to the top of the list.

	test	SwitchType,WOA_SWITCH_TAB
	jnz	UpdateTabType		;TAB type of switch.

	mov	si,CGO_NEXT		;absolute ordering
	test	SwitchType,WOA_SWITCH_NEXT;is it true ?
	jnz	@f

	mov	si,CGO_Z_PREV		;get last into first poition
@@:
	mov	ax,4a05h		;opcode
	int	2fh			;this will do it

; now get a pointer to this node.

	cCall	GetTopAppPtr		;DX:AX points to TOP APP
	jmp	short UpdateTaskListRet

UpdateTabType:

	mov	ax,TrackTabNode		;current pos of TAB task pointer
	mov	bx,1			;assume we TAB forward
	xor	cx,cx			;value after wrap around at NumApps
	test	SwitchType,WOA_SWITCH_NEXT;is it true ?
	jnz	@f

	mov	bx,-1			;we will have to go back
	mov	cx,NumApps			;value after wrap around at 0
@@:
	add	ax,bx			;new position of TAB task pointer
	cmp	ax,NumApps		;wrapped over ?
	jbe	@f			;no.
	mov	ax,cx			;get right value after wrap around
@@:
	mov	TrackTabNode,ax		;poition of new TAB pointer

; get a pointer to this node.

	mov	bx,ax			;get it in BX
	mov	ax,4a05h
	mov	si,CGET_ITH_ENTRY_DATA	;returns pointer in ES:SI
	int	2fh
	mov	dx,es
	mov	ax,si			;have it in DX:AX

UpdateTaskListRet:
	
cEnd
;----------------------------------------------------------------------------;
; GetTopAppPtr:								     ;
;									     ;
; Gets a pointer to the top app in the list and returns it in DX:AX.	     ;
;----------------------------------------------------------------------------;

cProc	GetTopAppPtr,<NEAR,PUBLIC,PASCAL>,<es,di,si>

cBegin

	mov	ax,4a05h		;opcode
	mov	si,CGET_GLOBAL_SWITCH_DATA
	int	2fh			;dx:ax has the long pointer
	mov	es,dx			;get it in es
	mov	di,ax			;es:di -> start of structure

; get a pointer to the first program in the list.

	lea	si,[di].Program_List	;start of array
	xor	ah,ah			;clear out high byte
	mov	al,es:[di].First_In_List;get index of first entry
	mov	bl,SIZE Switch_Entry	;size of each entry
	mul	bl			;ax has start offset
	add	ax,si			;get the offset
	mov	dx,es			;get the segment

cEnd
;----------------------------------------------------------------------------;
; DeleteApp:								     ;
;									     ;
; This takes a Program_ID as the paramete and deletes the app from the list. ;
;----------------------------------------------------------------------------;

cProc	DeleteApp,<NEAR,PUBLIC,PASCAL>,<es,di,si>

	parmW	Id			;ID of app to delete

cBegin

; get the logical node number (ignore DX:AX from following routine)

	cCall	GetSwitcherEntry,<Id>	;bx returns logical node number
	jc	DeleteAppRet		;could not get node

; delete the entry.

	mov	ax,4a05h		;opcode
	mov	si,CDELETE_PROGRAM_FROM_LIST
	int	2fh			;deletes it

DeleteAppRet:

cEnd
;----------------------------------------------------------------------------;
; GetSwitcherEntry:							     ;
;									     ;
; This routine takes an AppID as the parameter and gets to the app that has  ;
; that id. DX:AX returns a pointer to the entry.			     ;
;									     ;
; It returns the logical node number in BX.				     ;
;----------------------------------------------------------------------------;

cProc	GetSwitcherEntry,<NEAR,PUBLIC,PASCAL>,<es,si,di>

	parmW	GSEAppId		;id of app we are looking for

cBegin

; get a pointer to the global block.

	mov	ax,4a05h		;opcode
	mov	si,CGET_GLOBAL_SWITCH_DATA
	int	2fh			;dx:ax has the long pointer
	mov	es,dx			;load it into es
	mov	di,ax			;es:di -> info structure

; scan through the list in search for the ID. We must walk in the list order.

	xor	ah,ah			;zero out for 'mul' below
	mov	al,es:[di].First_In_list;get the first entry
	mov	bl,SIZE Switch_Entry	;size of each entry
	mul	bl			;ax has the node offset
	lea	si,[di].Program_List	;es:si points to the first program entry
	add	si,ax			;point to the right node.
	mov	cx,MAX_NUM_PROGRAMS	;get the max number of entries
	xor	dx,dx			;logical node number

LoopAllEntries:

	mov	ax,es:[si].Program_Id	;get the ID
	cmp	ax,GSEAppId		;does it match ? 
	jz	GetSwitcherEntryRet	;yes, break out of loop

ContinueLoop:

	xor	ah,ah			;zero out
	mov	al,es:[si].Next_In_List	;get the next entry
	mul	bl			;ax has the offset from the start
	lea	si,[di].Program_List	;es:si points to the first program entry
	add	si,ax			;point to the right node.
	inc	dx			;one more fresh entry obtained
	loop	LoopAllEntries		;continue looking
	stc				;error

GetSwitcherEntryRet:

	mov	bx,dx			;logical node number
	mov	dx,es			;segment of node
	mov	ax,si			;offset of node

cEnd
;----------------------------------------------------------------------------;
; AppToTheTop:								     ;
;									     ;
; This routine moves an app task to the top of the list. To do this it       ;
; must first find out the logical node number of the app and make an INT 2Fh ;
; call to move the logical node to the top. App ID=0 signifies the shell.    ;
;									     ;
; This destroys all registers but for ES,DS and BP.			     ;
;----------------------------------------------------------------------------;

cProc	AppToTheTop,<NEAR,PUBLIC,PASCAL>,<es>

	parmW	ATTT_AppID		;id of the app to move to the top
	
	localW	ATTT_NodeNum		;node number of the app.

cBegin

; get a pointer to the glocal structure.

	mov	ax,4a05h		;opcode
	mov	si,CGET_GLOBAL_SWITCH_DATA
	int	2fh			;dx:ax has the long pointer
	mov	es,dx			;load it into es
	mov	di,ax			;es:di -> info structure

; walk through the list in node order looking for the app.

	xor	ah,ah			;zero out for 'mul' below
	mov	al,es:[di].First_In_list;get the first entry
	mov	bl,SIZE Switch_Entry	;size of each entry
	mul	bl			;ax has the node offset
	lea	si,[di].Program_List	;es:si points to the first program entry
	add	si,ax			;point to the right node.
	mov	cx,MAX_NUM_PROGRAMS	;get the max number of entries
	mov	ATTT_NodeNum,0		;start with node 0.

ATTT_Loop:

; is this a free node ?

	test	es:[si].Program_Flags,F_FREE
	jnz	ATTT_NextNode		;it is free

; are we checking for the shell ?

	cmp	ATTT_AppID,0		;looking for shell ?
	jz	ATTT_LookForShell	;yes.

; is this the right ID ?

	mov	ax,es:[si].Program_Id	;get the ID of the node
	cmp	ax,ATTT_AppID		;match ?
	jz	ATTT_FoundApp		;we found it
	jmp	short ATTT_NextNode	;move on.

ATTT_LookForShell:

; Is this the shell node ?

	test	es:[si].Program_Flags,F_SHELL
	jnz	ATTT_FoundApp		;yes, we found it.

ATTT_NextNode:

; move on to the next node.

	xor	ah,ah			;zero out
	mov	al,es:[si].Next_In_List	;get the next entry
	mul	bl			;ax has the offset from the start
	lea	si,[di].Program_List	;es:si points to the first program entry
	add	si,ax			;point to the right node.
	inc	ATTT_NodeNum		;one more fresh entry obtained
	loop	ATTT_Loop		;continue looking

; we did not get a match. This should never happen. In any case we will not
; do any further in thie routine.

	jmp	short ATTT_Ret

ATTT_FoundApp:

	mov	bx,ATTT_NodeNum		;get the node number
	or	bx,bx			;is it already at the top ?
	jz	ATTT_Ret		;yes, nothing more to do.

; move the node to the top of the task list.

	mov	ax,4a05h		;opcode
	mov	si,CTOTOP_ITH_LIST_PE	;get it to the top
	int	2fh			;dx:ax has the long pointer to node

ATTT_Ret:

cEnd
;----------------------------------------------------------------------------;
; UpdateExitCode:							     ;
;								             ;
; This routine stuffs in the error code into the global structure. The main  ;
; error code and the subcode are passed in as paramets to this routine.	A    ;
; main code of 0 actually menas no error.				     ;
;----------------------------------------------------------------------------;

cProc	UpdateExitCode,<NEAR,PUBLIC,PASCAL>,<es,di>

	parmB	MainCode		;main error code
	parmB	SubCode			;auxilliary error code

cBegin

	mov	ax,4a05h		;opcode
	mov	si,CGET_GLOBAL_SWITCH_DATA
	int	2fh			;dx:ax has the long pointer
	mov	es,dx			;load it into es
	mov	di,ax			;es:di -> info structure

; get the error code.

	mov	ah,MainCode	
	mov	al,SubCode
	mov	es:[di].Exit_Code,ax	;stuff it in.

cEnd
;----------------------------------------------------------------------------;
; DisplayTitleBar:							     ;
;									     ;
; This displays a title bar of an application. It takes as parameters, a long;
; pointer to a string to display, and a pointer in the data segment to four  ;
; textmode color attributes, for the desktop background color, title bar     ;
; color, text foreground and background colors and displays the switcher     ;
; screen. It also takes the length of the string.			     ;
;									     ;
; The background of the switcher screen would drawn if 'FreshTitle' is set   ;
; to 0ffh (it needs to be drawn only once).				     ;
;----------------------------------------------------------------------------;

cProc DisplayTitleBar,<NEAR,PUBLIC,PASCAL>,<si,di>

	parmD	lpText			;text for the title
	parmW	Count			;length of the string
	parmW   pColorBlock		;near pointer to color block
	parmB	FreshTitle		;background to be drwan or not

cBegin

	mov	si,pColorBlock		;load near pointer to color block

; Skip background painting if not FresTitle

	cmp	FreshTitle,0		;first time being done ?
	jz	@f			;no.	

; position on line  row 0, coloumn 0

	mov	ah,2			;set cursor position code
	mov	bh,0			;page 0
	xor	dx,dx			;HOME the cursor
	int	10h
	mov	ax,40h			;want to access BIOS data area
	mov	es,ax			;es has bios data segment
	mov	al,24			;24 (25-1) lines on screen.
	mov	bl,es:[4ah]		;get the number of coloumns
	inc	ax			;no of rows is one more
	mul	bl			;ax has the no of coumns
	mov	cx,ax			;move it into cx


; now blank out the screen with the background color

	mov	bh,0			;want to do page 0 only
	mov	bl,[si]			;get the background color attribute
	mov	ax,0920h		;display sapce code
	int	10h			;background displayed

@@:

; position on line  row 1, coloumn 2

	mov	ah,2			;set cursor position code
	mov	bh,0			;page 0
	mov	dx,102h			;row 1 coloumn 2
	int	10h

; we want to center the text in the title bar. Figure out how many chars
; should preceed title

	mov	ax,Count		;get length including NULL
	dec	ax			;just the length
	cmp	ax,76			;is it more than 76 chars ?
	jbe	@f			;no
	mov	ax,76			;can write only 80
@@:
	mov	Count,ax		;number we will write
	mov	cx,76			;length of a row
	sub	cx,ax			;# of spaces on both sides
	push	cx			;save it
	shr	cx,1			;halve it
	adc	cx,0			;if uneven, 1 more on left
	jcxz	@f			;no blank on the left

; display blanks on the left with active caption background color

	push	cx			;save count
	mov	ax,920h			;display space code
	mov	bh,0			;in page 0
	mov	bl,[si+1]		;title bar color
	int	10h			;left of text done
	pop	cx			;get back no of spaces displayed

; now position cursor for text string

	mov	bh,0			;page 0
	mov	dh,1			;row 1
	mov	dl,cl			;start of title
	add	dl,2			;2 blanks at left edge
	mov	ah,2			;set cursor position code
	int	10h			;positioned
@@:

; now display the character string.

	mov	bl,[si+2]		;get text foreground attribute
	or	bl,[si+1]		;or it with back ground of caption bar
	mov	bh,0			;want in page 0
	les	di,lpText		;load pointer to string
	mov	cx,Count		;get number to write
	jcxz	title_displayed		;could be null title

write_character_loop:

	push	cx			;save count
	mov	al,es:[di]		;get the next character
	inc	di			;point to next character
	mov	ah,09h			;display character code
	mov	cx,1			;want to write one character
	int	10h			;character written
	mov	ah,3			;get cursor position code
	int	10h			;dl has coloumn
	inc	dl			;position to next coloumn
	mov	ah,2			;set cursor code
	int	10h			;positioned for next
	pop	cx			;get back count
	loop	write_character_loop	;write out title bar

title_displayed:
		
	pop	cx			;get back number of spaces
	shr	cx,1			;number of spaces to display on left
	jcxz	@f			;none on the right

; display spaces on the right

	mov	ax,920h			;display space code
	mov	bh,0			;in page 0
	mov	bl,[si+1]  		;title bar color
	int	10h			;spaces displayed
@@:

; now make the cursor invisible. The CH=20h/AH=1 call may not work in some 
; cases, so we will set the cursor to the 26th line on the screen and there
; by make it invisible.


	mov	ah,02h			;set cursor position
	xor	bh,bh			;page 0
	mov	dx,1900h		;to the 26th line
	int	10h			;cursor made invisible

cEnd

;----------------------------------------------------------------------------;
; StallOnSwitch:							     ;
;									     ;
; This routine loops for some sort of keyboard action as long as ALT key is  ;
; done. The variable WoaHotKeys is updated by the int 09 ISR. It returns with;
; carry clear if the ALT break is sensed, else if the alt key is still down  ;
; it returns with carry set if WoaHotKeys has a valid switch move.           ;
;									     ;
; NOTE: We get the ALT key state by groping directly into the BIOS data area ;
; rather than doing INT 16H calls. This is because to service the INT 16     ;
; calls DOSX has to swith to realmode and later switch back to pmode. On some;
; 286s like WANG PC250/16 this does not work well, presumably because the    ;
; processor resets mess up the CMOS data.				     ;
;----------------------------------------------------------------------------;

cProc	StallOnSwitch,<NEAR,PUBLIC,PASCAL>,<si,di,es,bx,dx>

cBegin

	mov	ax,40h			;bios data area
	mov	es,ax			
	mov	bx,17h			;es:[bx] has the keyboard state flags
	mov	dx,_WOARLMSEG		;hot keys in stub segment
  	
StallOnSwitchLoop:

; get key board shift status and test if ALT is down or not

	test	bptr es:[bx],ST_ALT	;get the keyboard status
	jz	@f			;it is nomore down

; test to see if a valid switch move has been made or not

	push	es
	mov	es,dx			;load the stubsegment
	assumes	es,StubSeg
	test	WoaHotKeyState,WOA_SWITCH;valid switch move
	pop	es
	assumes	es,nothing
	jz	StallOnSwitchLoop	;no,linger here

; valid move has been made, return with the move

	push	es			;save
	mov	es,dx			;go to stub segment
	assumes	es,StubSeg
	xor	al,al			;must reset it before next call
	xchg	al,WoaHotKeyState	;get the move
	and	al,WOA_SWITCH		;just interested in switch moves
	xor	ah,ah			;AX has move value
	pop	es			;restore
	assumes	es,nothing
	stc				;set carry for new move
	jmp	short StallOnSwitchRet	;go back

@@:
	clc		  		;ALT key has been released

StallOnSwitchRet:


cEnd
;----------------------------------------------------------------------------;
; This routine hooks the int 09 vector and leaves some information in the    ;
; code segment which the ISR needs to handle.				     ;
;----------------------------------------------------------------------------;

cProc	HookInt09,<NEAR<PASCAL>,<di,si>

cBegin
    
	push	es			;save
	mov	ax,_WOARLMSEG		;StubSegment
	mov	es,ax			;load the code seg
	assumes	es,StubSeg

; the original INT 09 address is already in the 'HInt09ChainAddr' variable in the
; StubSeg

; hook the ISR.

	push	ds			;save them
	smov	ds,es			;hook is in StubSeg
	assumes	ds,StubSeg		
	mov	dx,StubSEGOFFSET Int09ISR;ds:dx points to the ISR
	mov	ax,2509h		;want to set vector 9
	int	21h
	mov	WoaHotKeyState,0	;reset hot key state
	pop	ds			;restore 
	assumes	ds,Data

	pop	es			;restore
	assumes	es,nothing
	

cEnd
;----------------------------------------------------------------------------;
; This routine unhooks the Int 09 ISR that we pu in. 			     ;
;----------------------------------------------------------------------------;

cProc	UnHookInt09,<NEAR<PASCAL>

cBegin

	pushem	es,ds			;save
	mov	ax,_WOARLMSEG		;get StubSeg value
	mov	es,ax			;load it in es
	assumes	es,StubSeg
	assumes	ds,nothing

; get and set the orginal vector 

	mov	ds,wptr es:[HInt09ChainAddr+2]
	mov	dx,wptr es:[HInt09ChainAddr];DS:DX has the original vector
	mov	ax,2509h		;want to set vector for int 09
	int	21h
	popem   es,ds			;restore them	
	assumes	es,nothing
	assumes	ds,Data

cEnd
;----------------------------------------------------------------------------;
sEnd Code

end





