;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */

  
;----------------------------------------------------------------------------;
; This file implements all the switch API functions.			     ;
;									     ;
; History:								     ;
;									     ;
;	 Tue Nov-13-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 'SwitchAPICallIn' returns with carry set for unsupported call-ins.  ;
;									     ;
;        Thu Aug-23-1990.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created for Switcher. (Added the History legend)    		     ;
;----------------------------------------------------------------------------;


	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include macros.mac
	include njmp.mac
	include woaswapi.inc
	include woakeys.inc
	.list

	.8086

;----------------------------------------------------------------------------;
; define any public labels or eqautes here.				     ;
;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;

createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin	StubSeg

	assumes	cs,StubSeg
	assumes	ds,StubSeg

;----------------------------------------------------------------------------;
; declare public labe and names.					     ;
;----------------------------------------------------------------------------;

			public	SwitchAPICallIn

;----------------------------------------------------------------------------;
; declare variables defined in other files.				     ;
;----------------------------------------------------------------------------;

externW		WoahApp			;App's ID
externB		SwitcherName		;name of the switcher
externW		SwitcherDisabled	;code why switcher is disabled
externB		WoaSwitcherID		;ID of this switcher
externB		WoaHotKeyState		;type of hot key pressed
externW		WoaNodeToSwitchTo	;next node that we want to switch to
externB		WoaNetAsyncSwitching	;Network to be monitored or not
externB		AsyncNetPending		;asynchronous requests seen or not
externW		WoaSwapAreaParaSize	;size of the swap area

;----------------------------------------------------------------------------;
; define data areas that we need in this file.				     ;
;----------------------------------------------------------------------------;

Our_Ver_Struc  	Switcher_Ver_Struc <>	;our version information
Our_NB_API_Info	API_Info_Struc	<>	;our NetBios handler details.

lpCallBackChain		dd	?	;address of the call back SCBI list
CallBackAddrValid 	db	0	;above addr valid or not (not by default)

;----------------------------------------------------------------------------;
; declare external constants.						     ;
;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;
; define local constants.						     ;
;----------------------------------------------------------------------------;

SD_SWAPI_DISABLE  equ	04h		;disabled by another switcher

;----------------------------------------------------------------------------;
; define the call-in jump table.				             ;
;----------------------------------------------------------------------------;

SwitchAPICallInTable label word

	dw	GetVersion		;AX=0
	dw	TestMemoryRegion	;AX=1
	dw	SuspendSwitcher		;AX=2
	dw	ResumeSwitcher		;AX=3
	dw	HookCallOut		;AX=4
	dw	UnHookCallOut		;AX=5
	dw	QueryAPISupport		;AX=6


SWAPI_MAX_CALL_IN equ 6			;highest call number
;----------------------------------------------------------------------------;
; InitSwitcherAPI:							     ;
;									     ;
; This routine should do whatever is needed to initialize our Switcher API   ;
; support. At this momemnt it does the following:			     ;
;									     ;
;	. prepares the GetVersion return information buffer		     ;
;	. prepare a API_Info_Struc buffer for our NetBios handling details   ;
;       . finds out who is the best API handler for NetBios and if there is  ;
;	  one who is better, it sets the 'WoaNetAsyncSwitching' flag so that ;
;	  we will not mess with NetBios calls.				     ;
;									     ;
; Entry:								     ;
;       None.								     ;
; Exit:									     ;
;	None.								     ;
; Uses:									     ;
;	AX,Flags.							     ;
;----------------------------------------------------------------------------;
cProc	InitSwitcherAPI,<NEAR,PUBLIC,PASCAL>,<es,di,bx,si>

	localD	lp_ISAPI_Call		;to build a call address
cBegin

; prepare the invaliant part of the information buffer.

	smov	es,cs			;es:di -> the buffer to fill.
	mov	di,StubSegOFFSET Our_Ver_Struc
	mov	ax,OUR_API_MAJOR	;save major version of our API spec
	mov	es:[di.SVS_API_Major],ax
	mov	ax,OUR_API_MINOR	;save minor version of our API spec
	mov	es:[di.SVS_API_Minor],ax
	mov	ax,OUR_PRODUCT_MAJOR	;save major version of the switcher
	mov	es:[di.SVS_Product_Major],ax
	mov	ax,OUR_PRODUCT_MINOR	;save minor version of the switcher
	mov	es:[di.SVS_Product_Minor],ax
	xor	ah,ah			
	mov	al,WoaSwitcherID	;save the ID
	mov	es:[di.SVS_Switcher_ID],ax
	mov	ax,SwitcherDisabled	;save the state
	and	ax,SD_SWAPI_DISABLE	;isolate the disabled/enabled flag
	shiftr	ax,2			;bring it to LSB
	.errnz	SD_SWAPI_DISABLE - 4
	mov	es:[di.SVS_Flags],ax	
	mov	ax,StubSegOFFSET SwitcherName  ;get the offset of ID
	mov	wptr es:[di.SVS_Name_Ptr],ax   ;save offset 
	mov	ax,cs			       ;get the segment of name
	mov	wptr es:[di.SVS_Name_Ptr.2],ax ;save segment

; check to see if there is another switcher.

	pushem	es,di			;save
	mov	ax,SWAPI_DETECT_SWITCHER;detect switcher code
	xor	di,di			;need lots of zeros
	mov	es,di			;ES:DI = 0
	mov	bx,di			;BX = 0
	int	2fh			;make the call
	mov	ax,es			;is there another switcher ?
	or	ax,di
	jz	ISAPI_NoOther		;no.

; get the address of the previous switcher's version structure.

	mov	seg_lp_ISAPI_Call,es	;save segment of call address
	mov	off_lp_ISAPI_Call,di	;save offset of call address
	mov	ax,SWAPI_GETVERSION	;need to do a get version call
	pushf				;save interrupt flags
	cli				;interrupts disabled for call
	call	lp_ISAPI_Call		;get the address of the structure
	popf				;restore interrupt state
	mov	di,bx			;ES:DI has the address.

ISAPI_NoOther:

	mov	ax,di			;get offset of the structure
	mov	bx,es			;get the segment of the structure
	popem	es,di			;restore pointer to our structure
	mov	wptr es:[di.SVS_Prev_Switcher],ax   ;save offset 
	mov	wptr es:[di.SVS_Prev_Switcher.2],bx ;save segment

; the version structure has been prepared, prepare a buffer having details 
; about the level of our NetBios handler.

	smov	es,cs			;es:di -> the buffer to fill.
	mov	di,StubSegOFFSET Our_NB_API_Info
	mov	ax,SIZE API_Info_Struc  ;save size of the node
	mov	es:[di.AIS_Length],ax	
	mov	ax,API_NETBIOS		;save API code
	mov	es:[di.AIS_API],ax	
	mov	ax,OUR_NB_MAJOR_VER	;save major version of our support
	mov	es:[di.AIS_MAJOR_VER],ax
	mov	ax,OUR_NB_MINOR_VER	;save minor version of our support
	mov	es:[di.AIS_MINOR_VER],ax
	mov	ax,API_SL_MINIMAL	;save support level
	mov	es:[di.AIS_Support_Level],ax

; Now get the information about the best NetBios handler in the system. 
; If 'WoaNetAsyncSwitching' is 0 we will not do any checks at all since 
; we will not be handling NetBios calls then

	cmp	WoaNetAsyncSwitching,0	;No NetBios support ?
	jnz	ISAPI_Ret		;that right.
	mov	ax,SWAPI_QUERY_API_SUPPORT
	mov	bx,API_NETBIOS		;we are interested in NetBios alone
	call	QueryAPISupport		;a call in function supported by us

; compare to see if ours is the best or not. If they are equal we would still
; enforce ours since what is returned could be information about ourselves.

	mov	ax,es			;get the segment
	mov	di,cs			;get our cs
	cmp	ax,di			;is it in our segment ?
	jnz	ISAPI_NoNBSupport	;no, we will turn off NB support
	cmp	bx,StubSegOFFSET Our_NB_API_Info
	jz	ISAPI_Ret		;our's is the best, we will do NetBios

ISAPI_NoNBSupport:

; there is a better NetBios handler in the system than us. We will not handle
; any NetBios calls, that is, we will set WoaNetAsyncSwitching.

	mov	WoaNetAsyncSwitching,-1 ;no NetBios support

ISAPI_Ret:

cEnd
;----------------------------------------------------------------------------;
; BuildCallBackChain:							     ;
;									     ;
; This routine builds the chain of call back nodes.			     ;
;									     ;
; Entry: 								     ;
;	 None.							             ;
; Exit:									     ;
;       ES:BX	-- Call Back Chain.					     ;
;----------------------------------------------------------------------------;
cProc	BuildCallBackChain,<NEAR,PUBLIC,PASCAL>,<ax,cx,dx>

cBegin	

	pushf				;save interrupt state
	mov	ax,SWAPI_BUILD_CHAIN

; load ES:BX to be 0:0

	xor	bx,bx			;es:bx should be 0 at call time
	mov	es,bx

; load the call in finction address in CX:DX

	mov	dx,StubSegOFFSET SwitchAPICallIn
	mov	cx,cs

; make the INT 2FH call to build the chain of call back nodes.

	int	2fh
	popf				;restore interrupt state

cEnd
;----------------------------------------------------------------------------;
; OkToSuspend?:								     ;
;									     ;
; This call checks to see if it is ok to syspend the current app. The        ;
; following actions are done:						     ;
;									     ;
;	(1) A QuerySuspend call is done. If any one fails this call we do    ;
;	    a SessionActive call to all respondents and abort the switch     ;
;	    attempt. Else,						     ;
;	(2) We check to see whether our NetBios handler says it's OK to      ;
;	    switch or not (based on whether asynchronous calls have gone     ;
;	    through or not). If all's fine we move to step 3, else we        ;
;	    check to see if there is a better NetBios handler than us or not.;
;	    If there are none, we do a SessionActive call abd bort the       ;
;	    switch attempt. Else,					     ;
;	(3) We do a SuspendSession call. If this call is failed we abort     ;
;           our switch attempt as mentioned above, else this routine sets the;
;	    zero flag and returns, meaning thereby, it is ok to switch.	     ;
;									     ;
; Entry:								     ;
;	 None.								     ;
; Exit:									     ;
;        ZERO SET    - If the app can be suspended. Also Interrupts are      ;
;		       disabled in this case.				     ;
;      ZERO CLEAR    - If app cannot be suspended.			     ;
;----------------------------------------------------------------------------;
cProc	OkToSuspend?,<NEAR,PUBLIC,PASCAL>,<ax,bx,dx,es>

cBegin

	mov	ax,SWAPI_QUERY_SUSPEND	;code for QuerySuspend
	mov	dx,0ffffh		;call expects a return code
	sti				;interrupts on for this call
	call	MakeSwitchAPICall	;make the Switch API call
	jnz	SuspendFails		;some node failed the call
	cli				;shut off interrupts.

; have we seen any asynchronous NetBios requests go by ?

	cmp	cs:[AsyncNetPending],0	;any asynchronous net requests ?
	jz	OKTS_Step3		;next step.

; get a ptr to the API info structure block of the best handler of the 
; NetBios API.

	mov	ax,SWAPI_QUERY_API_SUPPORT
	mov	bx,API_NETBIOS		;we are interested in NetBios alone
	call	QueryAPISupport		;a call in function supported by us

; compare to see if ours is the best or not. If it is then we will make the
; decisions and since rquests have been oustanding we will fail the call.

	mov	ax,es			;get the segment
	mov	dx,cs			;get our cs
	cmp	ax,dx			;is it in our segment ?
	jnz	OKTS_Step3		;no, goto step 3
	cmp	bx,StubSegOFFSET Our_NB_API_Info
	jz	SuspendFails		;fail the switch.

OKTS_Step3:

; do the actual suspend call now. Interrupts should be disabled for this call

	mov	ax,SWAPI_SUSPEND	;code for QuerySuspend
	mov	dx,0ffffh		;call expects a return code
	cli				;interrupts off for this call
	call	MakeSwitchAPICall	;make the Switch API call
	jz	OkToSuspendRet		;suspend succeeded

SuspendFails:

; we got to make a SessionActive call into all the nodes

	mov	ax,SWAPI_SESSION_ACTIVE	;code for SessionActive
	xor	dx,dx			;call expects no return code
	push	cx			;save cx
	xor	cx,cx			;not the first SessionActive call
	sti				;interrupts on for this call
	call	MakeSwitchAPICall	;make the Switch API call
	pop	cx			;recover cx, ax != 0
	or	ax,ax			;ZERO not set -> failure

OkToSuspendRet:

; invalidate the call back address.

	mov	cs:[CallBackAddrValid],0;the address is invalid

cEnd
;----------------------------------------------------------------------------;
; MakeSwitchAPICall:							     ;
;									     ;
; This routine walks build the Switch API call back structure and calls the  ;
; the entry points in the chain with the function code in AX till all nodes  ;
; have been called or some node returns a non zero value in AX.		     ;
;									     ;
; Entry:								     ;
;	 AX     -    Switch API function code.				     ;
;	 DX	-    0 if return code not significant, -1 if it is.	     ;
;     Flags	-    Interrupts enabled/disabled as appropriate for the call ;
;								             ;
; Exit:									     ;
;    ZeroSet	-    If all the nodes returned success.			     ;
;									     ;
; Note: When this routine is called, DS will not always be our segment.      ;
;----------------------------------------------------------------------------;

cProc	MakeSwitchAPICall,<NEAR,PUBLIC,PASCAL>,<es,di,ax,bx,cx,dx>

	localD	OTS_Call_Ptr		;needed to call through a DWORD

cBegin

	cCall	BuildCallBackChain	;ES:BX points to start of chain

; we should save the address of the call back chain so that all call ins to 
; us from within the call outs do not have to regenerate the chain. This addr
; will however be invalidated at the end of this call.

	mov	wptr cs:[lpCallBackChain],bx ;save address of call back chain.
	mov	wptr cs:[lpCallBackChain+2],es
	mov	cs:[CallBackAddrValid],-1	;the address is valid
	
WalkChainLoop:

	push	ax			;save
	mov	ax,es			;is es:bx 0 ?
	or	ax,bx			;end of chain ?
	pop	ax			;restore
	jz	MakeSwitchAPICallRet	;yes, call was successful

; call down the chain.

	pushem	es,bx,ax,cx		;save in case the call destroys this
	push	dx			;save return code mask
	les	bx,es:[bx.SCBI_Entry_Pt];get the entry point
	mov	seg_OTS_Call_Ptr,es
	mov	off_OTS_Call_Ptr,bx	;save the address we want to call
	mov	bx,cs:[WoahApp]		;ID of app
	smov	es,cs			;es = cs
	mov	di,StubSegOFFSET SwitchAPICallIn
	call	OTS_Call_Ptr		;make the call
	pop	dx			;get back return code mask
	and	ax,dx			;mask the code
	or	ax,ax			;call returned success ?
	popem	es,bx,ax,cx		;restore pointer to current node & code
	jnz	MakeSwitchAPICallRet    ;this call was failed
	les	bx,es:[bx.SCBI_Next]	;load the pointer to the next node.
	jmp	short WalkChainLoop	;continue checking.

MakeSwitchAPICallRet:

cEnd
;----------------------------------------------------------------------------;
; SwitchAPICallIn:							     ;
;									     ;
; This is the Switch API call in function. This routine checks to see whether;
; the entry code is in range and if it is then the appropriate handler is    ;
; called. Carry is set for unsupported call-ins.			     ;
;----------------------------------------------------------------------------;
SwitchAPICallIn	 proc far

	cmp	ax,SWAPI_MAX_CALL_IN	;in range ?
	ja	SwitchAPICallInErr	;no. Return with carry set
	push	bx			;save
	mov	bx,ax			;get the entry code
	shl	bx,1			;for indexing into jump table
	add	bx,StubSegOFFSET SwitchAPICallInTable
	mov	ax,cs:[bx] 		;get the call address
	pop	bx			;restore
	call	ax			;call the routine

SwitchAPICallInRet:

	ret    

SwitchAPICallInErr:

	stc				;unsupported call-in
	ret

SwitchAPICallIn endp
;----------------------------------------------------------------------------;
; GetVersion:   							     ;
;                Entry:							     ;
;			AX = 0						     ;
;		        Interrupts are disabled and may not be enabled.      ;
;			DOS calls may NOT be made.			     ;
;		 Exit:							     ;
;		        carry flag clear				     ;
;			AX = 0 for future extensibility.		     ;
;		     ES:BX = address of current switcher ver structures.     ;
;									     ;
;----------------------------------------------------------------------------;
GetVersion proc near

	smov	es,cs			;make es:bx point to Our_Ver_Struc
	mov	bx,StubSegOFFSET Our_ver_Struc
	xor	ax,ax			;also clears carry
	ret

GetVersion endp
;----------------------------------------------------------------------------;
; TestMemoryRegion:							     ;
;                Entry:							     ;
;			AX = 1						     ;
;		     ES:DI = start of buffer				     ;
;		        CX = size of buffer (0=64K)			     ;
;		     Interrupts are disabled and may not be enabled.         ;
;		     DOS calls may NOT be made.				     ;
;		 Exit:							     ;
;		        carry flag clear				     ;
;			AX = 0	If entire buffer is in global memory         ;
;			AX = 1	If partially in global memory		     ;
;			AX = 2	If entire buffer is in local memory	     ;
;									     ;
; Note: The are which is swapped out by the switcher starts at CS:0 and      ;
;       extends for 'WoaSwapAreaParaSize' paragraphs. This is the only area  ;
;       that we are going to treat as local, everything else is local.	     ;
;								             ;
;       There are 5 cases marked below with the appropriate return code.     ;
;			   						     ;
;									     ;
;			  |-------------------|				     ;
;			  | local memory      |				     ;
;  CASE:   1		2 |	    3	      |	4	   5		     ;
;       |------|    |--------|	 |-------| |-------|   |--------|	     ;
;       |------|    |--------|	 |-------| |-------|   |--------|	     ;
;  RETS:   0		1 |----------2--------|	1	   0		     ;
;									     ;
;----------------------------------------------------------------------------;
TestMemoryRegion proc near

	pushem	bx,cx,dx,si,di		;save
	xor	ax,ax
	mov	bx,es	  		;ax:bx has the start segment
	dec	cx			;one less than size

REPT 	4
	shl	bx,1			;shift AX:BX left by 1
	rcl	ax,1
ENDM

	add	bx,di			;add in start offset
	adc	ax,0			;AX:BX has start linear address

; compare AX:BX to CS:0, If AX:BX is less we have case 1 or 2, else 3, 4 or 5

	mov	di,cs			;get CS
	xor	si,si			;SI:DI will have left edge of LocalMem
REPT 	4
	shl	di,1			;shift AX:BX left by 1
	rcl	si,1
ENDM

	call	CompareAXBXtoSIDI	;do the comparision
	jnc	short TMR_Case3or4or5	;beyong left edge of LocalMemory

; we have case 1 or 2.

	add	bx,cx
	adc	ax,0			;AX:BX has the rt edge of the buffer

; compare the right edge of the buffer to the left edge of global memory

	call	CompareAXBXtoSIDI	;do the comparision
	jc	TMR_RetCase1or5		;case 1.
	jmp	short TMR_RetCase2or4	;case 2

TMR_Case3or4or5:

; get the right edge of global memory.

	mov	di,cs			;get CS
	add	di,cs:[WoaSwapAreaParaSize];add in size of the local memory
	xor	si,si			;SI:DI will have left edge of LocalMem
REPT 	4
	shl	di,1			;shift AX:BX left by 1
	rcl	si,1			;SI:DI is one byte beyond right edge
ENDM
	
; figure out if it is case 5 or not. AX:BX is left edge of buffer and SI:DI
; is one byte beyond right edge of local memory.

	call	CompareAXBXtoSIDI	;do the comparision
	jnc	TMR_RetCase1or5		;case 5.

; we have case 3 or 4. Get the right edge of buffer.

	add	bx,cx
	adc	ax,0			;AX:BX has the rt edge of the buffer

; figure out whether it is case 3 or 4.

	call	CompareAXBXtoSIDI	;do the comparision
	jnc	TMR_RetCase2or4		;case 4

TMR_RetCase3:

	mov	ax,2			;buffer is totally local.
	jmp	short TMR_Ret		;done.

TMR_RetCase1or5:

	xor	ax,ax			;totally global
	jmp	short TMR_Ret		;done.

TMR_RetCase2or4:

	mov	ax,1			;partially in local memory

TMR_Ret:

	popem	bx,cx,dx,si,di		;restore
	ret

TestMemoryRegion  endp
;----------------------------------------------------------------------------;
; SuspendSwitcher:							     ;
;									     ;
;                Entry:							     ;
;			AX = 2						     ;
;		     ES:DI = Switcher call in of new task switcher,	     ;
;			     or 0:0 if not supported.			     ;
;		     Interrupts are enabled.				     ;
;		     DOS calls can be made.				     ;
;		 Exit:							     ;
;		        carry flag clear				     ;
;			AX = 0	If the switcher is now suspended.	     ;
;			AX = 1	If the switcher cannot be suspended.         ;
;			AX = 2  Not suspended, others may start.	     ;
;----------------------------------------------------------------------------;
SuspendSwitcher proc near

; set a flag bit to disabled the Switcher.

	or	cs:[SwitcherDisabled], SD_SWAPI_DISABLE				
	xor	ax,ax			;switcher being disabled, clears carry
	ret

SuspendSwitcher endp
;----------------------------------------------------------------------------;
; ResumeSwitcher:							     ;
;									     ;
;                Entry:							     ;
;			AX = 3						     ;
;		     ES:DI = Switcher call in of new task switcher.	     ;
;		     Interrupts are enabled.				     ;
;		     DOS calls can be made.				     ;
;		 Exit:							     ;
;		        carry flag clear				     ;
;			AX = 0	(required for future extensibility)	     ;
;----------------------------------------------------------------------------;
ResumeSwitcher proc near

; reset a flag bit which tells us that the switchet is disabled by another
; task switcher.

	and	cs:[SwitcherDisabled], NOT SD_SWAPI_DISABLE
	xor	ax,ax			;return code, clears carry
	ret

ResumeSwitcher endp
;----------------------------------------------------------------------------;
; HookCallout:								     ;
;									     ;
;                Entry:							     ;
;			AX = 4						     ;
;		     ES:DI = address of routine to add to call out chain.    ;
;		     Interrupts are enabled.				     ;
;		     DOS calls can be made.				     ;
;		 Exit:							     ;
;		        carry flag clear				     ;
;			AX = 0	(required for future extensibility)	     ;
;----------------------------------------------------------------------------;
HookCallOut proc near

	xor	ax,ax			;we generate INT 2f every time, carry clear
	ret

HookCallOut endp
;----------------------------------------------------------------------------;
; UnHookCallout:  							     ;
;									     ;
;                Entry:							     ;
;			AX = 5						     ;
;		     ES:DI = address of routine to delete from call out chain;
;		     Interrupts are enabled.				     ;
;		     DOS calls can be made.				     ;
;		 Exit:							     ;
;		        carry flag clear				     ;
;			AX = 0	(required for future extensibility)	     ;
;----------------------------------------------------------------------------;
UnHookCallOut proc near

	xor	ax,ax			;we generate INT 2f every time, carry clear
	ret

UnHookCallOut endp
;----------------------------------------------------------------------------;
; QueryAPISupport:							     ;
;									     ;
;                Entry:							     ;
;			AX = 6						     ;
;		        BX = API Code.					     ;
;		     Interrupts will not be enabled if the call is being from;
;		     within a call out from the switcher else they will be.  ;
;		     DOS calls will not be made				     ;
;		 Exit:							     ;
;		        carry flag clear				     ;
;			AX = 0	(required for future extensibility)	     ;
;		     ES:BX = address of the API_Info_Struc belonging to the  ;
;			     respondent with the best level of support for   ;
;			     this API.					     ;
;----------------------------------------------------------------------------;
QueryAPISupport proc near

	pushem	cx,si,ds		;save
	mov	cx,bx			;get the API code

; if the call back chain is still valid we whould not try to build the chain
; again.

	les	bx,cs:[lpCallBackChain]	;load it, in case it is vcalid
	cmp	cs:[CallBackAddrValid],0;is the call back address valid ?
	jnz	QAPIS_Walk_Chain	;it is valid

; will the chain again.

	cCall	BuildCallBackChain	;ES:BX points to start of chain

QAPIS_Walk_Chain:

	xor	si,si			;ds:si -> best handler's structure.
	mov	ds,si

QAPIS_WalkChainLoop:

	push	ax			;save
	mov	ax,es			;is es:bx 0 ?
	or	ax,bx			;end of chain ?
	pop	ax			;restore
	jz	QAPIS_DoneWithWalk	;yes, we have the results.

; get to the correct API node.

	pushem	es,bx,ax		;save
	les	bx,es:[bx.SCBI_API_Ptr]	;start of the pointer.
	mov	ax,es			;is it a valid node
	or	ax,bx			;NULL pointer ?
	jz	QAPIS_SameNode		;yes, skip this one.

QAPIS_SubLoop:

	mov	ax,es:[bx.AIS_Length]	;get the length 
	or	ax,ax			;is this the end ?
	jz	QAPIS_SameNode		;have exhausted the list
	cmp	es:[bx.AIS_API],cx	;is it the right API
	jz	QAPIS_FoundAPINode	;yes, got it!
	add	bx,ax			;es:bx -> next API node
	jmp	short QAPIS_SubLoop	;keep looking for node

QAPIS_FoundAPINode:

; check to see if this is a betther handler.

	call	CompareAPILevels	;compare levels
	jnc	QAPIS_SameNode		;current best is still best

; es:bx points to a better node's structure. Save it in DS:SI.

	smov	ds,es			;ds:si = es:bx
	mov	si,bx

QAPIS_SameNode:

	popem	es,bx,ax		;restore

; continue walking down the line.

	les	bx,es:[bx.SCBI_Next]	;load the pointer to the next node.
	jmp	short QAPIS_WalkChainLoop;continue checking.

QAPIS_DoneWithWalk:

; ds:si -> API_Info_Struc of best handler. Compare this with ours and if the
; current one is better or equal retain it.

	smov	es,cs			;es:bx -> to our API info structure
	mov	bx,StubSegOFFSET Our_NB_API_Info
	call	CompareAPILevels	;see if DS:SI still points to best
	jc	QAPIS_Ret		;ES:BX -> best handler's API struc

; DS:SI points to the best, put it in ES:BX

	smov	es,ds			
	mov	bx,si			

QAPIS_Ret:

	popem	cx,si,ds		;save
	ret

QueryAPISupport endp
;----------------------------------------------------------------------------;
; CompareAPILevels:							     ;
;									     ;
; Entry:								     ;
;	ES:BX -> first API Info structure.				     ;
;	DS:SI -> second API Info structure.				     ;
; Exit:									     ;
;	Carry clear if the second structure is still the better handler.     ;
; Uses:									     ;
;	Flags.								     ;
;----------------------------------------------------------------------------;

CompareAPILevels proc near
	
	push	ax			;save
	mov	ax,ds			;is there a valid second guy ?
	or	ax,si
	jz	CAPIL_FirstBest		;first one is the best.

	mov	ax,[si.AIS_Major_Ver]	;major ver of second one
	cmp	ax,es:[bx.AIS_Major_Ver];major ver of first one
	jb	CAPIL_FirstBest		;we have a new node
	ja	CAPIL_Ret		;second is better, carry clear
	mov	ax,[si.AIS_Minor_Ver]	;minor ver of second one.
	cmp	ax,es:[bx.AIS_Minor_Ver];minor ver of first guy.
	jb	CAPIL_FirstBest		;we have a new node
	ja	CAPIL_Ret		;the current one is better
	mov	ax,[si.AIS_Support_Level];level of second guy
	cmp	ax,es:[bx.AIS_Support_Level]
	jae	CAPIL_Ret		;second guy is better

CAPIL_FirstBest:

	stc				;first guy is better

CAPIL_Ret:

	pop	ax			;restore
	ret

CompareAPILevels endp
;----------------------------------------------------------------------------;
; ComapareAXBXtoSIDI:							     ;
;									     ;
; Compare AX:BX (32 bits) to SI:DI (32 bits), the flags return the result of ;
; the comparision as would a CMP AX:BX,SI:DI would do.			     ;
;----------------------------------------------------------------------------;
CompareAXBXtoSIDI  proc near

	cmp	ax,si			;compare high words
	jne	CABTSD_Ret		;either greater or less, flags tell
	cmp	bx,di			;compare low words, flags have result

CABTSD_Ret:

	ret

CompareAXBXtoSIDI  endp
;----------------------------------------------------------------------------;
; SWAPICreateSession:							     ;
;									     ;
; Makes a CREATE_SESSION SWAPI call out.     				     ;
;----------------------------------------------------------------------------;
cProc	SWAPICreateSession,<NEAR,PUBLIC,PASCAL>

cBegin
	mov	ax,SWAPI_CREATE		;create session call
	mov	dx,0ffffh		;call expects a return code
	sti				;interrupts on for this call
	call	MakeSwitchAPICall	;make the Switch API call

; invalidate the call back address.

	mov	cs:[CallBackAddrValid],0;the address is invalid

cEnd
;----------------------------------------------------------------------------;
; SWAPIResumeSession:							     ;
;									     ;
; Makes a RESUME_SESSION SWAPI call out.					     ;
;----------------------------------------------------------------------------;
cProc	SWAPIResumeSession,<NEAR,PUBLIC,PASCAL>

cBegin
	mov	ax,SWAPI_RESUME		;code for ResumeSession
	xor	dx,dx			;call expects no return code
	mov	cx,1			;being run for the first time
	cli				;interrupts off for this call.
	call	MakeSwitchAPICall	;make the Switch API call

; invalidate the call back address.

	mov	cs:[CallBackAddrValid],0;the address is invalid

cEnd
;----------------------------------------------------------------------------;
; SWAPISessionActive:							     ;
;									     ;
; Makes a SESSION_ACTIVE SWAPI call out. 				     ;
;----------------------------------------------------------------------------;
cProc	SWAPISessionActive,<NEAR,PUBLIC,PASCAL>

cBegin
	mov	ax,SWAPI_SESSION_ACTIVE	;code for SessionActive
	xor	dx,dx			;call expects no return code
	mov	cx,1			;being run for the first time
	sti				;interrupts on for this call
	call	MakeSwitchAPICall	;make the Switch API call

; invalidate the call back address.

	mov	cs:[CallBackAddrValid],0;the address is invalid

cEnd
;----------------------------------------------------------------------------;
; SWAPIDestroySession:							     ;
;									     ;
; Makes a DESTROY_SESSION SWAPI call out. 				     ;
;----------------------------------------------------------------------------;
cProc	SWAPIDestroySession,<NEAR,PUBLIC,PASCAL>

cBegin
	mov	ax,SWAPI_DESTROY	;code for DestroySession
	xor	dx,dx			;call expects no return code
	sti				;interrupts on for this call
	call	MakeSwitchAPICall	;make the Switch API call

; invalidate the call back address.

	mov	cs:[CallBackAddrValid],0;the address is invalid


cEnd
;----------------------------------------------------------------------------;
sEnd	StubSeg
end





	

	



