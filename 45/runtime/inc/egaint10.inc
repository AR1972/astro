;***
; EGAINT10 - Source for EGAINT10 driver
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This driver for the EGAINT10 interface allows the EGA registers
;	to be read as well as written.	It is loaded only if a valid
;	EGA.SYS or MOUSE.SYS driver containing the interface is not
;	detected during initialization.
;
;******************************************************************************
;
;	SCCSID = @(#)egaint10.inc	5.6 87/06/23

;-----------------------------------------------------------------------------;
;   brian conte                                                 12/24/84      ;
;   egaint10.inc                                                v1.40         ;
;   steve shaiman (mod to RevertDefault for glitch in           11/11/85      ;
;                  in graphics controller misc. register)       v1.50         ;
;   steve shaiman (mod to ChangeRegs to clear the direction      2/14/86      ;
;                  flag instead of assuming it's cleared)       v1.51         ;
;   steve shaiman (made Int10Vector, ChangeRegs, Int10Routine    2/21/86      ;
;                  public as needed by erice)                   v1.52         ;
;   steve shaiman (mod to RevertDefault for glitch in            6/09/86      ;
;                  in sequencer clocking mode register)         v1.53         ;
;   steve shaiman (added GetDefault as function f8)             10/30/86      ;
;                 (added SimWriteReg & SimWriteSet for use      v1.60         ;
;                  by routines that intercept standard Int 10                 ;
;                  calls and want to modify the shadow regs                   ;
;		   but not the actual hardware regs)			      ;
;		  (added int 10h bios support for functions		      ;
;		   5 [set display page], b [set CGA palette		      ;
;		   registers] & 10 [set EGA palette registers]		      ;
;	      jtp (fixed bug in SetEgaPalette to split 17-byte	 4/01/87      ;
;		   update into two updates)				      ;
;	      jtp (incorporated various bug fixes, primarily to 	      ;
;		   SetCgaPalette and SetEgaPalette Int hooks)	 4/12/87      ;
;	      jtp (optimized Int10Routine to minimize time		      ;
;		   spent on ignored functions, and optimized		      ;
;		   the rep-movsw's in the egaint10 functions)    4/12/87      ;
;	      jtp (added kludge to RevertDefault to update		      ;
;		   Bios data areas too, for Word /C operation)	 5/24/87      ;
;                                                                             ;
;   egaint10 is an interface to the ega card which allows all of the w/o      ;
;   registers to be read from as well as written to.  this is a requirement   ;
;   in order to use interrupt-driven graphics (such as cursor update code).   ;
;   interrupt-driven graphics can be implemented on the ega if both the       ;
;   synchronous app and the interrupt code use the egaint10 interface         ;
;   to the ega registers.  the microsoft mouse driver (version 3.0 and        ;
;   greater) uses the egaint10 interface to the ega card.                     ;
;                                                                             ;
;   this file contains the code that intercepts the int 10's and keeps        ;
;   shadow maps of all ega registers so that these registers can be           ;
;   read as well as written.  note that the primary consideration was         ;
;   speed, some of the calls need to be made in tight graphics loops.  for    ;
;   this reason, the interface may seem arbitrary and the code may seem       ;
;   unnecessarily "tricky" at time.  i've tried to document all such          ;
;   instances.  also keep in mind that there is no error checking of          ;
;   parameters.  illegal parameters may cause unpredictable results,          ;
;   including possibly a system crash.                                        ;
;                                                                             ;
;   this code may be used in a standalone driver, or may be incorporated      ;
;   into an app or another driver if desired.  here are the requirements      ;
;   for using this code:                                                      ;
;                                                                             ;
;               include the file inside your code segment.                    ;
;                                                                             ;
;               put the current screen mode in ah, and bits 7:4 of the        ;
;               crtc io address ("b" for "3b4", "d" for "3d4) into bits 7:4   ;
;               of al, set ds to zero, and then call "ChangeRegs".            ;
;                                                                             ;
;               get the current int 10 vector and save it in "Int10Vector".   ;
;                                                                             ;
;               set the int 10 vector to point to cs:Int10Routine.            ;
;                                                                             ;
;               after this, regular int 10 and extended int 10 calls may      ;
;               be executed as desired.                                       ;
;                                                                             ;
;   the code that does this in the ega.sys standalone driver follows:         ;
;                                                                             ;
;               xor     ax,ax                                                 ;
;               mov     ds,ax                                                 ;
;               mov     ah,ds:[449h]                                          ;
;               mov     al,byte ptr ds:[463h]                                 ;
;               and     al,11110000b                                          ;
;               call    ChangeRegs                                            ;
;               mov     ax,cs                                                 ;
;               mov     ds,ax                                                 ;
;               assume  ds:Code                                               ;
;               mov     ax,3510h                                              ;
;               int     21h                                                   ;
;               mov     word ptr [Int10Vector],bx                             ;
;               mov     word ptr [Int10Vector+2],es                           ;
;               mov     ax,2510h                                              ;
;               mov     dx,offset Int10Routine                                ;
;               int     21h                                                   ;
;                                                                             ;
;   eventually ega.sys will intercept all bios calls and update its tables.   ;
;   right now it only intercepts calls to set the mode (ah=0).                ;
;                                                                             ;
;   ega.sys is reentrant.  this means that it is legal for an interrupt       ;
;   routine to call ega.sys while the synchronous program is in the middle of ;
;   one of its calls to ega.sys.  [currently, this is only true if the        ;
;   interrupt routine only saves and restores registers (ie makes no permanent;
;   changes) but this could be changed to accomodate interrupt routines that  ;
;   make permanent changes (eg an interrupt routine that changed the palette  ;
;   on a vertical retrace).]                                                  ;
;                                                                             ;
;   a special note about the attribute chip registers:  the flip-flop which   ;
;   determines whether 3c0 is the address or data register must be set to     ;
;   "address" before making extended int 10 calls.  it will always be left    ;
;   in the "address" state on return.  interrupt code must be sure to set     ;
;   the flip flop to "address" before using attribute registers, and to       ;
;   return it to "address" on return.  any code which sets the flip flop      ;
;   to "data" and then depends on it being set that way must disable          ;
;   interrupts for the length of time between setting it to "data" and the    ;
;   last time it is assumed to be "data".                                     ;
;                                                                             ;
;   this code knows nothing about the sequencer memory mode register.  this   ;
;   is because the sequencer will produce a 20 ns glitch on the cas lines     ;
;   when this register is accessed, which seems to have a chance of glitching ;
;   vram even though write enable is never asserted.  therefore, reading to   ;
;   or writing to this register using these routines is an error.             ;
;                                                                             ;
;   this code does not restore the contents of the graphics controller        ;
;   miscellaneous register as part of the RevertDefault (function F6).        ;
;   this is because it appears to cause a glitch on the cas lines in a        ;
;   similar to the sequencer memory mode register.  the rest of the code      ;
;   does not give this register any special treatment, and it is the          ;
;   responsibility of the programmer to be careful.  this problem is          ;
;   particularly insideous because it will only show up on EGA cards with     ;
;   the add on memory cards that have INMOS RAM chips.  Beware!!!             ;
;                                                                             ;
;   this code has no provisions for reading the input status registers        ;
;   on the ega.  any code that wants to read either of the input status       ;
;   registers should do so directly.                                          ;
;                                                                             ;
;   extended int 10 call interface spec:                                      ;
;                                                                             ;
;   Read One Register                                                         ;
;                                                                             ;
;       in:     ah      0f0h                                                  ;
;               dx:     port number                                           ;
;                       0h: crtc chip      (3x4h)                             ;
;                       8h: sequencer chip (3c4h)                             ;
;                      10h: graphics chips (3ceh)                             ;
;                      18h: attribute chip (3c0h)                             ;
;                      20h: misc out reg   (3c2h)                             ;
;                      28h: feature reg    (3xah)                             ;
;                      30h: gr 1 pos reg   (3cch)                             ;
;                      38h: gr 2 pos reg   (3cah)                             ;
;               bx:     ptr for ptr/data ports (first 4 above)  (bh=0, bl=ptr);
;                       ignored for single registers (last 4 above)           ;
;                                                                             ;
;       return: ax      restored                                              ;
;               bl      data                                                  ;
;               bh      restored                                              ;
;               dx      restored                                              ;
;               all others restored                                           ;
;                                                                             ;
;                                                                             ;
;   Write One Register                                                        ;
;                                                                             ;
;       in:     ah      0f1h                                                  ;
;               dx      port number                                           ;
;                       0h: crtc chip      (3x4h)                             ;
;                       8h: sequencer chip (3c4h)                             ;
;                      10h: graphics chips (3ceh)                             ;
;                      18h: attribute chip (3c0h)                             ;
;                      20h: misc out reg   (3c2h)                             ;
;                      28h: feature reg    (3xah)                             ;
;                      30h: gr 1 pos reg   (3cch)                             ;
;                      38h: gr 2 pos reg   (3cah)                             ;
;               bl      pointer value for ptr/data ports (first 4 above)      ;
;                       data for single registers (last 4 above)              ;
;               bh      data for ptr/data ports                               ;
;                       ignored for single registers                          ;
;                                                                             ;
;       return: ax      restored                                              ;
;               bl      restored                                              ;
;               bh      trashed                                               ;
;               dx      trashed                                               ;
;               all others restored                                           ;
;                                                                             ;
;                                                                             ;
;   Read Register Range                                                       ;
;                                                                             ;
;       in:     ah      0f2h                                                  ;
;               dx      port number                                           ;
;                       0h: crtc chip      (3x4h)                             ;
;                       8h: sequencer chip (3c4h)                             ;
;                      10h: graphics chips (3ceh)                             ;
;                      18h: attribute chip (3c0h)                             ;
;               es:bx   table of values                                       ;
;               ch      starting pointer value                                ;
;               cl      number of regs (must be > 1)                          ;
;                                                                             ;
;       return: ax      restored                                              ;
;               bx      restored                                              ;
;               cx      trashed                                               ;
;               dx      restored                                              ;
;               es      restored                                              ;
;               all others restored                                           ;
;                                                                             ;
;                                                                             ;
;   Write Register Range                                                      ;
;                                                                             ;
;       in:     ah      0f3h                                                  ;
;               dx      port number                                           ;
;                       0h: crtc chip      (3x4h)                             ;
;                       8h: sequencer chip (3c4h)                             ;
;                      10h: graphics chips (3ceh)                             ;
;                      18h: attribute chip (3c0h)                             ;
;               es:bx   table of values                                       ;
;               ch      starting pointer value                                ;
;               cl      number of regs (must be > 1)                          ;
;                                                                             ;
;       return: ax      restored                                              ;
;               bx      trashed                                               ;
;               dx      trashed                                               ;
;               es      restored                                              ;
;               all others restored                                           ;
;                                                                             ;
;                                                                             ;
;   Read Register Set                                                         ;
;                                                                             ;
;       in:     ah      0f4h                                                  ;
;               es:bx   table of values, where each entry consists of         ;
;                               port number     (word)                        ;
;                               pointer value   (byte) (should be 0 for       ;
;                                                       single registers)     ;
;                               data            (byte)                        ;
;               cx      number of regs (must be > 1)                          ;
;                                                                             ;
;       return: ax      restored                                              ;
;               bx      restored                                              ;
;               cx      trashed                                               ;
;               es      restored                                              ;
;               all others restored                                           ;
;                                                                             ;
;                                                                             ;
;   Write Register Set                                                        ;
;                                                                             ;
;       in:     ah      0f5h                                                  ;
;               es:bx   table of values, where each entry consists of         ;
;                               port number     (word)                        ;
;                               pointer value   (byte) (should be 0 for       ;
;                                                       single registers)     ;
;                               data            (byte)                        ;
;               cx      number of regs (must be > 1)                          ;
;                                                                             ;
;       return: ax      restored                                              ;
;               bx      restored                                              ;
;               cx      trashed                                               ;
;               es      restored                                              ;
;               all others restored                                           ;
;                                                                             ;
;   Revert To Default Registers                                               ;
;                                                                             ;
;       in:     ah      0f6h                                                  ;
;                                                                             ;
;       return: all registers restored                                        ;
;                                                                             ;
;                                                                             ;
;   Define Default Register Table                                             ;
;                                                                             ;
;       in:     ah      0f7h                                                  ;
;               dx      port number                                           ;
;                       0h: crtc chip      (3x4h)                             ;
;                       8h: sequencer chip (3c4h)                             ;
;                      10h: graphics chips (3ceh)                             ;
;                      18h: attribute chip (3c0h)                             ;
;                      20h: misc out reg   (3c2h)                             ;
;                      28h: feature reg    (3xah)                             ;
;                      30h: gr 1 pos reg   (3cch)                             ;
;                      38h: gr 2 pos reg   (3cah)                             ;
;               es:bx  points to table of default values (all must be present);
;                                                                             ;
;       return: ax      restored                                              ;
;               bx      trashed                                               ;
;               dx      trashed                                               ;
;               es      restored                                              ;
;               all others restored                                           ;
;                                                                             ;
;                                                                             ;
;   Get Default Register Table                                                ;
;                                                                             ;
;       in:     ah      0f8h                                                  ;
;               dx      port number                                           ;
;                       0h: crtc chip      (3x4h)                             ;
;                       8h: sequencer chip (3c4h)                             ;
;                      10h: graphics chips (3ceh)                             ;
;                      18h: attribute chip (3c0h)                             ;
;                      20h: misc out reg   (3c2h)                             ;
;                      28h: feature reg    (3xah)                             ;
;                      30h: gr 1 pos reg   (3cch)                             ;
;                      38h: gr 2 pos reg   (3cah)                             ;
;               es:bx  points to table receiving default values               ;
;                      (table must have room for full set of values)          ;
;                                                                             ;
;       return: ax      restored                                              ;
;               bx      trashed                                               ;
;               dx      trashed                                               ;
;               es      restored                                              ;
;               all others restored                                           ;
;                                                                             ;
;                                                                             ;
;   Inquire Driver                                                            ;
;      in:     ah      0fah                                                   ;
;              bx      0                                                      ;
;                                                                             ;
;      return: ax      restored                                               ;
;              es:bx   nonzero (reserved)                                     ;
;                                                                             ;
;                                                                             ;
;  note: calls 0f9h, and 0fbh - 0fdh are reserved. 0feh & 0ffh are used by    ;
;        environments such as Windows, TopView, and Mondrian                  ;
;                                                                             ;
;-----------------------------------------------------------------------------;

;[1]not for BASIC RUNTIME!!
;[1]		.286c	;can use 286 instructions in os/2 driver

                public  Int10Vector

                public  ChangeRegs
                public  Int10Routine


MyCallTable     label   word
                dw      offset ReadReg          ;call f0
                dw      offset WriteReg         ;call f1
                dw      offset ReadRange        ;call f2
                dw      offset WriteRange       ;call f3
                dw      offset ReadSet          ;call f4
                dw      offset WriteSet         ;call f5
                dw      offset RevertDefault    ;call f6
                dw      offset DefineDefault    ;call f7
                dw      offset GetDefault       ;call f8
                dw      offset CallInt10        ;call f9
                dw      offset InquireDriver    ;call fa
                dw      offset CallInt10        ;call fb
                dw      offset CallInt10        ;call fc
                dw      offset CallInt10        ;call fd
                dw      offset CallInt10        ;call fe
                dw      offset CallInt10        ;call ff

MaxBIOSCall     equ     13h                     ;max rom bios call

BIOSCallTable   label   word                    ;eventually none will be ignored
                dw      offset SetMode          ;set display mode
                dw      offset Ignore
                dw      offset Ignore
                dw      offset Ignore
                dw      offset Ignore
		dw	offset Ignore		;set page now ignored  4/12/87
                dw      offset Ignore
                dw      offset Ignore
                dw      offset Ignore
                dw      offset Ignore
                dw      offset Ignore
                dw      offset SetCgaPalette    ;set CGA palette registers
                dw      offset Ignore
                dw      offset Ignore
                dw      offset Ignore
                dw      offset Ignore
                dw      offset SetEgaPalette    ;set EGA palette registers
                dw      offset Ignore
                dw      offset Ignore
                dw      offset Ignore

SetRec          struc                           ;used in read/write set calls
    SRPortNum   dw      ?
    SRPtr       db      ?
    SRData      db      ?                       ;must be last!
SetRec          ends

PortRec         struc
    PRPortAddr  dw      ?                       ;io port address (can change)
    PRCurrTable dw      ?                       ;current shadow map table
    PRDefTable  dw      ?                       ;default shadow map table
    PRNumRegs   db      ?                       ;number of data regs
    PRModFlag   db      ?                       ;modified since last "rev def"
PortRec         ends

MasmBug         macro   pa,ct,dt,nr,mf          ;masm 3.0 doesn't allow >1
                dw      pa                      ; ... relocatable in a record
                dw      ct
                dw      dt
                db      nr
                db      mf
                endm

NumPtrData      equ     4

PortTable       label   word                    ;code dependent on order
                MasmBug 3d4h,<offset CRTCRegs    >,<offset DefCRTCRegs    >,19h,0
                MasmBug 3c4h,<offset SeqRegs     >,<offset DefSeqRegs     >,4h,0
                MasmBug 3ceh,<offset GraphicsRegs>,<offset DefGraphicsRegs>,9h,0
                MasmBug 3c0h,<offset AttrRegs    >,<offset DefAttrRegs    >,14h,0
                MasmBug 3c2h,<offset MiscOutReg  >,<offset DefMiscOutReg  >,1,0
                MasmBug 3dah,<offset FeatureReg  >,<offset DefFeatureReg  >,1,0
                MasmBug 3cch,<offset Gr1PosReg   >,<offset DefGr1PosReg   >,1,0
                MasmBug 3cah,<offset Gr2PosReg   >,<offset DefGr2PosReg   >,1,0
PortTableEnd    label   byte

SequencerAddr   equ     03c4h
SequencerClMReg equ     1

GraphicsAddr    equ     03ceh                   ;graphics controller port address
GraphicsMiscReg equ     6                       ;miscellaneous reg offset in gc

StartShadowMaps label   byte
SeqRegs         db      5h dup(?)               ;these are the shadow map tables
MiscOutReg      db      ?                       ;order must be same as bios
CRTCRegs        db      19h dup(?)
AttrRegs        db      14h dup(?)
GraphicsRegs    db      9h dup(?)
SizeShadowMaps  equ     $-StartShadowMaps
DefSeqRegs      db      5h dup(?)               ;these are default sm tables
DefMiscOutReg   db      ?                       ;order must be same as bios
DefCRTCRegs     db      19h dup(?)
DefAttrRegs     db      14h dup(?)
DefGraphicsRegs db      9h dup(?)

Gr1PosReg       db      ?                       ;these aren't included in bios
Gr2PosReg       db      ?
FeatureReg      db      0                       ;never changed
DefGr1PosReg    db      ?
DefGr2PosReg    db      ?
DefFeatureReg   db      0                       ;never changed

SingleRegMod    db      0                       ;>0 if a single reg modified

SaveAX          dw      ?                       ;temp variables
SaveSI          dw      ?

Int10Vector     dd      ?                       ;old int 10 vector

EgaStateVars    label   byte                    ;this is reserved for the future
  Version       db      1,60
  CopyrightMsg  dw      offset Copyright
  CheckSum      dw      0
  CheckSumStart dw      offset SetMode
  InBIOS        db      0
Copyright       db      "*** This is Copyright (c) 1984,85,86  Microsoft ***"


; BIOS data segment offsets of display data

BiosCrtMode     equ     49h             ;40:49 = CRT mode
BiosCrtCols     equ     4ah             ;40:4a = CRT character columns/line
BiosCrtLen      equ     4ch             ;40:4c = regen buffer length
BiosCursorPosn  equ     50h             ;40:50 = start of cursor position array
BiosActivePage  equ     62h             ;40:62 = active display page #
BiosCrtPalette  equ     66h             ;40:66 = CRT palette byte



SetMode 	proc	near			;this is called when a bios
		push	ds			; set mode call is made.
		xor	ax,ax			;it must predict which mode
		mov	ds,ax			; table the bios will use, so
		mov	ax,ds:[410h]		; the bios decision logic is
		and	al,30h			; duplicated here.
                test    byte ptr ds:[487h],2
                jz      SMNoMonochrome
                cmp     al,30h
                jnz     SMNoChange
                mov     ah,byte ptr cs:[SaveAX]
                mov     al,0b0h                 ;io addr = 3bx
                and     ah,7fh
                cmp     ah,0fh
                jz      SMChangeRegs
                mov     ah,07h
                jmp     short SMChangeRegs      ;ah has mode
SMNoMonochrome: cmp     al,30h
                jz      SMNoChange
                mov     ah,byte ptr cs:[SaveAX]
                mov     al,0d0h                 ;io addr = 3dx
                and     ah,7fh
SMChangeRegs:   call    ChangeRegs
SMNoChange:     pop     ds
Ignore:         ret
SetMode         endp


MakeBase	proc	near			;extracted from ChangeRegs 4/12/87
		assume	ds:nothing		;ds must be = 0
		les	si,ds:[4a8h]
		les	si,es:[si]
		test	byte ptr ds:[487h],60h
		jz	mb0			;these labels correspond to
		add	si,440h 		;... bios offsets.
		cmp	ah,0fh
		je	mbx
		add	si,40h
		cmp	ah,10h
		je	mbx
		sub	si,480h
mb0:		cmp	ah,3
		ja	mb2
		mov	al,0fh
		and	al,ds:[488h]
		cmp	al,03h
		je	mb1
		cmp	al,09h
		jne	mb2
mb1:		add	si,4c0h
mb2:		xor	al,al
		shr	ax,1
		shr	ax,1
		add	si,ax
mbx:		ret				;es:si points to correct table
MakeBase	endp


ChangeRegs      proc    near                    ;inp: ah=mode,al=io addr,ds=0
                push    es                      ;sets up shadow map tables
                push    cx
                push    di
                pushf                           ; save direction flag
                cld                             ; clear direction flag
                and     byte ptr cs:PortTable[0].PRPortAddr,0fh
                or      byte ptr cs:PortTable[0].PRPortAddr,al
                and     byte ptr cs:PortTable[5 * size PortRec].PRPortAddr,0fh
                or      byte ptr cs:PortTable[5 * size PortRec].PRPortAddr,al
		call	MakeBase
		mov	ax,es			;es:si points to correct table
                mov     ds,ax
                assume  ds:nothing
                mov     ax,cs
                mov     es,ax                   ;ds is source, es dest
                add     si,5
                mov     di,offset StartShadowMaps
                mov     al,3
                stosb
                mov     cx,SizeShadowMaps-1
                shr     cx,1
                push    cx
                if      (SizeShadowMaps-1) and 1
                movsb
                endif
                rep     movsw
                sub     si,(SizeShadowMaps-1)
                stosb
                pop     cx
                if      (SizeShadowMaps-1) and 1
                movsb
                endif
                rep     movsw
                mov     ax,cs
                mov     ds,ax
                assume  ds:Code
                mov     [Gr1PosReg],cl
                mov     [DefGr1PosReg],cl
                inc     cl
                mov     [Gr2PosReg],cl
                mov     [DefGr2PosReg],cl
                xor     di,di
                mov     cx,NumPtrData
CRClearModFlag: mov     byte ptr PortTable[di].PRModFlag,0
                add     di,size PortRec
                loop    CRClearModFlag
                mov     [SingleRegMod],0
                popf
                pop     di
                pop     cx
                pop     es
                ret
ChangeRegs      endp


;   Since both hardware regs this routine is trying to shadow are readable,
;   and since BVS has taken responsibility for this anyway, this routine
;   is unnecessary.   It has perhaps not even been tested, since it
;   expects AL to have page # on entry, when in fact it must use SaveAX.  4/12/87
;
;SetDisplayPage proc	near
;		push	ax		     ;save registers that may get
;		push	bx		     ; destroyed
;		push	cx
;		push	dx
;		push	di
;
;		cbw			     ;ax <- display page #
;		mov	bx,ax		     ;bx <- display page #
;		push	ds		     ;save ds
;		mov	cx,40h		     ;set ds pointing to the BIOS
;		mov	ds,cx		     ; data segment
;		mov	cl,ds:[BiosActivePage]	;cl <- BIOS active display page
;		mov	dh,ds:[BiosCrtMode]	;dh <- BIOS display mode
;		mov	dl,ds:[BiosCrtCols]	;dl <- BIOS chars/line
;		mov	di,offset BiosCursorPosn[bx] ;di <- cursor position
;		mov	bx,ds:[BiosCrtLen]	;bx <- BIOS regen buffer length
;		pop	ds		     ;restore ds
;
;		cmp	al,cl		     ;check if page # hasn't changed
;		je	SDispPageEnd	     ;branch if new page == old page
;		mov	cx,ax		     ;cx <- display page #
;		xor	si,si		     ;si will end up with regen buffer
;					     ; address for selected page
;		jcxz	SetDispPage2	     ;branch if page 0 selected
;SetDispPage1:	add	si,bx		     ;add page offset to page start
;		loop	SetDispPage1	     ;sum pages until done
;SetDispPage2:	mov	bx,si		     ;bh <- msb of CRTC start address
;		cmp	dh,7		     ;modes <= 7 use word addresses
;		ja	SetDispPage3	     ;branch if display mode > 7
;		sar	bx,1		     ;use starting word address
;SetDispPage3:	mov	12[CRTCRegs],bh      ;save Start Address High Reg value
;		mov	13[CRTCRegs],bl      ;save Start Address Low Reg value
;		mov	bx,di		     ;bh <- curs row / bl <- curs col
;		mov	cl,dl		     ;cl <- characters/line
;		mov	dx,si		     ;dx <- page starting byte offset
;		sar	dx,1		     ;dx <- page starting cursor posn
;		mov	al,bh		     ;al <- cursor position row
;		mul	cl		     ;ax <- cursor row address/2
;		xor	bh,bh		     ;bh <- cursor column #
;		add	ax,bx		     ;ax <- page cursor position
;		add	ax,dx		     ;ax <- cursor position
;		mov	14[CRTCRegs],ah      ;save Cursor Loc High Reg value
;		mov	15[CRTCRegs],al      ;save Cursor Loc Low Reg value
;		mov	byte ptr PortTable[0].PRModFlag,1 ;set CRTC dirty flag
;
;SDispPageEnd:	pop	di
;		pop	dx
;		pop	cx
;		pop	bx
;		pop	ax
;		ret
;SetDisplayPage endp


SetCgaPalette   proc    near
                push    ax                   ;save registers that may get
                push    bx                   ; destroyed
                push    cx
                push    dx

                push    ds                   ;save ds
                mov     cx,40h               ;set ds pointing to the BIOS
                mov     ds,cx                ; data segment
                mov     cl,ds:[BiosCrtPalette]  ;cl <- BIOS CRT palette byte
                mov     ch,ds:[BiosCrtMode]     ;ch <- BIOS CRT mode
                pop     ds                   ;restore ds

                or      bh,bh                ;background or palette colors ?
                jnz     SCgaPalette          ;branch if set CGA palette colors

		and	cl,11100000b	     ;cl gets an updated
		and	bl,00011111b	     ; version of the BIOS palette byte
		or	cl,bl		     ; now, with new background color

                mov     bh,bl                ;convert the color number
                shl     bl,1                 ; in bl into an intensity
                and     bl,10h               ; in bit 4 and color in
                and     bh,7                 ; bits 2-0 and leave the
                or      bh,bl                ; results in bh

                cmp     ch,3                 ;check for character modes
                jbe     SCgaCharPal          ;branch if in a character mode

		mov	dx,18h		     ;dx <- attribute controller port #
                push    bx                   ;save color in bh
                xor     bl,bl                ;set attr cntrlr palette color 0
                call    SimWriteReg          ; (background) to value in bh
                pop     bx                   ;restore color to bh
SCgaCharPal:    mov     dx,18h               ;dx <- attribute controller port #
                mov     bl,11h               ;set attribute controller
                call    SimWriteReg          ; overscan color to value in bh
;		jmp	short SCgaPaletteEnd
		mov	bl,cl		     ;ega function w/bh=0 sets both
		and	bl,00100000b	     ; border and background color,
;[1]		rol	bl,3		     ; so we drop into background logic
		rol	bl,1		     ;[1]
		rol	bl,1		     ;[1]
		rol	bl,1		     ;[1]

SCgaPalette:    cmp     ch,3                 ;check for character modes
                jbe     SCgaPaletteEnd       ;branch if character mode (no
                                             ; palettes for character modes)
                and     cl,10h               ;cl <- intensity bit from palette
                                             ; which is actually derived from
                                             ; the background color by the BIOS
                and     bl,1                 ;only allow palettes 0 and 1
                or      bl,cl                ;or intensity into palette
                or      bl,2                 ;color 1 is green (2) or cyan (3)
                mov     bh,bl                ;bh <- color and intensity
                mov     bl,1                 ;set attribute controller palette
                                             ; register 1 to value in bh
		mov	dx,18h		     ;dx <- attribute controller port #
		push	bx		     ;save bx
                call    SimWriteReg          ;write it!
                pop     bx                   ;restore bx
                inc     bh                   ;color 2 is red (4) or magenta (5)
                inc     bh
                inc     bl                   ;set attribute controller palette
                                             ; register 2 to value in bh
                mov     dx,18h               ;dx <- attribute controller port #
		push	bx		     ;save bx
                call    SimWriteReg          ;write it!!
                pop     bx                   ;restore bx
                inc     bh                   ;color 3 is brown (6) or white (7)
                inc     bh
                inc     bl                   ;set attribute controller palette
                                             ; register 3 to value in bh
                mov     dx,18h               ;dx <- attribute controller port #
                call    SimWriteReg          ;write it!!!

SCgaPaletteEnd: pop     dx
                pop     cx
                pop     bx
                pop     ax
                ret
SetCgaPalette   endp


SetEgaPalette   proc    near
                push    ax                   ;save registers that may get
                push    bx                   ; destroyed
                push    cx
                push    dx

                mov     dx,18h               ;dx <- attribute controller port #
                mov     al,byte ptr [SaveAX] ;al <- subfunction
                or      al,al                ;0 = set individual palette reg
                jnz     NotS1EgaPalReg       ;branch if not set 1 register

S1EgaPalReg:    call    SimWriteReg          ;write 1 Attribute Controller Reg
                jmp     short SEgaPaletteEnd

NotS1EgaPalReg: dec     al                   ;1 = set overscan register
                jnz     NotSetOverscan       ;branch if not overscan register
                mov     bl,11h               ;bl <- overscan register number
                jmp     short S1EgaPalReg    ;write it!!!

NotSetOverscan: dec     al                   ;2 = set palette & overscan regs
                jnz     ToggleBlink          ;branch if not set all
                pop     bx                   ;bx <- table offset (was in dx)
                push    bx                   ;put dx value back onto stack
                mov     cx,16                ;16 registers to write first
                call    SimWriteRange        ;write them!!!
		pop	bx
		push	bx			;get pointer to data again
		mov	bh,es:[bx+16]		;get the overscan byte now
		mov	bl,11h			;ptr to overscan reg
		call	SimWriteReg		;write separated overscan now!
                jmp     short SEgaPaletteEnd

ToggleBlink:    dec     al                   ;3 = toggle blink bit
                jnz     SEgaPaletteEnd       ;branch if not toggle blink bit

;   In order to parallel EGA Bios logic (strange though it sometimes is),
;   this code must fetch the default attr mode reg value from the tables in
;   EGA ROM and modify THAT, rather than the CURRENT setting of the mode reg.
;   If the bh parm is not legal, then that default value is stored instead,
;   which may well be different from the current setting of the mode reg.  4/12/87

		push	ds
		push	es
		cbw			     ;we know al already 0
		mov	ds,ax		     ;MakeBase requires ds = 0
		mov	ah,ds:[449h]	     ;also requires video bios mode #
		call	MakeBase
		mov	bh,es:[si+51]	     ;get default attr mode value
		pop	es
		pop	ds

		or	bl,bl		     ;what's our purpose in life?
		jz	resetbit	     ;0 means shut off bit
		dec	bl
		or	bl,bl
		jnz	storebit	     ;still not 0? give'em a default
		or	bh,00001000b
		jmp	short storebit
resetbit:	and	bh,11110111b

;		mov	bx,16		     ;read attr controller mode reg
;		call	ReadReg 	     ;bl <- attr controller mode reg
;		pop	cx		     ;cx <- toggle bit
;		shl	cl,1		     ;move toggle bit from
;		shl	cl,1		     ; bit 0 to bit 3
;		shl	cl,1
;		or	cl,bl		     ;cl <- new value for mode reg
;		mov	bh,cl		     ;bh <- new value for mode reg

storebit:	mov	bl,16		     ;write attr controller mode reg
		mov	dx,18h		     ;dx <- attribute controller port #
		jmp	short S1EgaPalReg    ;write attr controller mode reg

SEgaPaletteEnd: pop     dx
                pop     cx
                pop     bx
                pop     ax
                ret
SetEgaPalette   endp


Int10Routine    proc    far
		cmp	ah,0f0h 		;Fast EGA R.I. call?
		jb	NotMine 		;no
MyCall:         push    ds                      ;CallInt10 depends on this!
                push    si
                push    ax
                mov     al,ah
                and     ax,000fh
                shl     ax,1
                mov     si,ax
                mov     ax,cs
                mov     ds,ax
                call    [si].MyCallTable
                pop     ax
                pop     si
                pop     ds
                iret

NotMine:	cmp	ah,0Bh			;SetCgaPalette call?
		je	Mine2			;we hook that one
		test	ah,0Fh			;test for AH=00 or 10
		jz	Mine1			;we hook both of those calls
		jmp	cs:[Int10Vector]	;we don't hook any other AH=0X

Mine1:		cmp	ah,MaxBIOSCall		;but we don't hook AH=20,30,etc
                ja      DoInt10
Mine2:		push	ds
                mov     cs:[SaveSI],si
                mov     si,cs
                mov     ds,si
                mov     [SaveAX],ax
                mov     al,ah
                xor     ah,ah
                shl     ax,1
                mov     si,ax
                call    BIOSCallTable[si]
                mov     ax,[SaveAX]
                mov     si,[SaveSI]
                pop     ds
DoInt10:        jmp     cs:[Int10Vector]
Int10Routine    endp


SimWriteReg     proc    near
                mov     ax,bx
                mov     si,dx
                cmp     dx,NumPtrData * size PortRec
                mov     dx,PortTable[si].PRPortAddr
                mov     byte ptr PortTable[si].PRModFlag,1
                mov     si,PortTable[si].PRCurrTable
                jge     SWRegNoPtr
                xor     bh,bh
                mov     [si+bx],ah
                ret
SWRegNoPtr:     mov     [si],al
                mov     [SingleRegMod],1
                ret
SimWriteReg     endp


SimWriteRange   proc    near
                cld
                push    di
                push    es
                mov     si,bx
                mov     di,dx
                mov     byte ptr PortTable[di].PRModFlag,1
                mov     di,PortTable[di].PRCurrTable
                mov     ax,es
                mov     bx,ds
                mov     es,bx
                mov     ds,ax
                xor     ax,ax
                xchg    al,ch
                add     di,ax
                shr     cx,1
                rep     movsw
		rcl	cx,1
		rep	movsb
                mov     ds,bx
                pop     es
                pop     di
                ret
SimWriteRange   endp


ReadReg         proc    near
                mov     si,dx
                mov     si,PortTable[si].PRCurrTable
                cmp     dx,NumPtrData * size PortRec
                jge     RRegNoPtr
                mov     bl,[si+bx]
                ret
RRegNoPtr:      mov     bl,[si]
                ret
ReadReg         endp


WriteReg        proc    near                    ;3c0h assumed to be ok state
                mov     ax,bx
                mov     si,dx
                cmp     dx,NumPtrData * size PortRec
                mov     dx,PortTable[si].PRPortAddr
                mov     byte ptr PortTable[si].PRModFlag,1
                mov     si,PortTable[si].PRCurrTable
                jge     WRegNoPtr
                xor     bh,bh
                mov     [si+bx],ah
                out     dx,ax
                ret
WRegNoPtr:      mov     [si],al
                mov     [SingleRegMod],1
                out     dx,al
                ret
WriteReg        endp


ReadRange       proc    near            ;should only be called for ptr/data regs
                sti
                cld                     ;cl must be > 1
                push    di
                mov     di,bx
                mov     si,dx
                mov     si,PortTable[si].PRCurrTable
                xor     ax,ax
                xchg    al,ch
                add     si,ax
		shr	cx,1
		rep	movsw
		rcl	cx,1
		rep	movsb
                pop     di
                ret
ReadRange       endp


WriteRange      proc    near
                sti
                cld
                push    di
                push    es
                mov     si,bx
                mov     di,dx
                mov     byte ptr PortTable[di].PRModFlag,1
                mov     dx,PortTable[di].PRPortAddr
                mov     di,PortTable[di].PRCurrTable
                mov     ax,es
                mov     bx,ds
                mov     es,bx
                mov     ds,ax
                xor     ax,ax
                xchg    al,ch
                add     di,ax
                push    cx
                shr     cx,1
		rep	movsw
		rcl	cx,1
		rep	movsb
                mov     ds,bx
                pop     cx
                sub     di,cx
WRangeNextReg:  mov     ah,[di]
                out     dx,ax           ;al initially contains starting reg
                inc     di
                inc     al
                loop    WRangeNextReg
                pop     es
                pop     di
                ret
WriteRange      endp


ReadSet         proc    near
                sti
                cld
                push    di
                mov     di,bx
RSetNextEntry:  mov     si,es:[di].SRPortNum
                mov     si,PortTable[si].PRCurrTable
                mov     al,es:[di].SRPtr
                cbw
                add     si,ax
                add     di,offset SRData
                movsb
                loop    RSetNextEntry
                pop     di
                ret
ReadSet         endp


WriteSet        proc    near
                sti
                cld
                push    di
                push    dx
                mov     di,bx
WSetNextEntry:  mov     si,es:[di].SRPortNum
                mov     byte ptr PortTable[si].PRModFlag,1
                mov     dx,PortTable[si].PRPortAddr
                mov     si,PortTable[si].PRCurrTable
                mov     al,es:[di].SRPtr
                cbw
                add     si,ax
                mov     ah,es:[di].SRData
                mov     [si],ah
                out     dx,ax
                add     di,size SetRec
                loop    WSetNextEntry
                pop     dx
                pop     di
                ret
WriteSet        endp


RevertDefault   proc    near
                sti
                cld
                push    di
                push    es
                push    dx
                push    cx
                push    bx
                mov     ax,ds
                mov     es,ax
                mov     bx,offset PortTable
                xor     cx,cx
RDefNextPtrData:cmp     byte ptr [bx].PRModFlag,ch
                je      RDefNotModified
                mov     byte ptr [bx].PRModFlag,ch
                mov     cl,byte ptr [bx].PRNumRegs
                mov     di,[bx].PRCurrTable
                mov     si,[bx].PRDefTable
                mov     dx,cx
                shr     cx,1
		rep	movsw
		rcl	cx,1
		rep	movsb
                mov     cx,dx
		sub	si,cx
                mov     dx,[bx].PRPortAddr

		cmp	dx,3C0h 		;attribute controller?
		jne	RDef1			;no
		sub	ax,ax			;yes, so update bios areas
		mov	es,ax			;they don't need es anymore
		mov	al,[si+16]		;fetch new mode ctrl
		and	al,00001000b		;isolate blink bit
		and	byte ptr es:[0465h],11011111b
;[1]		shl	al,2
		shl	al,1
		shl	al,1
		or	es:[0465h],al		;store new blink bit
		les	di,es:[04A8h]		;get save area address
		les	di,es:[di+4]		;get 2nd dword ptr there
		mov	ax,es
		or	ax,di			;anything there?
		jz	RDef1			;no
		mov	ax,cx			;save count
		mov	cx,16/2 		;copy 16 palette registers
		rep	movsw			;done!
		inc	si			;skip to the overscan value
		movsb				;copied!
		sub	si,18			;rewind src pointer
		mov	cx,ax			;restore count

RDef1:		mov	al,ch
RDefNextOut:    mov     ah,[si]
                cmp     dx,SequencerAddr                ; SAS 06/09/86
                jne     RDefNextOut1                    ; SAS 06/09/86
                cmp     al,SequencerClMReg              ; SAS 06/09/86
                je      DontRDefOut                     ; SAS 06/09/86
RDefNextOut1:   cmp     dx,GraphicsAddr
                jne     RDefOut
                cmp     al,GraphicsMiscReg
                je      DontRDefOut
RDefOut:        out     dx,ax
DontRDefOut:    inc     si
                inc     al
                loop    RDefNextOut

RDefNotModified:add     bx,size PortRec
                cmp     bx,offset PortTable + (NumPtrData * size PortRec)
		jae	RDef2
		jmp	RDefNextPtrData
RDef2:		cmp	[SingleRegMod],ch
                je      RDEFSinglesOK
                mov     [SingleRegMod],ch
                mov     dx,[bx+size PortRec].PRPortAddr
                mov     al,[DefFeatureReg]
                mov     [FeatureReg],al
                out     dx,al
                mov     dx,3c2h
                mov     al,[DefMiscOutReg]
                mov     [MiscOutReg],al
                out     dx,al
                mov     dx,3cch
                mov     al,[DefGR1PosReg]
                mov     [GR1PosReg],al
                out     dx,al
                mov     dx,3cah
                mov     al,[DefGR2PosReg]
                mov     [GR2PosReg],al
                out     dx,al

RDefSinglesOK:  mov     dx,3c0h
                mov     al,20h
                out     dx,al
                mov     dx,PortTable[5 * size PortRec].PRPortAddr
                in      al,dx
                pop     bx
                pop     cx
                pop     dx
                pop     es
                pop     di
                ret
RevertDefault   endp


DefineDefault   proc    near
                sti
                cld
                push    di
                push    cx
                mov     si,bx
                mov     di,dx
                xor     ch,ch
                mov     cl,byte ptr PortTable[di].PRNumRegs
                mov     byte ptr PortTable[di].PRModFlag,cl
                mov     [SingleRegMod],cl
                mov     di,PortTable[di].PRDefTable
                mov     ax,es
                mov     bx,ds
                mov     es,bx
                mov     ds,ax
                shr     cx,1
		rep	movsw
		rcl	cx,1
		rep	movsb
		mov	es,ax
                mov     ds,bx
                pop     cx
                pop     di
                ret
DefineDefault   endp


GetDefault      proc    near
                sti
                cld
                push    si
                push    di
                push    cx
                mov     di,bx
                mov     si,dx
                xor     ch,ch
                mov     cl,byte ptr PortTable[si].PRNumRegs
                mov     si,PortTable[si].PRDefTable
                shr     cx,1
		rep	movsw
		rcl	cx,1
		rep	movsb
		pop	cx
                pop     di
                pop     si
                ret
GetDefault      endp


InquireDriver   proc    near
                mov     ax,ds
                mov     es,ax
                mov     bx,offset EgaStateVars
		ret
InquireDriver   endp


CallInt10	proc	near			;very dependent on stack!
                push    bp
                mov     bp,sp
                mov     ax,[bp].8
                mov     ds,ax
                mov     si,[bp].6
                mov     ax,[bp].4
                pop     bp
                pushf
                call    cs:[Int10Vector]
                push    bp
                mov     bp,sp
                mov     [bp].4,ax
                mov     [bp].6,si
                mov     ax,ds
                mov     [bp].8,ax
                pop     bp
                ret
CallInt10       endp
