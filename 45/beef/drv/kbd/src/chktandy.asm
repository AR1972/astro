;---------------------------------------------------------------------------
;CHKTANDY.ASM
;
;       Routines to check for TANDY 1000 machine and install the TANDY
;       keyboard driver for CW
;---------------------------------------------------------------------------

        include drv.inc
        include fxdrv.inc
        include inkbd.inc

;---------------------------------------------------------------------------
;Keyboard driver special data segment
;---------------------------------------------------------------------------
sBegin  DATA

externB inkj
externB tnkj

sEnd    DATA

;---------------------------------------------------------------------------
;Keyboard driver segment
;---------------------------------------------------------------------------
sBegin	DRV
    assumes CS,DRV
    assumes ds,NOTHING
    assumes ss,NOTHING

;---------------------------------------------------------------------------
;InstallTandyKBDDRV
;
;	Copies the function jump table in TANDY.ASM to the currently
;	installed table (FX_KBD3.ASM)
;
;	entry:	n/a
;
;	exit:	n/a
;
;	crunch: AX, CX
;---------------------------------------------------------------------------

cProc	InstallTandyKBDDRV,<FAR, PUBLIC>,<SI,DI,DS>
cBegin	InstallTandyKBDDRV

        mov     ax, DGROUP
        mov     es, ax
        mov     ds, ax
        mov     si, DATAoffset tnkj             ;addr if tandy jmp-table
        mov     di, DATAoffset inkj             ;addr of current jmp-table
        mov	cx, cpfnKbdMin * 2		;Number of entries (DWords)
        rep	movsw				;install new table
        
cEnd	InstallTandyKBDDRV

;----------------------------------------------------------------------------
;FCheckTandy1000
;	Check for Tandy 1000 Machine, and call the Tandy KBD DRV install
;       routine if detected.
;
;	entry:	n/a
;
;	exit:   n/a
;----------------------------------------------------------------------------

segROM	EQU     0F000H

cProc	FCheckTandy1000, <PUBLIC, FAR>, <>
cBegin	nogen	; FCheckTandy1000

	push	ds
	mov	ax,segROM
	mov	ds,ax
	cmp	byte ptr ds:[0C000h],21h
	pop	ds

	jne	fcTandyNo
        call    InstallTandyKBDDRV

fcTandyNo:
	ret

cEnd	nogen	; FCheckTandy1000

sEnd    DRV

        end
