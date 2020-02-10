;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1988-1991
; *                      All Rights Reserved.
; */
M001 07/25/90	HIMEM5.ASM	PIC status read bug
M002 07/26/90	HIMEM.INC	Enabling Toshiba 1600/1200 detection
M003 10/26/90   HIMEM5.ASM	Int 15h errors on blockmove not being 
				returned properly. Fixed crashing bug with
				CV, Emm386 & Smartdrive.
M004 03/11/91   Himem1.asm      Added support for Bull Micral BM60
                Himem2.asm

M005 03/11/91   Himem2.asm      Added conditionals to disable check
                                for 16Mb boundary.  Not active in DOS 5.0.

M006 05/13/91	Himem5.asm	Preserved _textseg in DS, instead of trying
				trying to retrive it from the variable
				textseg everytime. (Because the init code
				is getting executed from a throw away memory
				area). Bug 1579 in DOS 5.1 raid.
M007 05/13/91	Himem2.asm	B#7275 in Win 3.1 database. Made eisa memory
				scan more strict. Earlier himem used only
				to make sure that EISA memory was read/write
				memory. But we need to make sure that it is
				SYSTEM memory & it is not shared memory etc.
M008 05/16/91	Himem1.asm	Implemented A20 handler request API.
		Himem.asm	(INT 2f AX=4308h)

M009 05/20/91	Himem1.asm	Precharge the bus before looking for HP
				signature in ROM.
M010 05/20/91	Himem1.asm	Added DELL XBIOS A20 handler
		Himem2.asm


