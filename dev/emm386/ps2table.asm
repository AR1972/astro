.386p

;****************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;
;   This file is not normal source because it contains no code, all it has 
;   is a binary table which tells EMM386 about which regions of memory 
;   betweeen A000:0000 and EFFF:000F need to beexcluded from use by EMM386 
;   for things like EMM mapping regions.
;
;   Adaptors on PS/2 machines are often programmable as to which regions
;   of address space ROM and/or RAM on the adaptor may occupy. This table
;   allows EMM386 to read the adaptor configuration and exclude the proper
;   regions. EMM386 is capable of correctly excluding some adaptors on
;   PS/2 machines by employing a memory search, but this causes problems
;   with many adaptors:
;
;	o Some adaptors don't actually start "occupying" the address space
;		they use until they are enabled by a piece of software. This
;		defeats the EMM386 scan because the region looks empty if
;		the adaptor is disabled when WIN386 is started.
;
;	o Some adaptors have a RAM area which is actually a memory mapped
;		control area. The EMM386 scan reads and writes as part of
;		its operation, this can alter the adaptor state and disturb
;		the operation of the adaptor (the IBM 3270 card is an example
;		of an adaptor like this, writing to certain adaptor memory
;		locations causes the 3270 adaptor to try to execute an adaptor
;		command).
;
;	o Some adaptors adress ranges are only partly ocupied at any given time
;		based on the current state of the adaptor.
;
;
;   This table tells EMM386 how to interpret the adaptor POS bytes to determine
;   what regions of address space the adaptor is occupying.
;
;   The memory regions which EMM386 cares about for this are typically between
;   page A0 and page FF. Note that the data structure allows the specification
;   of ANY page number between 0 and FFh though.
;
;   *******************************************************
;   *							  *
;   *  Above ability to spec any page in 0-FF range is	  *
;   *  difference between table revs 0000h and 0001h	  *
;   *							  *
;   *******************************************************
;
;   The spec of an adapter range is 4K granular, an adaptor occupies only
;   part of a given 4k page will excluded the entire 4k page.
;
;   The Table is formed like this:
;
;	pPOSCardTable	near pointer to supported options table
;	    TotalOptions    dw	    ?	    ; Number of entries
;	    TabelRev	    dw	    0	    ; REV of table definition
;	    OptionTables    db	    ? DUP(?); Start of OptionStruc structures
;
;   TotalOptions indicates the size of the table. It is the count of how many
;   OptionStruc structures are in the file starting at OptionTables.
;
;   The current TableRev is 0001.
;
;   For each adaptor supported there is an OptionStruc which defines how to
;   exclude the region occupied by the adaptor. The values in these structures
;   are produced by interpreting the information in the @????.ADF file for
;   the adaptor:
;
;	OptionStruc struc
;		OptID	    dw	    ?	    ; 16 bit adaptor ID
;		POS2Mask    db	    ?	    ; How to interpret POS byte 2
;		POS2Shft    db	    ?
;		POS3Mask    db	    ?	    ; How to interpret POS byte 3
;		POS3Shft    db	    ?
;		POS4Mask    db	    ?	    ; How to interpret POS byte 4
;		POS4Shft    db	    ?
;		POS5Mask    db	    ?	    ; How to interpret POS byte 5
;		POS5Shft    db	    ?
;		LookUpCnt   dw	    ?	    ; Number of entries following
;		LookUpTab   db	    ? DUP(?); start of MemAddr Structures
;	OptionStruc ends
;
;	MemAddr struc
;		StartPg     db	    ?	    ; 0 based Physical page #
;					    ;	of first 4k page occupied
;					    ;	by adaptor.
;		PGLen	    db	    ?	    ; Number of 4K pages occupied
;					    ;	(May be 0).
;	MemAddr ends
;
;   This table describes how to convert the adaptor specific POS bytes
;   into the list of 4k pages to mark as used by this adaptor:
;
;     NOTE: In the following code the normal ">>" C operator IS NOT A SHR, IT
;	    IS A ROR.
;
;     if(ThisOptionID == OptionStrPtr->OptID) {
;	LookUpPTR = &OptionStrPtr->LookUpTab; /* point at LookUpTab for adaptor */
;	PgPTR = pPgArray;
;	index  = (POS2Byte & OptionStrPtr->POS2Mask) >> OptionStrPtr->POS2Shft;
;	index |= (POS3Byte & OptionStrPtr->POS3Mask) >> OptionStrPtr->POS3Shft;
;	index |= (POS4Byte & OptionStrPtr->POS4Mask) >> OptionStrPtr->POS4Shft;
;	index |= (POS5Byte & OptionStrPtr->POS5Mask) >> OptionStrPtr->POS5Shft;
;	if(index < LookUpCnt) {
;	   LookUpPTR += index;
;	   PgPTR += LookUpPTR->StartPg;
;	   for(i=0;i < LookUpPTR->PGLen;i++)
;		   *PgPtr++=1;
;	}
;     }
;
;   What is basically going on here is that the POSnMask and POSnShft values
;   specify a combonation of the POS byte bits which produce an index into
;   the LookUpTab MemAddr structure which specifies a region occupied by the
;   adaptor. NOTE THAT THE >> OPERATOR IS ROR NOT SHR!!!!!!! This allows for
;   more flexibility in bit movement.
;   The index variable IS A BYTE, therefore LookUpCnt could be 0FFh at most,
;   but typical values are <= 16. Try to be efficient in forming the index.
;   Don't waste space with lots of invalid entries.
;
;   POS bytes to be ignored are indicated by POSnMask = POSnShft = 0.
;
;   THE POS BYTES NUMBERS IN THIS FILE ARE DIFFERENT THAT THE POS BYTE NUMBERS
;   SPECIFIED IN THE .ADF FILE. THE DELTA IS 2. This is because the numbers in
;   this file are PHYSICAL POS byte numbers. The numbers in the ADF file are
;   logical. PHYSICAL POS bytes 0 and 1 are the adaptor ID POS bytes.
;
;	.ADF file POS	WIN386.PS2 POS
;	  byte #	   byte #
;     ==================================
;	    0		     2
;	    1		     3
;	    2		     4
;	    3		     5
;
;   Invalid entries in LookUpTab MemAddr structures should set
;     StartAddr = PGLen = 0.
;
;   NOTE to support adaptors with multiple discontiguous memory use
;   blocks, make more than one entry for the card ID
;
;   Note: Ported from win386 3.0 sources
;
;
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 TAG 		Description
;==	-------- --------	--------------------------------------------
;==	01/21/91 M003		Added an entry for the IBM Artic Adapter
;==				(@EFF0.ADF). This has been ssynched up from
;==				the win386 project.
;=============================================================================


include	vdmseg.inc

LAST	SEGMENT
	assume cs:last

public	PS2DATA
PS2DATA	Label	BYTE

	dw	19	; Total options
	dw	0001h	; Table Rev
;
; AdapterName  "The IBM 3270 Connection Version A"
;
	dw	0E7FFH	; ID of 3270 card V A
	db	0	; Ignore all POS bytes
	db	0
	db	0
	db	0
	db	0
	db	0
	db	0
	db	0
	dw	1	; One entry
	db	0CEh
	db	2
;
; AdapterName "The IBM 3270 Connection Version B"
;
	dw	0E1FFH	; ID of 3270 card V B
	db	0	; Ignore POS[2]
	db	0
	db	00011110B
	db	1
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[4]
	db	0
	dw	16	; 16  entrys
	db	0	; 0000 invalid
	db	0
	db	0	; 0001 invalid
	db	0
	db	0	; 0010 invalid
	db	0
	db	0	; 0011 invalid
	db	0
	db	0	; 0100 invalid
	db	0
	db	0CAh	; 0101 0CA000h- 0CBFFFh
	db	2
	db	0CCh	; 0110 0CC000h- 0CDFFFh
	db	2
	db	0CEh	; 0111 0CE000h- 0CFFFFh
	db	2
	db	0D0h	; 1000 0D0000h- 0D1FFFh
	db	2
	db	0D2h	; 1001 0D2000h- 0D3FFFh
	db	2
	db	0D4h	; 1010 0D4000h- 0D5FFFh
	db	2
	db	0D6h	; 1011 0D6000h- 0D7FFFh
	db	2
	db	0D8h	; 1100 0D8000h- 0D9FFFh
	db	2
	db	0DAh	; 1101 0DA000h- 0DBFFFh
	db	2
	db	0DCh	; 1110 0DC000h- 0DDFFFh
	db	2
	db	0DEh	; 1111 0DE000h- 0DFFFFh
	db	2
;
; AdapterName  "Ungermann-Bass NICps/2 Ethernet LAN Adapter"
;
;  NOTE: THAT THIS ADAPTOR HAS 2 ENTRIES. One for RAM region, One for ROM
;	 region.
;
	dw	0EFF5H	; ID of UB net card RAM
	db	0	; Ignore POS[2]
	db	0
	db	10000001B
	db	6
	db	0	; Ignore POS[4]
	db	0
	db	00010010B
	db	1
	dw	13	; 13 entrys (last three of normal 16 are invalid)
	db	0C0h	; 0000	mem 0c0000h-0c7fffh
	db	8
	db	0C8h	; 0001	mem 0c8000h-0cffffh
	db	8
	db	0	; 0010 invalid
	db	0
	db	0	; 0011 invalid
	db	0
	db	0D0h	; 0100	mem 0d0000h-0d7fffh
	db	8
	db	0D8h	; 0101	mem 0d8000h-0dffffh
	db	8
	db	0	; 0110 invalid
	db	0
	db	0	; 0111 invalid
	db	0
	db	0C0h	; 1000	mem 0c0000h-0cffffh
	db	16
	db	0	; 1001 invalid
	db	0
	db	0	; 1010 invalid
	db	0
	db	0	; 1011 invalid
	db	0
	db	0D0h	; 1100	mem 0d0000h-0dffffh
	db	16
		; NOTE 1110 is "valid" but specifies a range that
		;    is outside V86 mode addressability.
		; 1110	mem 0e10000h-0e1ffffh

	dw	0EFF5H	; ID of UB net card ROM
	db	0	; Ignore POS[2]
	db	0
	db	0	; Ignore POS[3]
	db	0
	db	10000001B
	db	5
	db	00001100B
	db	2
	dw	12	; 12 entrys (last 4 of normal 16 are invalid)
	db	0C0h	; 0000 mem 0c0000h-0c3fffh
	db	4
	db	0C4h	; 0001 mem 0c4000h-0c7fffh
	db	4
	db	0C8h	; 0010 mem 0c8000h-0cbfffh
	db	4
	db	0CCh	; 0011 mem 0cc000h-0cffffh
	db	4
	db	0	; 0100 NOTE 0100 is "valid" but specifies a range that
	db	0	;	       is outside V86 mode addressability.
			;	       mem 0E00000h-0E03fffh
	db	0	; 0101 invalid
	db	0
	db	0	; 0110 invalid
	db	0
	db	0	; 0111 invalid
	db	0
	db	0D0h	; 1000 mem 0d0000h-0d3fffh
	db	4
	db	0D4h	; 1001 mem 0d4000h-0d7fffh
	db	4
	db	0D8h	; 1010 mem 0d8000h-0dbfffh
	db	4
	db	0DCh	; 1011 mem 0dc000h-0dffffh
	db	4
;
; AdapterName "Ungermann-Bass Net/One PSNIU LAN Adapter"
;
;  NOTE: THAT THIS ADAPTOR HAS 2 ENTRIES.
;
	dw	07012H	; ID of UB net card shared RAM
	db	01000000b
	db	3
	db	00000111b
	db	0
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[5]
	db	0
	dw	16	; 16 entrys
	db	0	; 0000 invalid
	db	0
	db	0	; 0001 invalid
	db	0
	db	0C8h	; 0010 Mem 0C8000h-0CFFFFh
	db	8
	db	0	; 0011 invalid
	db	0
	db	0	; 0100 invalid
	db	0
	db	0	; 0101 invalid
	db	0
	db	0D8h	; 0110 Mem 0D8000h-0DFFFFh
	db	8
	db	0	; 0111 invalid
	db	0
	db	0	; 1000 invalid
	db	0
	db	0	; 1001 invalid
	db	0
	db	0C8h	; 1010 Mem 0C8000h-0CBFFFh
	db	4
	db	0CCh	; 1011 Mem 0CC000h-0CFFFFh
	db	4
	db	0	; 1100 invalid
	db	0
	db	0	; 1101 invalid
	db	0
	db	0D8h	; 1110 Mem 0D8000h-0DBFFFh
	db	4
	db	0DCh	; 1111 Mem 0DC000h-0DFFFFh
	db	4

	dw	07012H	; ID of UB net card Remote IPL and 3270 emulation
	db	00001100b
	db	7
	db	01110000b
	db	4
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[5]
	db	0
	dw	32	; 32 entrys
	db	0	; 00000 These 8 are 3270 disabled remote IPL disabled
	db	0	;	  In this case no additional space is used
	db	0	; 00001
	db	0
	db	0	; 00010
	db	0
	db	0	; 00011
	db	0
	db	0	; 00100
	db	0
	db	0	; 00101
	db	0
	db	0	; 00110
	db	0
	db	0	; 00111
	db	0
	db	0CEh	; 01000 These 8 are 3270 enabled remote IPL disabled
	db	2	;	in this case 3270 window at CE000-CFFFF
	db	0CEh	; 01001
	db	2	;
	db	0CEh	; 01010
	db	2	;
	db	0CEh	; 01011
	db	2
	db	0CEh	; 01100
	db	2
	db	0CEh	; 01101
	db	2
	db	0CEh	; 01110
	db	2
	db	0CEh	; 01111
	db	2
	db	0	; 10000 Invalid These 8 are 3270 disabled remote IPL
	db	0	;		enabled in this case RIPL window
	db	0	; 10001 invalid address is set
	db	0
	db	0C8h	; 10010 Mem 0C8000h-0C9FFFh
	db	2
	db	0CCh	; 10011 Mem 0CC000h-0CDFFFh
	db	2
	db	0	; 10100 invalid
	db	0
	db	0	; 10101 invalid
	db	0
	db	0D8h	; 10110 Mem 0D8000h-0D9FFFh
	db	2
	db	0DCh	; 10111 Mem 0DC000h-0DDFFFh
	db	2
	db	0CCh	; 11000 These 8 are 3270 enabled remote IPL enabled
	db	4	;	In this case RIPL window fixed at CC000-CDFFF
	db	0CCh	; 11001 3270 window fixed at CE000-CFFFF
	db	4
	db	0CCh	; 11010
	db	4
	db	0CCh	; 11011
	db	4
	db	0CCh	; 11100
	db	4
	db	0CCh	; 11101
	db	4
	db	0CCh	; 11110
	db	4
	db	0CCh	; 11111
	db	4
;
; AdapterName "IBM Token-Ring Network Adapter/A"
;
;  NOTE: THAT THIS ADAPTOR HAS 2 ENTRIES. One for RAM region, One for ROM
;	 region.
;
	dw	0E000H	; ID of IBM Token Ring card RAM
	db	00011100B
	db	2
	db	0	; Ignore POS[3]-[5]
	db	0
	db	0
	db	0
	db	0
	db	0
	dw	8	; 8 entrys
	db	0C0h	; 000 MEM 0C0000h - 0C3FFFh
	db	4
	db	0C4h	; 001 MEM 0C4000h - 0C7FFFh
	db	4
	db	0C8h	; 010 MEM 0C8000h - 0CBFFFh
	db	4
	db	0CCh	; 011 MEM 0CC000h - 0CFFFFh
	db	4
	db	0D0h	; 100 MEM 0D0000h - 0D3FFFh
	db	4
	db	0D4h	; 101 MEM 0D4000h - 0D7FFFh
	db	4
	db	0D8h	; 110 MEM 0D8000h - 0DBFFFh
	db	4
	db	0DCh	; 111 MEM 0DC000h - 0DFFFFh
	db	4


	dw	0E000H	; ID of IBM Token Ring card ROM
	db	0	; Ignore POS[2]
	db	0
	db	0	; Ignore POS[3]
	db	0
	db	00011110B
	db	1
	db	0	; Ignore POS[5]
	db	0
	dw	16	; 16 entrys
	db	0C0h	; 0000 MEM 0C0000h - 0C1FFFh
	db	2
	db	0C2h	; 0001 MEM 0C2000h - 0C3FFFh
	db	2
	db	0C4h	; 0010 MEM 0C4000h - 0C5FFFh
	db	2
	db	0C6h	; 0011 MEM 0C6000h - 0C7FFFh
	db	2
	db	0C8h	; 0100 MEM 0C8000h - 0C9FFFh
	db	2
	db	0CAh	; 0101 MEM 0CA000h - 0CBFFFh
	db	2
	db	0CCh	; 0110 MEM 0CC000h - 0CDFFFh
	db	2
	db	0CEh	; 0111 MEM 0CE000h - 0CFFFFh
	db	2
	db	0D0h	; 1000 MEM 0D0000h - 0D1FFFh
	db	2
	db	0D2h	; 1001 MEM 0D2000h - 0D3FFFh
	db	2
	db	0D4h	; 1010 MEM 0D4000h - 0D5FFFh
	db	2
	db	0D6h	; 1011 MEM 0D6000h - 0D7FFFh
	db	2
	db	0D8h	; 1100 MEM 0D8000h - 0D9FFFh
	db	2
	db	0DAh	; 1101 MEM 0DA000h - 0DBFFFh
	db	2
	db	0DCh	; 1110 MEM 0DC000h - 0DDFFFh
	db	2
	db	0DEh	; 1111 MEM 0DE000h - 0DFFFFh
	db	2
;
; AdapterName  "IBM Display Adapter 8514/A"
;
;  NOTE: that this adaptor IGNORES all the POS bytes. The 8514 adaptor memory
;	use locations are FIXED and cannot be changed.
;
;  C6800-C7FFF and CA000-CA7FF
;
;  Two entries, one for each block
;
	dw	0EF7FH	; ID of IBM 8514/A adaptor C6800-C7FFF
	db	0	; Ignore POS[2]
	db	0
	db	0	; Ignore POS[3]
	db	0
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[5]
	db	0
	dw	1	; 1 entry
	db	0C6h
	db	2

	dw	0EF7FH	; ID of IBM 8514/A adaptor CA000-CA7FF
	db	0	; Ignore POS[2]
	db	0
	db	0	; Ignore POS[3]
	db	0
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[5]
	db	0
	dw	1	; 1 entry
	db	0CAh
	db	1
;
; AdapterName  "PC Network Adapter "
;
	dw	0EFEFh	; ID of PC Network adaptor
	db	00001110b
	db	1
	db	0	; Ignore POS[3]
	db	0
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[5]
	db	0
	dw	8	; 8 entrys
	db	0	; 000 invalid
	db	0
	db	0D0h	; 001 mem 0D0000h - 0D7FFFh
	db	8
	db	0C8h	; 010 mem 0C8000h - 0CFFFFh
	db	8
	db	0D8h	; 011 mem 0D8000h - 0DFFFFh
	db	8
	db	0	; 100 invalid
	db	0
	db	0D6h	; 101 mem 0D6000h - 0D7FFFh
	db	2
	db	0CEh	; 110 mem 0CE000h - 0CFFFFh
	db	2
	db	0DEh	; 111 mem 0DE000h - 0DFFFFh
	db	2
;
; AdapterName  "3117 Scanner Adapter/A"
;
	dw	0F04FH	; ID of 3117
	db	11110000b
	db	4
	db	0	; Ignore POS[3]
	db	0
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[5]
	db	0
	dw	12	; 12 entrys (last 4 of normal 16 are invalid)
	db	0DCh	; 0000 mem 0DC000h - 0DDFFFh
	db	2
	db	0DEh	; 0001 mem 0DE000h - 0DFFFFh
	db	2
	db	0D8h	; 0010 mem 0D8000h - 0D9FFFh
	db	2
	db	0DAh	; 0011 mem 0DA000h - 0DBFFFh
	db	2
	db	0D4h	; 0100 mem 0D4000h - 0D5FFFh
	db	2
	db	0D6h	; 0101 mem 0D6000h - 0D7FFFh
	db	2
	db	0D0h	; 0110 mem 0D0000h - 0D1FFFh
	db	2
	db	0D2h	; 0111 mem 0D2000h - 0D3FFFh
	db	2
	db	0CCh	; 1000 mem 0CC000h - 0CEFFFh
	db	3
	db	0CEh	; 1001 mem 0CE000h - 0CFFFFh
	db	2
	db	0C8h	; 1010 mem 0C8000h - 0C9FFFh
	db	2
	db	0CAh	; 1011 mem 0CA000h - 0CBFFFh
	db	2
;
; AdapterName  "High Speed Adapter/A"
;
	dw	0E04EH	; ID of 3117
	db	11110000b
	db	4
	db	0	; Ignore POS[3]
	db	0
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[5]
	db	0
	dw	12	; 12 entrys (last 4 of normal 16 are invalid)
	db	0DEh	; 0000 mem 0DE000h - 0DFFFFh
	db	2
	db	0DCh	; 0001 mem 0DC000h - 0DDFFFh
	db	2
	db	0DAh	; 0010 mem 0DA000h - 0DBFFFh
	db	2
	db	0D8h	; 0011 mem 0D8000h - 0D9FFFh
	db	2
	db	0D6h	; 0100 mem 0D6000h - 0D7FFFh
	db	2
	db	0D4h	; 0101 mem 0D4000h - 0D5FFFh
	db	2
	db	0D2h	; 0110 mem 0D2000h - 0D3FFFh
	db	2
	db	0D0h	; 0111 mem 0D0000h - 0D1FFFh
	db	2
	db	0CEh	; 1000 mem 0CE000h - 0CFFFFh
	db	2
	db	0CCh	; 1001 mem 0CC000h - 0CDFFFh
	db	2
	db	0CAh	; 1010 mem 0CA000h - 0CBFFFh
	db	2
	db	0C8h	; 1011 mem 0C8000h - 0C9FFFh
	db	2
;
; AdapterName  "3119 Adapter/A"
;
	dw	0E04EH	; ID of 3117
	db	11110000b
	db	4
	db	0	; Ignore POS[3]
	db	0
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[5]
	db	0
	dw	12	; 12 entrys (last 4 of normal 16 are invalid)
	db	0DEh	; 0000 mem 0DE000h - 0DFFFFh
	db	2
	db	0DCh	; 0001 mem 0DC000h - 0DDFFFh
	db	2
	db	0DAh	; 0010 mem 0DA000h - 0DBFFFh
	db	2
	db	0D8h	; 0011 mem 0D8000h - 0D9FFFh
	db	2
	db	0D6h	; 0100 mem 0D6000h - 0D7FFFh
	db	2
	db	0D4h	; 0101 mem 0D4000h - 0D5FFFh
	db	2
	db	0D2h	; 0110 mem 0D2000h - 0D3FFFh
	db	2
	db	0D0h	; 0111 mem 0D0000h - 0D1FFFh
	db	2
	db	0CEh	; 1000 mem 0CE000h - 0CFFFFh
	db	2
	db	0CCh	; 1001 mem 0CC000h - 0CDFFFh
	db	2
	db	0CAh	; 1010 mem 0CA000h - 0CBFFFh
	db	2
	db	0C8h	; 1011 mem 0C8000h - 0C9FFFh
	db	2
;
; AdapterName "3Com EtherLink/MC Ethernet Adapter"
;
	dw	06042h	; ID
	db	00011000b
	db	3
	db	0	; Ignore POS[3]
	db	0
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[5]
	db	0
	dw	4	; 4 entrys
	db	0C0h	; 00 pos[0]=XXX00XXXb  mem 0c0000h-0C5FFFh
	db	6
	db	0C8h	; 01 pos[0]=XXX01XXXb  mem 0c8000h-0CDFFFh
	db	6
	db	0D0h	; 10 pos[0]=XXX10XXXb  mem 0d0000h-0D5FFFh
	db	6
	db	0D8h	; 11 pos[0]=XXX11XXXb  mem 0d8000h-0DDFFFh
	db	6
;
; AdapterName "ATTACHMATE - Advanced 3270 Adapter/2"
;
	dw	0677Fh	; ID
	db	0	; Ignore POS[2]
	db	0
	db	0	; Ignore POS[3]
	db	0
	db	00011111B
	db	1
	db	0	; Ignore POS[5]
	db	0
	dw	16	; 16 entrys NOTE: The low bit is the "mem disabled"
			;    bit, by rotating by one, this bit is placed in
			;    the high bit. Thus any disabled settings are
			;    "out of range" and we correctly ignore them.
	db	0C0h	; 0000 mem 0C0000h-0C1FFFh
	db	2
	db	0C2h	; 0001 mem 0C2000h-0C3FFFh
	db	2
	db	0C4h	; 0010 mem 0C4000h-0C5FFFh
	db	2
	db	0C6h	; 0011 mem 0C6000h-0C7FFFh
	db	2
	db	0C8h	; 0100 mem 0C8000h-0C9FFFh
	db	2
	db	0CAh	; 0101 mem 0CA000h-0CBFFFh
	db	2
	db	0CCh	; 0110 mem 0CC000h-0CDFFFh
	db	2
	db	0CEh	; 0111 mem 0CE000h-0CFFFFh
	db	2
	db	0D0h	; 1000 mem 0D0000h-0D1FFFh
	db	2
	db	0D2h	; 1001 mem 0D2000h-0D3FFFh
	db	2
	db	0D4h	; 1010 mem 0D4000h-0D5FFFh
	db	2
	db	0D6h	; 1011 mem 0D6000h-0D7FFFh
	db	2
	db	0D8h	; 1100 mem 0D8000h-0D9FFFh
	db	2
	db	0DAh	; 1101 mem 0DA000h-0DBFFFh
	db	2
	db	0DCh	; 1110 mem 0DC000h-0DDFFFh
	db	2
	db	0DEh	; 1111 mem 0DE000h-0DFFFFh
	db	2
;
; IBM Token-Ring Network 16/4 Adapter/A
;
;  NOTE: THAT THIS ADAPTOR HAS 2 ENTRIES. One for RAM region, One for ROM
;	 region.
;
	dw	0E001h	; ID ROM
	db	0	; Ignore POS[2]
	db	0
	db	0	; Ignore POS[3]
	db	0
	db	00011110B
	db	1
	db	0	; Ignore POS[5]
	db	0
	dw	16	; 16 entrys
	db	0C0h	; 0000 MEM 0C0000h - 0C1FFFh
	db	2
	db	0C2h	; 0001 MEM 0C2000h - 0C3FFFh
	db	2
	db	0C4h	; 0010 MEM 0C4000h - 0C5FFFh
	db	2
	db	0C6h	; 0011 MEM 0C6000h - 0C7FFFh
	db	2
	db	0C8h	; 0100 MEM 0C8000h - 0C9FFFh
	db	2
	db	0CAh	; 0101 MEM 0CA000h - 0CBFFFh
	db	2
	db	0CCh	; 0110 MEM 0CC000h - 0CDFFFh
	db	2
	db	0CEh	; 0111 MEM 0CE000h - 0CFFFFh
	db	2
	db	0D0h	; 1000 MEM 0D0000h - 0D1FFFh
	db	2
	db	0D2h	; 1001 MEM 0D2000h - 0D3FFFh
	db	2
	db	0D4h	; 1010 MEM 0D4000h - 0D5FFFh
	db	2
	db	0D6h	; 1011 MEM 0D6000h - 0D7FFFh
	db	2
	db	0D8h	; 1100 MEM 0D8000h - 0D9FFFh
	db	2
	db	0DAh	; 1101 MEM 0DA000h - 0DBFFFh
	db	2
	db	0DCh	; 1110 MEM 0DC000h - 0DDFFFh
	db	2
	db	0DEh	; 1111 MEM 0DE000h - 0DFFFFh
	db	2

	dw	0E001h	; ID RAM
	db	00011110b
	db	1
	db	00001100b
	db	6
	db	0	; Ignore POS[4]
	db	0
	db	0	; Ignore POS[5]
	db	0
	dw	57	; 57 entrys (last 7 of normal 64 are invalid)
   ;8k RAM
	db	0C0h	; 000000 MEM 0C0000h - 0C1FFFh
	db	2
	db	0C2h	; 000001 MEM 0C2000h - 0C3FFFh
	db	2
	db	0C4h	; 000010 MEM 0C4000h - 0C5FFFh
	db	2
	db	0C6h	; 000011 MEM 0C6000h - 0C7FFFh
	db	2
	db	0C8h	; 000100 MEM 0C8000h - 0C9FFFh
	db	2
	db	0CAh	; 000101 MEM 0CA000h - 0CBFFFh
	db	2
	db	0CCh	; 000110 MEM 0CC000h - 0CDFFFh
	db	2
	db	0CEh	; 000111 MEM 0CE000h - 0CFFFFh
	db	2
	db	0D0h	; 001000 MEM 0D0000h - 0D1FFFh
	db	2
	db	0D2h	; 001001 MEM 0D2000h - 0D3FFFh
	db	2
	db	0D4h	; 001010 MEM 0D4000h - 0D5FFFh
	db	2
	db	0D6h	; 001011 MEM 0D6000h - 0D7FFFh
	db	2
	db	0D8h	; 001100 MEM 0D8000h - 0D9FFFh
	db	2
	db	0DAh	; 001101 MEM 0DA000h - 0DBFFFh
	db	2
	db	0DCh	; 001110 MEM 0DC000h - 0DDFFFh
	db	2
	db	0DEh	; 001111 MEM 0DE000h - 0DFFFFh
	db	2
   ;16k RAM
	db	0C0h	; 010000 MEM 0C0000h - 0C3FFFh
	db	4
	db	0	; 010001 Invalid
	db	0
	db	0C4h	; 010010 MEM 0C4000h - 0C7FFFh
	db	4
	db	0	; 010011 Invalid
	db	0
	db	0C8h	; 010100 MEM 0C8000h - 0CBFFFh
	db	4
	db	0	; 010101 Invalid
	db	0
	db	0CCh	; 010110 MEM 0CC000h - 0CFFFFh
	db	4
	db	0	; 010111 Invalid
	db	0
	db	0D0h	; 011000 MEM 0D0000h - 0D3FFFh
	db	4
	db	0	; 011001 Invalid
	db	0
	db	0D4h	; 011010 MEM 0D4000h - 0D7FFFh
	db	4
	db	0	; 011011 Invalid
	db	0
	db	0D8h	; 011100 MEM 0D8000h - 0DBFFFh
	db	4
	db	0	; 011101 Invalid
	db	0
	db	0DCh	; 011110 MEM 0DC000h - 0DFFFFh
	db	4
	db	0	; 011111 Invalid
	db	0
   ;32k RAM
	db	0C0h	; 100000 MEM 0C0000h - 0C7FFFh
	db	8
	db	0	; 100001 Invalid
	db	0
	db	0	; 100010 Invalid
	db	0
	db	0	; 100011 Invalid
	db	0
	db	0C8h	; 100100 MEM 0C8000h - 0CFFFFh
	db	8
	db	0	; 100101 Invalid
	db	0
	db	0	; 100110 Invalid
	db	0
	db	0	; 100111 Invalid
	db	0
	db	0D0h	; 101000 MEM 0D0000h - 0D7FFFh
	db	8
	db	0	; 101001 Invalid
	db	0
	db	0	; 101010 Invalid
	db	0
	db	0	; 101011 Invalid
	db	0
	db	0D8h	; 101100 MEM 0D8000h - 0DFFFFh
	db	8
	db	0	; 101101 Invalid
	db	0
	db	0	; 101110 Invalid
	db	0
	db	0	; 101111 Invalid
	db	0
   ;64k RAM
	db	0C0h	; 110000 MEM 0C0000h - 0CFFFFh
	db	16
	db	0	; 110001 Invalid
	db	0
	db	0	; 110010 Invalid
	db	0
	db	0	; 110011 Invalid
	db	0
	db	0	; 110100 Invalid
	db	0
	db	0	; 110101 Invalid
	db	0
	db	0	; 110110 Invalid
	db	0
	db	0	; 110111 Invalid
	db	0
	db	0D0h	; 111000 MEM 0D0000h - 0DFFFFh
	db	16
;
; M003 - Start
; AdapterName "IBM Realtime Interface Co-processor Multiport/2 or X.25 /2"
;
	dw	0EFF0H	; ID of 3270 card V A
	db	0	; Ignore POS[2]
	db	0
	db	00001111B  ; Start addr
	db	0
	db	00000001B  ; > 1Meg memory select
	db	2
	db	00000011B  ; Size, 00 8K, 01 16K, 10 32K, 11 64k
	db	4
	dw	57	; All high bit set entries are > 1Meg memory configs
			;  last 7 entries of normal 64 are invalid
     ;8K
	db	0C0h	; 0000000 MEM 0C0000h - 0C1FFFh
	db	2
	db	0C2h	; 0000001 MEM 0C2000h - 0C3FFFh
	db	2
	db	0C4h	; 0000010 MEM 0C4000h - 0C5FFFh
	db	2
	db	0C6h	; 0000011 MEM 0C6000h - 0C7FFFh
	db	2
	db	0C8h	; 0000100 MEM 0C8000h - 0C9FFFh
	db	2
	db	0CAh	; 0000101 MEM 0CA000h - 0CBFFFh
	db	2
	db	0CCh	; 0000110 MEM 0CC000h - 0CDFFFh
	db	2
	db	0CEh	; 0000111 MEM 0CE000h - 0CFFFFh
	db	2
	db	0D0h	; 0001000 MEM 0D0000h - 0D1FFFh
	db	2
	db	0D2h	; 0001001 MEM 0D2000h - 0D3FFFh
	db	2
	db	0D4h	; 0001010 MEM 0D4000h - 0D5FFFh
	db	2
	db	0D6h	; 0001011 MEM 0D6000h - 0D7FFFh
	db	2
	db	0D8h	; 0001100 MEM 0D8000h - 0D9FFFh
	db	2
	db	0DAh	; 0001101 MEM 0DA000h - 0DBFFFh
	db	2
	db	0DCh	; 0001110 MEM 0DC000h - 0DDFFFh
	db	2
	db	0DEh	; 0001111 MEM 0DE000h - 0DFFFFh
	db	2
     ;16K
	db	0C0h	; 0010000 MEM 0C0000h - 0C3FFFh
	db	4
	db	0	; 0010001 Invalid
	db	0
	db	0C4h	; 0010010 MEM 0C4000h - 0C7FFFh
	db	4
	db	0	; 0010011 Invalid
	db	0
	db	0C8h	; 0010100 MEM 0C8000h - 0CBFFFh
	db	4
	db	0	; 0010101 Invalid
	db	0
	db	0CCh	; 0010110 MEM 0CC000h - 0CFFFFh
	db	4
	db	0	; 0010111 Invalid
	db	0
	db	0D0h	; 0011000 MEM 0D0000h - 0D3FFFh
	db	4
	db	0	; 0011001 Invalid
	db	0
	db	0D4h	; 0011010 MEM 0D4000h - 0D7FFFh
	db	4
	db	0	; 0011011 Invalid
	db	0
	db	0D8h	; 0011100 MEM 0D8000h - 0DBFFFh
	db	4
	db	0	; 0011101 Invalid
	db	0
	db	0DCh	; 0011110 MEM 0DC000h - 0DFFFFh
	db	4
	db	0	; 0011111 Invalid
	db	0
    ;32K
	db	0C0h	; 0100000 MEM 0C0000h - 0C7FFFh
	db	8
	db	0	; 0100001 Invalid
	db	0
	db	0	; 0100010 Invalid
	db	0
	db	0	; 0100011 Invalid
	db	0
	db	0C8h	; 0100100 MEM 0C8000h - 0CFFFFh
	db	8
	db	0	; 0100101 Invalid
	db	0
	db	0	; 0100110 Invalid
	db	0
	db	0	; 0100111 Invalid
	db	0
	db	0D0h	; 0101000 MEM 0D0000h - 0D7FFFh
	db	8
	db	0	; 0101001 Invalid
	db	0
	db	0	; 0101010 Invalid
	db	0
	db	0	; 0101011 Invalid
	db	0
	db	0D8h	; 0101100 MEM 0D8000h - 0DFFFFh
	db	8
	db	0	; 0101101 Invalid
	db	0
	db	0	; 0101110 Invalid
	db	0
	db	0	; 0101111 Invalid
	db	0
    ;64K
	db	0C0h	; 0110000 MEM 0C0000h - 0CFFFFh
	db	16
	db	0	; 0110001 Invalid
	db	0
	db	0	; 0110010 Invalid
	db	0
	db	0	; 0110011 Invalid
	db	0
	db	0	; 0110100 Invalid
	db	0
	db	0	; 0110101 Invalid
	db	0
	db	0	; 0110110 Invalid
	db	0
	db	0	; 0110111 Invalid
	db	0
	db	0D0h	; 0111000 MEM 0D0000h - 0DFFFFh
	db	16

;
; M003 - End
;


LAST ENDS
     END 
