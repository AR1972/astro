;********************************************************************
;								    *
;   (C) Copyright MICROSOFT Corp., 1988-1991			    *
;								    *
;   MODULE:	PS2EQU.INC					    *
;								    *
;   WRITTEN BY: SUNIL PAI					    *
;								    *
;   DATE:	9/21/1987					    *
;								    *
;   PURPOSE:	TO DEFINE HARDWARE EQUATES FOR THE PS/2 MEMORY	    *
;								    *
;********************************************************************

; Micro Channel Architecture:
;
; The IBM micro channel architecture provides a number of connectors
; in which one can put expansion boards.  The programmable option
; select (POS) eliminates switches from the system board and adapters
; by replacing their function with programmable registers.  The adapters
; and system board all share I/O addresses 100h - 107h.  The system
; board POS byte (94h) and the adapter POS port (96h) control the
; device unique setup signals for all devices.
;
; POS I/O Address Space:
;
;   Address	Function
;    (hex)
;
;   0094	System Board Enable / Setup
;   0095	Reserved
;   0096	Adapter Enable / Setup
;   0097	Reserved
;   0100	POS Reg. 0 - Adapter ID byte (LSB)
;   0101	POS Reg. 1 - Adapter ID byte (MSB)
;   0102	POS Reg. 2 - Option Select Data Byte 1 ( bit 0 is card enable )
;   0103	POS Reg. 3 - Option Select Data Byte 2
;   0104	POS Reg. 4 - Option Select Data Byte 3
;   0105	POS Reg. 5 - Option Select Data Byte 4
;   0106	POS Reg. 6 - Subaddress Extension (LSB)
;   0107	POS Reg. 7 - Subaddress Extension (MSB)
;
;
; SYSTEM BOARD POS BYTE (94h):
;
;     ---------------------------------
;     | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
;     ---------------------------------
;	|   |	|   |	|   |	|   |
;	|   |	|   -----------------------Reserved
;	|   |	|
;	|   |	---------------------------VGA +Enbl/-Setup
;	|   |
;	|   -------------------------------Reserved
;	|
;	--------------------------- -------System Board Functions +Enbl/-Setup
;
    ;
    ; System POS Port
    ;
    SYSTEM_POS	EQU	094h
    ;
	;
	; equates for this byte
	;
	VGA_ENB    equ     20h
	VGA_SET    equ	   NOT VGA_ENB
	;
	SYS_ENB    equ     80h
	SYS_SET    equ	   NOT SYS_ENB
;
;
;   ADAPTER POS BYTE (96h):
;
;		  ---------------------------------
;   Channel Sel.  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
;   for setup	  ---------------------------------
;
;	None	    0	0   0	0   0	X   X	X
;	 1	    0	0   0	0   1	0   0	0
;	 2	    0	0   0	0   1	0   0	1
;	 3	    0	0   0	0   1	0   1	0
;	 4	    0	0   0	0   1	0   1	1
;	 5	    0	0   0	0   1	1   0	0
;	 6	    0	0   0	0   1	1   0	1
;	 7	    0	0   0	0   1	1   1	0
;	 8	    0	0   0	0   1	1   1	1
;   channel reset   1	0   0	0   X	X   X	X
;
;   Each channel has a unique setup line (-CD SETUP) associated with it.
;   Prior to a setup cycle, an I/O operation to port hex 0096h sets up
;   the bit pattern to select the the channel position to which the
;   subsequent setup operation will occur.
;
;   NOTE: The status of port hex 0096 can be read by software. However bits
;	  4, 5 and 6 will be read as 1
    ;
    ; Adapter POS Port
    ;
    ADAPTER_POS EQU	096h
    ;
	;
	; equates for this byte
	;
	ADAPTER_DSB	    equ	    0
	ADAPTER_ENB	    equ     08h
	ADAPTER_POS_MASK    equ     NOT ADAPTER_ENB
	CHANNEL_MASK	    equ     07h
;
;
;   FOR SYSTEM BOARD SETUP:
;
;	1) Port hex 0094 bit 7 must be 0 for setup of system board functions
;	   other than VGA.
;
;	2) Port hex 0094 bit 5 must be 1 to avoid driving a setup to the VGA
;
;	3) Port hex 0096 bit 3 must be 0 to avoid driving a setup to an adapter
;
;   FOR ADAPTER SETUP:
;
;	1) Port hex 0094 bit 7 must be 1 to avoid a setup to system board
;	   functions.
;
;	2) Port hex 0094 bit 5 must be 1 to avoid a setup to the VGA.
;
;	3) Port hex 0096 bit 3 must be 1 for adapter setup with I/O addresses
;	   100h - 107h.
;
;
;   NOTE:
;
;   In our case the only system board setup we shall need to do is disable
;   the system 1M of system board memory. This is done by a command to bit
;   0 of port 103h. To disable memory this should be set to 0 and to enable
;   memory it should be set to 1.
;
	SYS_MEM_CONT	equ	103h
	SYS_MEM_BIT	equ	01B
	SYS_MEM_DSB	equ	NOT 01B
;
; The PMeM:
;
;   Has 2MB of additional dyanamic random access memory. Four banks
;   of special sockets hold 256K X 9 memory SIPs.
;
;   The PMeM features Enhanced Dynamic Memory Relocation which provides
;   the user with the ability to map any 16K physical segment of PMeM
;   memory into any 16K segment of the 16M system memory space.
;						 -------------
;		 (Translation RAM)    -----------|	     |---- Select one
;		  ---------------    |		 | DEMUX     | ... of 4 banks
;  A23 -----------|A9	      D7|-----	   BS0-2 |	     |----
;	    10	  |	TRAM	|	 --------|------------
;  A14 -----------|A0  (1X8K) D0|-----	 |
;		  ---------------    |	 --------} The upper five address bits
;				     |	   A14-18} select one of the 32 16K
;				     ------------} pages. The lower 14 bits
;  A13-------------------------------------------} select offset within this
;					   A0-13 } segment
;  A0 -------------------------------------------}
;
;   The PMeM is on an adapter board accessible through the programmmable select
;   option registers (100h - 107h).  The board can be setup only when the
;   CD/SETUP line to the connector, on which this board is installed, is
;   active.  This can be activated by selecting adapter setup and indicating
;   which connector we want to examine / change.  The ports to do these are
;   ports 94h and 96h.
;
;
;
; Port addresses for PMeM memory
;
    ;
    ; 1. Card ID ADDR
    ;
    ID_ADDR	EQU	100H
    ID_ADDR_LO  EQU	ID_ADDR
    ID_ADDR_HI	EQU	ID_ADDR+1
	    ;
	    ; ID FOR PMeM CARD
	    ;
	    MEM_ID_LO	EQU 0FEH
	    MEM_ID_HI	EQU 0FEH
	    ;
    ;
    ; The ID is guaranteed to be unique.
    ;
    ;
    ;
    ; 2. CONFIG PORT
    ;
    ;	---------------------------------
    ;	| 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 | 102H
    ;	---------------------------------
    ;	  |   |   |   |   |   |   |   |----------Sleep Bit 0 - sleeping
    ;	  |   |   |   |   |   |   |			   1 - awake	(R/W)
    ;	  |   |   |   |   |   |   |
    ;	  |   |   |   |   |   |   ---------------Bank 0 Presc. Det2 (R)
    ;	  |   |   |   |   |   |
    ;	  |   |   |   |   |   -------------------Bank 1 Presc. Det1 (R)
    ;	  |   |   |   |   -----------------------Bank 1 Presc. Det2 (R)
    ;	  |   |   |   |
    ;	  |   |   |   ---------------------------Bank 2 Presc. Det1 (R)
    ;	  |   |   -------------------------------Bank 2 Presc. Det2 (R)
    ;	  |   |
    ;	  |   -----------------------------------Bank 3 Presc. Det1 (R)
    ;	  ---------------------------------------Bank 3 Presc. Det2 (R)
    ;
    ;	The procedure to check the prescence of a bank is to check for the
    ;	logical or of both the bits set aside for the particular bank. If
    ;	this value is one then we conclude that the bank is present.  Also
    ;	during configuration or reconfiguration we should either exclude
    ;	interrupts or put the board to sleep.  After this process is over
    ;	we can awaken the board.
    ;
    CONF_ADDR	EQU	ID_ADDR + 2
    ;
	    ;
	    ; STATUS BIT EQUATES
	    ;
	    AWAKE   EQU     1H
	    SLEEP   EQU     NOT AWAKE
	    ;
	    BANK0_PR	EQU 00000010B
	    BANK1_PR	EQU 00001100B
	    BANK2_PR	EQU 00110000B
	    BANK3_PR	EQU 11000000B
    ;
    ;
    ; 3. DATA PORT
    ;
    TRAM_DATA	EQU	ID_ADDR + 3
    ;
    ;
    ;	The TRAM Decoder output is used as the following:
    ;
    ;	---------------------------------
    ;	| 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
    ;	---------------------------------
    ;	  |   |   |   | 	      |
    ;	  |   -----   -----------------
    ;	  |	|	     |
    ;	  |	|	     --------------------------- Page Select (1 of 32)
    ;	  |	|
    ;	  |	---------------------------------------- Bank Select (1 0f 4)
    ;	  |
    ;	  ---------------------------------------------- Enble Bit,0 - PMeM not
    ;							 accessed,1 - PMeM accessed
    ;
    ;
    ;	Using this will be described in the discussion for the next two ports
    ;
	;
	; Equates for enable bit
	;
	TRAM_ENB    EQU 80H
	TRAM_DSB    EQU NOT TRAM_ENB
	;
    ;
    ;
    ; 4. TRAM ADDR PORTS
    ;
    ;	The TRAM is RAM with 10 address pin input.  To initialise the TRAM
    ;	we have to first specify the address of the TRAM location we are
    ;	going to initialise or change.	This is done by outputting the low
    ;	byte of the address to port 106H and then the high byte to address
    ;	107H.  The data at this address can then be output to the TRAM_DATA
    ;	port at 103H according to the values decided by the mapping desired
    ;	and the bit assignment of the TRAM decoder as given in the diagram
    ;	above.
    ;
    TRAM_ADDR	    EQU ID_ADDR + 6
    TRAM_ADDR_LO    EQU TRAM_ADDR
    TRAM_ADDR_HI    EQU TRAM_ADDR + 1
    ;
