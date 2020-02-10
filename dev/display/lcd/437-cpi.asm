;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
CODE    SEGMENT BYTE PUBLIC 'CODE'
        ASSUME CS:CODE,DS:CODE

IF1
;        %OUT    EGA.CPI creation file
;        %OUT    .
;        %OUT    CP SRC files:
;        %OUT    .
;        %OUT    .       CODE PAGE:  437
ENDIF

EGA437: DW     LEN_437                  ;SIZE OF ENTRY HEADER
        DW     POST_EGA437,0            ;POINTER TO NEXT HEADER
        DW     1                        ;DEVICE TYPE
        DB     "LCD     "               ;DEVICE SUBTYPE ID
        DW     437                      ;CODE PAGE ID
        DW     3 DUP(0)                 ;RESERVED
        DW     OFFSET DATA437,0         ;POINTER TO FONTS
LEN_437 EQU    ($-EGA437)

DATA437:DW     1                        ;CART/NON-CART
        DW     1                        ;# OF FONTS
        DW     LEN_D437                 ;LENGTH OF DATA
D437:                                   ;
        DB     8,8                      ;CHARACTER BOX SIZE
        DB     0,0                      ;ASPECT RATIO (UNUSED)
        DW     256                      ;NUMBER OF CHARACTERS
    Db  000h,000h,000h,000h,000h,000h,000h,000h ;   Hex #0
    Db  03Ch,042h,0A5h,0A5h,081h,0BDh,05Ah,03Ch ;   Hex #1
    Db  03Ch,07Eh,0DBh,0DBh,0FFh,0C3h,066h,03Ch ;   Hex #2
    Db  036h,07Fh,07Fh,07Fh,03Eh,01Ch,008h,000h ;   Hex #3
    Db  008h,01Ch,03Eh,07Fh,03Eh,01Ch,008h,000h ;   Hex #4
    Db  01Ch,03Eh,01Ch,07Fh,07Fh,036h,008h,01Ch ;   Hex #5
    Db  008h,01Ch,03Eh,07Fh,07Fh,036h,008h,01Ch ;   Hex #6
    Db  000h,000h,018h,03Ch,03Ch,018h,000h,000h ;   Hex #7
    Db  0FFh,0FFh,0E7h,0C3h,0C3h,0E7h,0FFh,0FFh ;   Hex #8
    Db  000h,000h,03Ch,066h,066h,03Ch,000h,000h ;   Hex #9
    Db  0FFh,0FFh,0C3h,099h,099h,0C3h,0FFh,0FFh ;   Hex #A
    Db  007h,003h,03Eh,066h,066h,066h,03Ch,000h ;   Hex #B
    Db  03Ch,066h,066h,066h,03Ch,018h,03Ch,018h ;   Hex #C
    Db  008h,00Ch,00Eh,00Ah,00Ah,008h,038h,030h ;   Hex #D
    Db  018h,016h,019h,017h,071h,061h,007h,006h ;   Hex #E
    Db  048h,06Bh,03Eh,0E4h,027h,07Ch,0D6h,012h ;   Hex #F
    Db  040h,070h,07Ch,07Fh,07Ch,070h,040h,000h ;   Hex #10
    Db  001h,007h,01Fh,07Fh,01Fh,007h,001h,000h ;   Hex #11
    Db  018h,03Ch,07Eh,018h,018h,07Eh,03Ch,018h ;   Hex #12
    Db  036h,036h,036h,036h,036h,000h,036h,000h ;   Hex #13
    Db  03Fh,06Ah,06Ah,03Ah,00Ah,00Ah,01Ah,000h ;   Hex #14
    Db  03Ch,076h,038h,06Ch,036h,01Ch,06Eh,03Ch ;   Hex #15
    Db  000h,000h,000h,07Fh,07Fh,07Fh,000h,000h ;   Hex #16
    Db  018h,03Ch,07Eh,018h,07Eh,03Ch,018h,0FFh ;   Hex #17
    Db  018h,03Ch,07Eh,05Ah,018h,018h,018h,000h ;   Hex #18
    Db  018h,018h,018h,05Ah,07Eh,03Ch,018h,000h ;   Hex #19
    Db  000h,00Ch,006h,07Fh,07Fh,006h,00Ch,000h ;   Hex #1A
    Db  000h,018h,030h,07Fh,07Fh,030h,018h,000h ;   Hex #1B
    Db  000h,000h,060h,060h,07Fh,07Fh,000h,000h ;   Hex #1C
    Db  000h,014h,036h,07Fh,07Fh,036h,014h,000h ;   Hex #1D
    Db  008h,008h,01Ch,01Ch,03Eh,03Eh,07Fh,000h ;   Hex #1E
    Db  07Fh,03Eh,03Eh,01Ch,01Ch,008h,008h,000h ;   Hex #1F
    Db  000h,000h,000h,000h,000h,000h,000h,000h ;   Hex #20
    Db  018h,018h,018h,018h,018h,000h,018h,000h ;   Hex #21
    Db  036h,036h,014h,000h,000h,000h,000h,000h ;   Hex #22
    Db  00Ah,00Ah,03Fh,014h,07Eh,028h,028h,000h ;   Hex #23
    Db  008h,03Eh,068h,03Eh,00Bh,07Eh,008h,000h ;   Hex #24
    Db  001h,03Fh,052h,06Ch,01Bh,035h,076h,000h ;   Hex #25
    Db  01Ch,036h,01Ch,03Bh,06Eh,066h,03Bh,000h ;   Hex #26
    Db  018h,018h,030h,000h,000h,000h,000h,000h ;   Hex #27
    Db  006h,00Ch,018h,018h,018h,00Ch,006h,000h ;   Hex #28
    Db  030h,018h,00Ch,00Ch,00Ch,018h,030h,000h ;   Hex #29
    Db  000h,036h,01Ch,07Fh,01Ch,036h,000h,000h ;   Hex #2A
    Db  000h,018h,018h,07Eh,018h,018h,000h,000h ;   Hex #2B
    Db  000h,000h,000h,000h,000h,018h,018h,030h ;   Hex #2C
    Db  000h,000h,000h,07Eh,000h,000h,000h,000h ;   Hex #2D
    Db  000h,000h,000h,000h,000h,018h,018h,000h ;   Hex #2E
    Db  003h,006h,00Ch,018h,030h,060h,040h,000h ;   Hex #2F
    Db  018h,02Ch,066h,066h,066h,034h,018h,000h ;   Hex #30
    Db  018h,018h,038h,018h,018h,018h,03Ch,000h ;   Hex #31
    Db  03Ch,066h,066h,00Ch,018h,032h,07Eh,000h ;   Hex #32
    Db  03Ch,066h,00Ch,01Ch,006h,066h,03Ch,000h ;   Hex #33
    Db  00Ch,01Ch,02Ch,06Ch,07Eh,00Ch,01Eh,000h ;   Hex #34
    Db  07Eh,060h,07Ch,066h,006h,066h,03Ch,000h ;   Hex #35
    Db  01Ch,030h,060h,07Ch,066h,066h,03Ch,000h ;   Hex #36
    Db  07Eh,066h,04Ch,00Ch,018h,018h,038h,000h ;   Hex #37
    Db  03Ch,066h,076h,03Ch,06Eh,066h,03Ch,000h ;   Hex #38
    Db  03Ch,066h,066h,03Eh,006h,00Ch,038h,000h ;   Hex #39
    Db  000h,018h,018h,000h,000h,018h,018h,000h ;   Hex #3A
    Db  000h,018h,018h,000h,000h,018h,018h,030h ;   Hex #3B
    Db  006h,00Ch,018h,030h,018h,00Ch,006h,000h ;   Hex #3C
    Db  000h,000h,07Eh,000h,07Eh,000h,000h,000h ;   Hex #3D
    Db  030h,018h,00Ch,006h,00Ch,018h,030h,000h ;   Hex #3E
    Db  03Ch,066h,026h,00Ch,018h,000h,018h,000h ;   Hex #3F
    Db  03Eh,041h,05Dh,055h,05Fh,040h,03Eh,000h ;   Hex #40
    Db  01Ch,00Ch,01Ch,016h,03Eh,023h,063h,000h ;   Hex #41
    Db  07Ch,036h,036h,03Eh,033h,033h,07Eh,000h ;   Hex #42
    Db  01Dh,033h,061h,060h,060h,031h,01Eh,000h ;   Hex #43
    Db  07Ch,036h,033h,033h,033h,036h,07Ch,000h ;   Hex #44
    Db  07Fh,031h,034h,03Ch,034h,031h,07Fh,000h ;   Hex #45
    Db  07Fh,031h,034h,03Ch,034h,030h,078h,000h ;   Hex #46
    Db  01Dh,033h,061h,060h,067h,033h,01Fh,000h ;   Hex #47
    Db  066h,066h,066h,07Eh,066h,066h,066h,000h ;   Hex #48
    Db  03Ch,018h,018h,018h,018h,018h,03Ch,000h ;   Hex #49
    Db  01Fh,006h,006h,006h,066h,066h,03Ch,000h ;   Hex #4A
    Db  067h,066h,06Ch,078h,06Ch,066h,067h,000h ;   Hex #4B
    Db  078h,030h,030h,030h,031h,033h,07Fh,000h ;   Hex #4C
    Db  041h,063h,077h,07Fh,06Bh,063h,063h,000h ;   Hex #4D
    Db  043h,063h,073h,07Bh,06Fh,067h,063h,000h ;   Hex #4E
    Db  01Ch,036h,063h,063h,063h,036h,01Ch,000h ;   Hex #4F
    Db  07Eh,033h,033h,03Eh,030h,030h,078h,000h ;   Hex #50
    Db  01Ch,036h,063h,063h,06Bh,036h,01Ch,007h ;   Hex #51
    Db  07Ch,066h,066h,07Ch,06Ch,066h,067h,000h ;   Hex #52
    Db  03Eh,066h,070h,03Ch,00Eh,066h,07Ch,000h ;   Hex #53
    Db  07Eh,05Ah,018h,018h,018h,018h,03Ch,000h ;   Hex #54
    Db  063h,063h,063h,063h,063h,063h,03Eh,000h ;   Hex #55
    Db  077h,062h,036h,034h,01Ch,018h,008h,000h ;   Hex #56
    Db  063h,063h,06Bh,06Bh,03Eh,036h,022h,000h ;   Hex #57
    Db  066h,066h,03Ch,018h,03Ch,066h,066h,000h ;   Hex #58
    Db  0E7h,066h,034h,018h,018h,018h,03Ch,000h ;   Hex #59
    Db  07Eh,066h,04Ch,018h,032h,066h,07Eh,000h ;   Hex #5A
    Db  01Eh,018h,018h,018h,018h,018h,01Eh,000h ;   Hex #5B
    Db  060h,030h,018h,00Ch,006h,003h,001h,000h ;   Hex #5C
    Db  03Ch,00Ch,00Ch,00Ch,00Ch,00Ch,03Ch,000h ;   Hex #5D
    Db  008h,01Ch,036h,000h,000h,000h,000h,000h ;   Hex #5E
    Db  000h,000h,000h,000h,000h,000h,000h,0FFh ;   Hex #5F
    Db  030h,018h,00Ch,000h,000h,000h,000h,000h ;   Hex #60
    Db  000h,000h,03Ch,066h,01Eh,066h,07Bh,000h ;   Hex #61
    Db  070h,030h,03Eh,03Bh,033h,03Bh,06Eh,000h ;   Hex #62
    Db  000h,000h,03Eh,066h,060h,066h,03Ch,000h ;   Hex #63
    Db  00Eh,006h,036h,06Eh,066h,066h,03Bh,000h ;   Hex #64
    Db  000h,000h,03Ch,066h,07Eh,060h,03Eh,000h ;   Hex #65
    Db  00Eh,01Bh,018h,03Eh,018h,018h,03Ch,000h ;   Hex #66
    Db  000h,000h,03Dh,066h,038h,03Eh,063h,03Eh ;   Hex #67
    Db  070h,030h,036h,03Bh,033h,033h,073h,000h ;   Hex #68
    Db  018h,000h,038h,018h,018h,018h,03Ch,000h ;   Hex #69
    Db  00Ch,000h,01Ch,00Ch,00Ch,06Ch,06Ch,038h ;   Hex #6A
    Db  070h,030h,033h,036h,03Ch,036h,077h,000h ;   Hex #6B
    Db  038h,018h,018h,018h,018h,018h,03Ch,000h ;   Hex #6C
    Db  000h,000h,076h,07Fh,06Bh,06Bh,06Bh,000h ;   Hex #6D
    Db  000h,000h,076h,03Bh,033h,033h,073h,000h ;   Hex #6E
    Db  000h,000h,03Ch,066h,066h,066h,03Ch,000h ;   Hex #6F
    Db  000h,000h,06Eh,033h,033h,03Eh,030h,078h ;   Hex #70
    Db  000h,000h,03Ah,066h,066h,03Eh,006h,00Fh ;   Hex #71
    Db  000h,000h,06Eh,03Bh,033h,030h,078h,000h ;   Hex #72
    Db  000h,000h,03Eh,070h,03Ch,00Eh,07Ch,000h ;   Hex #73
    Db  008h,018h,03Eh,018h,018h,01Ah,00Ch,000h ;   Hex #74
    Db  000h,000h,066h,066h,066h,066h,03Bh,000h ;   Hex #75
    Db  000h,000h,073h,032h,036h,01Ch,008h,000h ;   Hex #76
    Db  000h,000h,06Bh,06Bh,07Fh,036h,022h,000h ;   Hex #77
    Db  000h,000h,073h,036h,01Ch,036h,067h,000h ;   Hex #78
    Db  000h,000h,077h,033h,01Ah,00Ch,06Ch,038h ;   Hex #79
    Db  000h,000h,07Eh,04Ch,018h,032h,07Eh,000h ;   Hex #7A
    Db  00Eh,018h,018h,070h,018h,018h,00Eh,000h ;   Hex #7B
    Db  018h,018h,018h,018h,018h,018h,018h,000h ;   Hex #7C
    Db  070h,018h,018h,00Eh,018h,018h,070h,000h ;   Hex #7D
    Db  039h,04Eh,000h,000h,000h,000h,000h,000h ;   Hex #7E
    Db  008h,01Ch,01Ch,036h,026h,063h,07Fh,07Fh ;   Hex #7F
    Db  01Dh,033h,061h,060h,060h,031h,00Eh,01Ch ;   Hex #80
    Db  06Ch,000h,066h,066h,066h,066h,03Bh,000h ;   Hex #81
    Db  00Eh,018h,03Ch,066h,07Eh,060h,03Eh,000h ;   Hex #82
    Db  018h,03Ch,03Ch,066h,01Eh,066h,07Bh,000h ;   Hex #83
    Db  036h,000h,03Ch,066h,01Eh,066h,07Bh,000h ;   Hex #84
    Db  030h,018h,03Ch,066h,01Eh,066h,07Bh,000h ;   Hex #85
    Db  018h,018h,03Ch,066h,01Eh,066h,07Bh,000h ;   Hex #86
    Db  000h,000h,03Eh,066h,060h,036h,01Ch,038h ;   Hex #87
    Db  018h,03Ch,03Ch,066h,07Eh,060h,03Eh,000h ;   Hex #88
    Db  036h,000h,03Ch,066h,07Eh,060h,03Eh,000h ;   Hex #89
    Db  030h,018h,03Ch,066h,07Eh,060h,03Eh,000h ;   Hex #8A
    Db  06Ch,000h,038h,018h,018h,018h,03Ch,000h ;   Hex #8B
    Db  018h,03Ch,000h,038h,018h,018h,03Ch,000h ;   Hex #8C
    Db  030h,018h,000h,038h,018h,018h,03Ch,000h ;   Hex #8D
    Db  036h,008h,01Ch,016h,036h,03Fh,063h,000h ;   Hex #8E
    Db  01Ch,014h,01Ch,01Eh,036h,03Fh,063h,000h ;   Hex #8F
    Db  007h,00Ch,07Fh,031h,03Ch,031h,07Fh,000h ;   Hex #90
    Db  000h,000h,076h,01Bh,03Fh,06Ch,077h,000h ;   Hex #91
    Db  03Fh,03Dh,02Ch,03Eh,06Ch,06Dh,06Fh,000h ;   Hex #92
    Db  018h,03Ch,03Ch,066h,066h,066h,03Ch,000h ;   Hex #93
    Db  066h,000h,03Ch,066h,066h,066h,03Ch,000h ;   Hex #94
    Db  070h,018h,03Ch,066h,066h,066h,03Ch,000h ;   Hex #95
    Db  018h,03Ch,042h,066h,066h,066h,03Bh,000h ;   Hex #96
    Db  070h,018h,066h,066h,066h,066h,03Bh,000h ;   Hex #97
    Db  036h,000h,077h,033h,01Ah,00Ch,06Ch,038h ;   Hex #98
    Db  063h,01Ch,036h,063h,063h,036h,01Ch,000h ;   Hex #99
    Db  036h,041h,063h,063h,063h,063h,03Eh,000h ;   Hex #9A
    Db  006h,004h,03Ch,06Eh,068h,06Ah,03Ch,030h ;   Hex #9B
    Db  01Eh,033h,033h,07Ch,039h,05Bh,076h,000h ;   Hex #9C
    Db  066h,066h,03Ch,07Eh,018h,07Eh,018h,000h ;   Hex #9D
    Db  078h,06Ch,06Ch,07Ah,066h,06Fh,066h,003h ;   Hex #9E
    Db  00Eh,01Bh,018h,03Eh,018h,018h,058h,070h ;   Hex #9F
    Db  00Eh,018h,03Ch,066h,01Eh,066h,07Bh,000h ;   Hex #A0
    Db  01Ch,030h,000h,038h,018h,018h,03Ch,000h ;   Hex #A1
    Db  00Eh,018h,03Ch,066h,066h,066h,03Ch,000h ;   Hex #A2
    Db  00Eh,018h,066h,066h,066h,066h,03Bh,000h ;   Hex #A3
    Db  01Ah,02Ch,076h,03Bh,033h,033h,073h,000h ;   Hex #A4
    Db  01Ah,02Ch,073h,07Bh,06Fh,067h,063h,000h ;   Hex #A5
    Db  01Eh,036h,036h,01Fh,000h,03Fh,000h,000h ;   Hex #A6
    Db  01Ch,036h,036h,01Ch,000h,03Eh,000h,000h ;   Hex #A7
    Db  018h,000h,018h,030h,064h,066h,03Ch,000h ;   Hex #A8
    Db  000h,000h,000h,07Eh,060h,060h,000h,000h ;   Hex #A9
    Db  000h,000h,000h,07Eh,006h,006h,000h,000h ;   Hex #AA
    Db  060h,066h,06Ch,07Eh,03Bh,066h,04Ch,00Fh ;   Hex #AB
    Db  060h,066h,06Ch,07Bh,037h,06Bh,04Fh,003h ;   Hex #AC
    Db  018h,000h,018h,018h,018h,018h,018h,000h ;   Hex #AD
    Db  000h,01Bh,036h,06Ch,06Ch,036h,01Bh,000h ;   Hex #AE
    Db  000h,06Ch,036h,01Bh,01Bh,036h,06Ch,000h ;   Hex #AF
    Db  011h,044h,011h,044h,011h,044h,011h,044h ;   Hex #B0
    Db  055h,0AAh,055h,0AAh,055h,0AAh,055h,0AAh ;   Hex #B1
    Db  0EEh,0BBh,0EEh,0BBh,0EEh,0BBh,0EEh,0BBh ;   Hex #B2
    Db  018h,018h,018h,018h,018h,018h,018h,018h ;   Hex #B3
    Db  018h,018h,018h,0F8h,0F8h,018h,018h,018h ;   Hex #B4
    Db  018h,018h,0F8h,0F8h,018h,0F8h,018h,018h ;   Hex #B5
    Db  034h,034h,034h,0F4h,0F4h,034h,034h,034h ;   Hex #B6
    Db  000h,000h,000h,0F8h,0FCh,034h,034h,034h ;   Hex #B7
    Db  000h,000h,0F0h,0F8h,018h,0F8h,018h,018h ;   Hex #B8
    Db  034h,034h,0F4h,0F4h,004h,0F4h,034h,034h ;   Hex #B9
    Db  034h,034h,034h,034h,034h,034h,034h,034h ;   Hex #BA
    Db  000h,000h,0F8h,0FCh,004h,0F4h,034h,034h ;   Hex #BB
    Db  034h,034h,0F4h,0F4h,004h,0FCh,000h,000h ;   Hex #BC
    Db  034h,034h,034h,0FCh,0FCh,000h,000h,000h ;   Hex #BD
    Db  018h,018h,0F8h,0F8h,018h,0F8h,000h,000h ;   Hex #BE
    Db  000h,000h,000h,0F8h,0F8h,018h,018h,018h ;   Hex #BF
    Db  018h,018h,018h,01Fh,01Fh,000h,000h,000h ;   Hex #C0
    Db  018h,018h,018h,0FFh,0FFh,000h,000h,000h ;   Hex #C1
    Db  000h,000h,000h,0FFh,0FFh,018h,018h,018h ;   Hex #C2
    Db  018h,018h,018h,01Fh,01Fh,018h,018h,018h ;   Hex #C3
    Db  000h,000h,000h,0FFh,0FFh,000h,000h,000h ;   Hex #C4
    Db  018h,018h,018h,0FFh,0FFh,018h,018h,018h ;   Hex #C5
    Db  018h,018h,01Fh,01Fh,018h,01Fh,018h,018h ;   Hex #C6
    Db  034h,034h,034h,037h,037h,034h,034h,034h ;   Hex #C7
    Db  034h,034h,037h,037h,030h,01Fh,000h,000h ;   Hex #C8
    Db  000h,000h,03Fh,03Fh,030h,037h,034h,034h ;   Hex #C9
    Db  034h,034h,0F7h,0F7h,000h,0FFh,000h,000h ;   Hex #CA
    Db  000h,000h,0FFh,0FFh,000h,0F7h,034h,034h ;   Hex #CB
    Db  034h,034h,037h,037h,030h,037h,034h,034h ;   Hex #CC
    Db  000h,000h,0FFh,0FFh,000h,0FFh,000h,000h ;   Hex #CD
    Db  034h,034h,0F7h,0F7h,000h,0F7h,034h,034h ;   Hex #CE
    Db  018h,018h,0FFh,0FFh,000h,0FFh,000h,000h ;   Hex #CF
    Db  034h,034h,034h,0FFh,0FFh,000h,000h,000h ;   Hex #D0
    Db  000h,000h,0FFh,0FFh,000h,0FFh,018h,018h ;   Hex #D1
    Db  000h,000h,000h,0FFh,0FFh,034h,034h,034h ;   Hex #D2
    Db  034h,034h,034h,03Fh,01Fh,000h,000h,000h ;   Hex #D3
    Db  018h,018h,01Fh,01Fh,018h,00Fh,000h,000h ;   Hex #D4
    Db  000h,000h,01Fh,01Fh,018h,01Fh,018h,018h ;   Hex #D5
    Db  000h,000h,000h,03Fh,03Fh,034h,034h,034h ;   Hex #D6
    Db  034h,034h,034h,0FFh,0FFh,034h,034h,034h ;   Hex #D7
    Db  018h,018h,0FFh,0FFh,018h,0FFh,018h,018h ;   Hex #D8
    Db  018h,018h,018h,0F8h,0F8h,000h,000h,000h ;   Hex #D9
    Db  000h,000h,000h,01Fh,01Fh,018h,018h,018h ;   Hex #DA
    Db  0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh,0FFh ;   Hex #DB
    Db  000h,000h,000h,000h,0FFh,0FFh,0FFh,0FFh ;   Hex #DC
    Db  0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,0F0h,0F0h ;   Hex #DD
    Db  00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,00Fh,00Fh ;   Hex #DE
    Db  0FFh,0FFh,0FFh,0FFh,000h,000h,000h,000h ;   Hex #DF
    Db  000h,000h,03Dh,06Eh,066h,06Eh,03Bh,000h ;   Hex #E0
    Db  03Ch,066h,06Ch,066h,063h,07Bh,06Eh,000h ;   Hex #E1
    Db  07Fh,033h,031h,030h,030h,030h,078h,000h ;   Hex #E2
    Db  000h,03Fh,07Eh,054h,014h,036h,066h,000h ;   Hex #E3
    Db  07Fh,033h,018h,00Ch,018h,033h,07Fh,000h ;   Hex #E4
    Db  000h,000h,03Fh,06Ch,06Ch,06Ch,038h,000h ;   Hex #E5
    Db  000h,000h,033h,033h,033h,037h,06Dh,060h ;   Hex #E6
    Db  000h,03Fh,07Eh,050h,018h,01Ch,00Ch,000h ;   Hex #E7
    Db  01Ch,008h,03Eh,06Bh,03Eh,008h,01Ch,000h ;   Hex #E8
    Db  01Ch,036h,063h,07Fh,063h,036h,01Ch,000h ;   Hex #E9
    Db  01Ch,036h,063h,063h,036h,055h,077h,000h ;   Hex #EA
    Db  01Ch,030h,018h,02Ch,066h,066h,03Ch,000h ;   Hex #EB
    Db  000h,036h,07Fh,04Dh,059h,07Fh,036h,000h ;   Hex #EC
    Db  001h,003h,03Eh,067h,06Bh,073h,03Eh,040h ;   Hex #ED
    Db  000h,000h,01Eh,030h,03Eh,030h,01Eh,000h ;   Hex #EE
    Db  03Ch,066h,066h,066h,066h,066h,066h,000h ;   Hex #EF
    Db  000h,07Eh,000h,07Eh,000h,07Eh,000h,000h ;   Hex #F0
    Db  018h,018h,07Eh,018h,018h,000h,07Eh,000h ;   Hex #F1
    Db  060h,038h,00Eh,038h,060h,000h,07Eh,000h ;   Hex #F2
    Db  006h,01Ch,070h,01Ch,006h,000h,07Eh,000h ;   Hex #F3
    Db  00Eh,01Bh,01Ah,018h,018h,018h,018h,018h ;   Hex #F4
    Db  018h,018h,018h,018h,058h,0D8h,070h,000h ;   Hex #F5
    Db  018h,018h,000h,07Eh,000h,018h,018h,000h ;   Hex #F6
    Db  000h,03Bh,06Eh,000h,03Bh,06Eh,000h,000h ;   Hex #F7
    Db  01Ch,036h,036h,01Ch,000h,000h,000h,000h ;   Hex #F8
    Db  000h,000h,018h,03Ch,018h,000h,000h,000h ;   Hex #F9
    Db  000h,000h,000h,018h,000h,000h,000h,000h ;   Hex #FA
    Db  003h,002h,006h,024h,06Ch,038h,018h,010h ;   Hex #FB
    Db  076h,03Bh,033h,033h,033h,000h,000h,000h ;   Hex #FC
    Db  03Ch,066h,00Ch,038h,07Eh,000h,000h,000h ;   Hex #FD
    Db  000h,000h,03Ch,03Ch,03Ch,03Ch,000h,000h ;   Hex #FE
    Db  000h,000h,000h,000h,000h,000h,000h,000h ;   Hex #FF
LEN_D437        EQU ($-D437)
                                        ;
POST_EGA437     EQU     $               ;
                                        ;
CODE    ENDS
        END

