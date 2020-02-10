                TITLE   CRC table
                PAGE    66, 132

COMMENT @
    crctab.asm : Alan Butt : May 1, 1989 : Expansion Box Project

    This module contains the CRC table and related variables that are common
    to both the serial and parallel communications.
@

INCLUDE         model.inc
INCLUDE         fastlynx.inc

                .DATA

                PUBLIC __crctab
                PUBLIC __crc_errors

                EVEN

__crctab  DW 00000H, 01021H, 02042H, 03063H, 04084H, 050a5H, 060c6H, 070e7H
        DW 08108H, 09129H, 0a14aH, 0b16bH, 0c18cH, 0d1adH, 0e1ceH, 0f1efH
        DW 01231H, 00210H, 03273H, 02252H, 052b5H, 04294H, 072f7H, 062d6H
        DW 09339H, 08318H, 0b37bH, 0a35aH, 0d3bdH, 0c39cH, 0f3ffH, 0e3deH
        DW 02462H, 03443H, 00420H, 01401H, 064e6H, 074c7H, 044a4H, 05485H
        DW 0a56aH, 0b54bH, 08528H, 09509H, 0e5eeH, 0f5cfH, 0c5acH, 0d58dH
        DW 03653H, 02672H, 01611H, 00630H, 076d7H, 066f6H, 05695H, 046b4H
        DW 0b75bH, 0a77aH, 09719H, 08738H, 0f7dfH, 0e7feH, 0d79dH, 0c7bcH
        DW 048c4H, 058e5H, 06886H, 078a7H, 00840H, 01861H, 02802H, 03823H
        DW 0c9ccH, 0d9edH, 0e98eH, 0f9afH, 08948H, 09969H, 0a90aH, 0b92bH
        DW 05af5H, 04ad4H, 07ab7H, 06a96H, 01a71H, 00a50H, 03a33H, 02a12H
        DW 0dbfdH, 0cbdcH, 0fbbfH, 0eb9eH, 09b79H, 08b58H, 0bb3bH, 0ab1aH
        DW 06ca6H, 07c87H, 04ce4H, 05cc5H, 02c22H, 03c03H, 00c60H, 01c41H
        DW 0edaeH, 0fd8fH, 0cdecH, 0ddcdH, 0ad2aH, 0bd0bH, 08d68H, 09d49H
        DW 07e97H, 06eb6H, 05ed5H, 04ef4H, 03e13H, 02e32H, 01e51H, 00e70H
        DW 0ff9fH, 0efbeH, 0dfddH, 0cffcH, 0bf1bH, 0af3aH, 09f59H, 08f78H
        DW 09188H, 081a9H, 0b1caH, 0a1ebH, 0d10cH, 0c12dH, 0f14eH, 0e16fH
        DW 01080H, 000a1H, 030c2H, 020e3H, 05004H, 04025H, 07046H, 06067H
        DW 083b9H, 09398H, 0a3fbH, 0b3daH, 0c33dH, 0d31cH, 0e37fH, 0f35eH
        DW 002b1H, 01290H, 022f3H, 032d2H, 04235H, 05214H, 06277H, 07256H
        DW 0b5eaH, 0a5cbH, 095a8H, 08589H, 0f56eH, 0e54fH, 0d52cH, 0c50dH
        DW 034e2H, 024c3H, 014a0H, 00481H, 07466H, 06447H, 05424H, 04405H
        DW 0a7dbH, 0b7faH, 08799H, 097b8H, 0e75fH, 0f77eH, 0c71dH, 0d73cH
        DW 026d3H, 036f2H, 00691H, 016b0H, 06657H, 07676H, 04615H, 05634H
        DW 0d94cH, 0c96dH, 0f90eH, 0e92fH, 099c8H, 089e9H, 0b98aH, 0a9abH
        DW 05844H, 04865H, 07806H, 06827H, 018c0H, 008e1H, 03882H, 028a3H
        DW 0cb7dH, 0db5cH, 0eb3fH, 0fb1eH, 08bf9H, 09bd8H, 0abbbH, 0bb9aH
        DW 04a75H, 05a54H, 06a37H, 07a16H, 00af1H, 01ad0H, 02ab3H, 03a92H
        DW 0fd2eH, 0ed0fH, 0dd6cH, 0cd4dH, 0bdaaH, 0ad8bH, 09de8H, 08dc9H
        DW 07c26H, 06c07H, 05c64H, 04c45H, 03ca2H, 02c83H, 01ce0H, 00cc1H
        DW 0ef1fH, 0ff3eH, 0cf5dH, 0df7cH, 0af9bH, 0bfbaH, 08fd9H, 09ff8H
        DW 06e17H, 07e36H, 04e55H, 05e74H, 02e93H, 03eb2H, 00ed1H, 01ef0H

__crc_errors    dw      0



                END
