;***
; llgrp.inc - 01-May-87 - lowlevel graphics definitions
;***
	.XLIST
;***
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
; Contains the definition of symbols used in lowlevel graphics modules.
; 
;******************************************************************************

; Graphics adapter types (values used in b$Adapter)

MDPA	=	00000001B	;Monochrome Display/Printer Adapter
CGA	=	00000010B	;Color Graphics Adapter
EGA	=	00000100B	;Enhanced Graphics Adapter
VGA	=	00001000B	;Video Graphics Array
MCGA	=	00010000B	;MultiColor Graphics Array
HGC	=	00100000B	;Hercules Graphics Card 		[1]
OGA	=	01000000B	;Olivetti Graphics Adapter		[4]

; Monitor types (values used in b$Monitor)

Monochrome  =  00000001B	;Monochrome monitor
StdColor    =  00000010B	;Standard Color monitor
				; (or Enhanced Color in compatibility mode)
EnhColor    =  00000100B	;Enhanced Color monitor
AnalogMono  =  00001000B	;Analog monitor supporting monochrome modes
AnalogColor =  00010000B	;Analog monitor supporting color modes

; Note: The definition of AnalogMono and AnalogColor is somewhat unusual:
;	IBM's Analog monitors are called Analog Color (which shows 256k colors)
;	and Analog Monochrome (which shows 64 shades of gray).	This is NOT
;	the definition being used here.  That distinction is unnecessary for
;	our purposes because both types support what the bios considers "color"
;	modes and "monochrome" modes.  The definition we use is based on what
;	bios modes are available in the current environment.  This is defined
;	by what secondary video cards (if any) are present in the machine.
;	If a CGA is present, then the VGA driving the Analog monitor is
;	limited by the bios to monochrome modes (7 & F).  This is "AnalogMono".
;	Similarly, if there is an MDPA present, the VGA is limited by the bios
;	to using color modes (all modes except 7 & F).	This is "AnalogColor".
;	If the VGA is the only video card in the system, then both the
;	AnalogMono bit and the AnalogColor bit of b$Monitor will be set,
;	indicating that all bios modes (color and monochrome) are available.

; Constants used b$VGAmodes to test which BIOS modes are supported.

VGAmode0h	= 00000001B	; supports BIOS mode 0h
VGAmode1h	= 00000010B	; supports BIOS mode 1h
VGAmode2h	= 00000100B	; supports BIOS mode 2h
VGAmode3h	= 00001000B	; supports BIOS mode 3h
VGAmode4h	= 00010000B	; supports BIOS mode 4h
VGAmode5h	= 00100000B	; supports BIOS mode 5h
VGAmode6h	= 01000000B	; supports BIOS mode 6h
VGAmode7h	= 10000000B	; supports BIOS mode 7h
VGAmodeDh	= 20h		; supports BIOS mode Dh
VGAmodeEh	= 40h		; supports BIOS mode Eh
VGAmodeFh	= 80h	  	; supports BIOS mode Fh
VGAmode10h	= 00000001B	; supports BIOS mode 10h
VGAmode11h	= 00000010B	; supports BIOS mode 11h
VGAmode12h	= 00000100B	; supports BIOS mode 12h
VGAmode13h	= 00001000B	; supports BIOS mode 13h

HALF		= 1		;[4] Hercules HALF mode value
	.LIST
