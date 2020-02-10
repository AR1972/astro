;*
;*	CW : Character Windows
;*
;*	kbd_head.inc : header file for all KBD drivers

;*****************************************************************************

.xlist
	include	drv.inc

	include inkbd.inc				;* installable keyboard
	include	inmou.inc				;* mouse (for MK values)
.list

;*****************************************************************************
;*	* Virtual Key Codes *

	include	vkey.inc

;*****************************************************************************
;*	* To Convert VK to VW, take low byte
VwOfVk	EQU	LOW
VkOfVw	EQU	100H+

;*****************************************************************************
;*	* Fill the gaps in VK_ list
VK_1		=	VK_0+1
VK_2		=	VK_0+2
VK_3		=	VK_0+3
VK_4		=	VK_0+4
VK_5		=	VK_0+5
VK_6		=	VK_0+6
VK_7		=	VK_0+7
VK_8		=	VK_0+8

VK_B		=	VK_A+1
VK_C		=	VK_A+2
VK_D		=	VK_A+3
VK_E		=	VK_A+4
VK_F		=	VK_A+5
VK_G		=	VK_A+6
VK_H		=	VK_A+7
VK_I		=	VK_A+8
VK_J		=	VK_A+9
VK_K		=	VK_A+10
VK_L		=	VK_A+11
VK_M		=	VK_A+12
VK_N		=	VK_A+13
VK_O		=	VK_A+14
VK_P		=	VK_A+15
VK_Q		=	VK_A+16
VK_R		=	VK_A+17
VK_S		=	VK_A+18
VK_T		=	VK_A+19
VK_U		=	VK_A+20
VK_V		=	VK_A+21
VK_W		=	VK_A+22
VK_X		=	VK_A+23
VK_Y		=	VK_A+24

;*****************************************************************************
