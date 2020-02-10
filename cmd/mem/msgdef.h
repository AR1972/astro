;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1988 - 1991
; *                      All Rights Reserved.
; */
/************************************************************************/
/* MSGDEF.H		- This include file defines each message type	*/
/*			  that can occur in MEM.  These defines will	*/
/*			  be used by MEM to build the proper message.	*/
/*									*/
/*	Date	: 10/29/87						*/
/************************************************************************/
#include "version.h"                                                    /*EGH*/

#define NewLineMsg			10
#define Title1Msg			11
#define Title2Msg			12
#define Title3Msg			13
#define Title4Msg			14
#define MainLineMsg			15
#define DriverLineMsg			16
#define DeviceLineMsg			17
#define Int15MemoryMsg			18
#define SpaceOverMsg			19
#define XMSVersionMsg			20
#define EMSVersionMsg			21
#define LoadHighMsg			22
#define HiddenMsg			23
#define InterruptVectorMsg		24
#define ROMCommunicationAreaMsg 	25
#define DOSCommunicationAreaMsg 	26
#if IBMCOPYRIGHT                                                        /*EGH*/
#define IbmbioMsg                       56                              /*EGH*/
#define IbmdosMsg                       57                              /*EGH*/
#else                                                                   /*EGH*/
#define IbmbioMsg                       27
#define IbmdosMsg			28
#endif                                                                  /*EGH*/
#define SystemDataMsg			29
#define SystemProgramMsg		30
#define SystemDeviceDriverMsg		31
#define InstalledDeviceDriverMsg	32
#define SingleDriveMsg			33
#define MultipleDrivesMsg		34
#define ConfigBuffersMsg		35
#define ConfigFilesMsg			36
#define ConfigFcbsMsg			37
#define ConfigStacksMsg 		38
#define ConfigDeviceMsg 		39
#define ConfigIFSMsg			40
#define ConfigLastDriveMsg		41
#define ConfigInstallMsg		45	/* gga */
#define UnownedMsg			42
#define BlankMsg			43
#define HandleMsg			44
#define EXTMemAvlMsg			46	/* ;an001; dms;*/
#define StackMsg			47
#define FreeMsg 			48
#define ProgramMsg			49
#define EnvironMsg			50
#define DataMsg 			51
#define XMSMemAvlMsg			52
#define HMAAvlMsg			53
#define HMANotAvlMsg			54
#define HMADOSMsg			55
#define ROMDOSMsg			58
#define CTtlTitleMsg			59
#define ModUseMsg			60
#define FreeConvMsg			61
#define	CFreeMsg			62
#define CTtlNameMsg			63
#define CTtlUScoreMsg			64
#define FreeTitleMsg			65
#define FreeUScoreMsg			66
#define MainFLineMsg			67
#define CMemFragMsg			68
#define SystemMsg			69
#define FreeSumMsg			70

#define ModNoneMsg			71
#define MainMLineMsg			72
#define ModBarMsg			73
#define ModSumMsg			74
#define ModTitleMsg	75
#define ModUScoreMsg	76
#define MainMXLineMsg	77
#define MainCLineMsg	78
#define MainXLineMsg	79
#define KeyPressMsg	80
#define NoUMBAvailMsg	81

#define Title1AMsg	82
#define Title2AMsg	83
#define ConvMemDet	84
#define UpperMemDet	85

#define MemSumm1Msg	86
#define MemSumm2Msg	87
#define MemLineMsg	88
#define LargeExMsg	89
#define LargeUMBMsg	90
#define ConvMsg		91
#define UpperMsg	92
#define XMSMsg		93
#define EMSMsg		94
#define TotalMsg	95
#define TtlConvMsg	96

#define FreeUpperMsg	97
#define FreeUTitleMsg	98
#define FreeUUScoreMsg	99
#define MainFULineMsg	100

#define MemSumMsg	101
#define SumTitleMsg	102
#define SumUScoreMsg	103
#define SumLineMsg	104

#define AdaptMsg  	105

#define MainMDLineMsg	106  // See 72
#define MainMDXLineMsg	107  // See 77

#define TtlEms		108
#define ModuleName      109

#define Bites           110
#define FreeEms 	111

#define XMSMsgPool	112
#define FreeEMSPool	113
#define PoolMsg1	114
#define PoolMsg2	115

#define	MSG_OPTIONS_FIRST	300
#define	MSG_OPTIONS_LAST	306

#ifdef JAPAN
#define	AdddrvMsg			400
#endif

#define ParseError1Msg			01
#define ParseError10Msg 		10

/************************************************************************/
/*		Message Retriever Standard Equates			*/
/************************************************************************/

#define Ext_Err_Class			0x0001
#define Parse_Err_Class 		0x0002
#define Utility_Msg_Class		0x00ff
#define No_Handle			0xffff
#define No_Replace			0x0000
#define Sublist_Length			0x000b
#define Reserved			0x0000
#define Left_Align			0x0000
#define Right_Align			0x0080
#define Char_Field_Char 		0x0000
#define Char_Field_ASCIIZ		0x0010
#define Unsgn_Bin_Byte			0x0011
#define Unsgn_Bin_Word			0x0021
#define Unsgn_Bin_DWord 		0x0031
#define Sgn_Bin_Byte			0x0012
#define Sgn_Bin_Word			0x0022
#define Sgn_Bin_DWord			0x0032
#define Bin_Hex_Byte			0x0013
#define Bin_Hex_Word			0x0023
#define Bin_Hex_DWord			0x0033
#define No_Input			0x0000
#define STDIN				0x0000
#define STDOUT				0x0001
#define STDERR				0x0002
#define Blank				0x0020

#define CarryFlag			0x0001
