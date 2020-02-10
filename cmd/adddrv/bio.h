;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

typedef struct {
	DWORD	packet, strategy, Interrupt;
} CALLDEV;

typedef struct {
	BYTE len;
	BYTE unitcode;
	BYTE code;
	WORD status;
	BYTE reserve[8];
	BYTE unitcount;
	DWORD break_addr;
	DWORD bpb_array;
	BYTE device_number;
} REQ, far * LPREQ;

#define	REQ_ERROR	0x8000
#define	REQ_BUSY	0x0200
#define	REQ_DONE	0x0100

void	bio(CALLDEV *);
int	checkumb(WORD);
int	check_swapper(void);
