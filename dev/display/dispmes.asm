;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
CODE	SEGMENT BYTE PUBLIC 'CODE'      ;
	ASSUME	CS:CODE,DS:CODE 	;
					;
CR	EQU	13			;
LF	EQU	10			;
					;
	PUBLIC	ERROR_1B		;
	PUBLIC	ERROR_1A		;
	PUBLIC	ERROR_2 		;
	PUBLIC	ERROR_3 		;
;;	PUBLIC	MSG_4			;
					;
INCLUDE DISPMES.INC
					;
CODE	ENDS				;
	END				;
