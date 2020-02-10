;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

int	lpstrlen(LPSTR);
void	lpstrnupr(LPSTR, unsigned short);

int	lpstrncmp(LPSTR, LPSTR, unsigned short);
void	lpstrcpy(LPSTR, LPSTR);

void	lpstrncpy(LPSTR, LPSTR, int);				/* MSKK10 */

void	conv_dbspace(LPSTR, unsigned short);
int	wordlen(LPSTR);
int	IsDBCSLeadByte(unsigned char);
