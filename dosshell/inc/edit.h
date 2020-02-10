;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#define MAXLINELENGTH 30

typedef struct editline
{
    struct editline far *next;
    BYTE length;
    char text[MAXLINELENGTH];
}editline;

typedef struct editstruct
{
    int curline;
    int curoffset;
    int numlines;
    int width;
    struct editline far *head;
}editstruct;


extern editstruct gEdit;
extern long Get_Num_Lines(editstruct *e);
extern BOOL InsertChar(editstruct *e,char c);
extern BOOL Get_Ith_Line(editstruct *e,char *dest,int ith,int len);
extern BOOL Init_EditStruct(editstruct *e,int width);
extern VOID InitEdit();
extern BOOL InsertLine(editstruct *e);
extern BOOL BackLine(editstruct *e);
extern BOOL ForwardLine(editstruct *e);
extern BOOL BackArrow(editstruct *e);
extern BOOL ForwardArrow(editstruct *e);
extern BOOL DeleteChar(editstruct *e);
extern BOOL DeleteLine(editstruct *e);
extern BOOL Home(editstruct *e);
extern BOOL End(editstruct *e);
