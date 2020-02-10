;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#define tmmSetFocus tmmUserMin
#define tmmDrawItem tmmUserMin+1
#define tmmSelect   tmmUserMin+2
#define tmmActivate tmmUserMin+3
#define tmmToggleSelect tmmUserMin+4
#define tmmDeselect tmmUserMin+5
#define tmmDeselectAll tmmUserMin+6
#define tmmPickUp tmmUserMin+7
#define tmmDrop   tmmUserMin+8
#define tmmExplicit	 tmmUserMin+9
#define tmmImplicit	 tmmUserMin+10
#define tmmGetItemString tmmUserMin+11
#define tmmQuerySelect tmmUserMin+12
#define tmmDeSelect tmmUserMin+13

#define WM_MOUSEIDLE     WM_USER
#define WM_DISPLAYDRIVES WM_USER+1
#define WM_SETVIEWFILE   WM_USER+2

#define TF_ISFOCUS  1
#define TF_ICONONLY 2

#define ISRANGESELECT -1
typedef struct ScrollBarData
{
    ARC rect;
    int value;
    int maxvalue;
    BOOL dragging;
    BOOL inited;
    BOOL doupdate;
    WORD mouseoffset;
}ScrollBarData;

typedef struct ListBoxData
{
    int numlinesonscreen;
    int nextlinetoupdate;
    int numlinesscrolled;
    int focusitem;
    BOOL update;
    PWND pwd;
    ARC rect;
    ScrollBarData scroll;
    BOOL inited;
	WORD (*ListProc) (WORD tmm, char *sz, WORD isz, TMC tmc,
										WORD x, WORD y, WORD bArg) ;
    WORD LastClicked;
    BOOL blank;
    char *title;
    BOOL halted;
    WORD tmc;
    BOOL stupid; /* no scroll bar or boundaries */
    BOOL hasglobalfocus;
    WORD drawahead;
    WORD drawbehind;
    WORD anchor;
    BOOL mode;	    /* implicit = TRUE;explicit = FALSE */
    BOOL alwaysexplicit; /* if cannot go into implicit mode */
	 ISA  color;
	 BYTE lastmouseX;
	 BYTE lastmouseY;
	 BYTE lastmousestate;
}ListBoxData;

#define Get_List_Focus(listbox) ((listbox)->focusitem)
#define Get_List_Window(listbox) ((listbox)->pwd)
#define Get_List_Rect(listbox) ((listbox)->rect)
#define Is_List_Captured(listbox) ((listbox)->scroll.dragging)
#define ReInit_ListBox(listbox) ((listbox)->inited=FALSE)
#define Not_Blank(listbox) ((listbox)->blank=FALSE)
#define Set_Anchor(listbox,value) (listbox)->anchor = (value)
#define Always_Explicit(listbox) {(listbox)->alwaysexplicit = TRUE; (listbox)->mode = FALSE;}
#define Set_Explicit(listbox) ((listbox)->mode = 0)
#define Halt_Listbox(listbox) ((listbox)->halted = 1)
#define UnHalt_Listbox(listbox) ((listbox)->halted = 0)
#define Set_List_Color(listbox,value) ((listbox)->color = (value))
#define Get_List_Scrolled(listbox) ((listbox)->numlinesscrolled)
#define Set_List_Focus(listbox,value) ((listbox)->focusitem = value)
#define Set_List_Scrolled(listbox,value) ((listbox)->numlinesscrolled=value)
