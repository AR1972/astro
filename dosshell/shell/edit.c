;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <text.h>
#include <edit.h>

editstruct gEdit;

editline far *AllocLine(editstruct far *e)
{
    editline far *t;
    int size;
    /* BUG BUG! */
    size = e->width+6;

    t=LpbAllocWorkFar(size);
    t->next = NULL;
    t->length = 0;
    return(t);
}

VOID FreeLine(editline far * e)
{
    FreeWorkFar(e);
}

BOOL Init_EditStruct(editstruct *e,int width)
{
    int i;

    e->numlines = 1;
    e->curline = 0;
    e->curoffset = 0;
    e->width = width;
    e->head = AllocLine(e);

    if(!e->head)
	return(FALSE);
    return(TRUE);
}


VOID InitEdit()
{
    Init_EditStruct(&gEdit,30);
}

long Get_Num_Lines(editstruct *e)
{
    return(e->numlines);
}

editline far *get_nth_line(editstruct *e,int n)
{
    int i;
    editline far *t;
    t=e->head;
    if(t==NULL)
	return(NULL);
    for(i=0;i<n;i++)
    {
	t=t->next;
	if(t==NULL)
	    break;
    }
    return(t);
}

BOOL Get_Ith_Line(editstruct *e,char *dest,int ith,int len)
{
    int i;
    editline far *t;
    int clen;
    t = get_nth_line(e,ith);
    i = 0;
    if(t!=NULL)
    {
	clen = min(t->length,len);
	for(i=0;i<clen;i++)
	{
	    dest[i]=t->text[i];
	}
    }
    for(;i<len;i++)
	dest[i] = ' ';
    return(TRUE);
}

BOOL Ripple(editstruct *e)
{
    int i;
    editline far *t;
    int savecurline;
    int savecuroffset;
    int start;
    char wrapchar;

    t = get_nth_line(e,e->curline);
    if(t==NULL)
	return(FALSE);
    start = e->curoffset;
    wrapchar = t->text[e->width-4];
    for(i=t->length;i>start;i--)
    {
	  t->text[i] = t->text[i-1];
    }
    ++t->length;
    if(t->length >= e->width-3)
    {
	savecurline=e->curline;
	savecuroffset=e->curoffset;
	++e->curline;
	e->curoffset=0;
	InsertChar(e,wrapchar);
	e->curline = savecurline;
	e->curoffset=savecuroffset;
	--t->length;
    }
    return(TRUE);
}
VOID Oversert(editstruct *e,char c)
{
    editline far *t;
    t = get_nth_line(e,e->curline);
    t->text[e->curoffset] = c;
    ++e->curoffset;

}
BOOL DeleteChar(editstruct *e)
{
    int i;
    editline far *t;
    int start;

    if(e->curoffset <= 0)
    {
	Beep();
	return(FALSE);
    }
    t = get_nth_line(e,e->curline);
    if(t==NULL)
	return(FALSE);
    --e->curoffset;
    start = e->curoffset;

    for(i=start;i<t->length;i++)
    {
	  t->text[i] = t->text[i+1];
    }
    --t->length;
    return(TRUE);
}

BOOL InsertChar(editstruct *e,char c)
{
    if(e->curoffset >= e->width-4)
    {
	InsertLine(e);
    }
    if(Ripple(e))
    {
	Oversert(e,c);
    }else
    {
	Beep();
	return(FALSE);
    }
}

BOOL InsertLine(editstruct *e)
{
    int i;
    editline far *t;
    editline far *link;
    t = get_nth_line(e,e->curline);
    if(t==NULL)
	return(FALSE);
    link = t->next;
    t->next = AllocLine(e);
    if(t->next ==NULL)
	return(FALSE);
    t->next->next = link;
    ++e->numlines;
    ++e->curline;
    e->curoffset = 0;
    return(TRUE);
}

BOOL BackLine(editstruct *e)
{
    editline far *t;
    if(e->curline > 0)
    {
	--e->curline;
	t = get_nth_line(e,e->curline);

	if(e->curoffset > t->length)
	    e->curoffset = t->length;
	return(TRUE);
    } else
    {
	Beep();
	return(FALSE);
    }
}
BOOL ForwardLine(editstruct *e)
{
    editline far *t;
    if(e->curline < e->numlines)
    {
	++e->curline;
	t = get_nth_line(e,e->curline);
	if(e->curoffset > t->length)
	    e->curoffset = t->length;

	return(TRUE);
    } else
    {
	Beep();
	return(FALSE);
    }
}
BOOL BackArrow(editstruct *e)
{
    if(e->curoffset > 0 )
    {
	--e->curoffset;
	return(TRUE);
    } else
    {
	Beep();
	return(FALSE);
    }

}
BOOL ForwardArrow(editstruct *e)
{
    editline far *t;
    t = get_nth_line(e,e->curline);
    if(t==NULL)
    {
	Beep();
	return(FALSE);
    }
    if(e->curoffset < t->length)
    {
	++e->curoffset;
	return(TRUE);
    } else
    {
	Beep();
	return(FALSE);
    }
}

BOOL DeleteLine(editstruct *e)
{
    editline far *t;
    editline far *link;
    if(e->curline > 0)
    {
       t = get_nth_line(e,e->curline-1);
       if(t==NULL)
	return(FALSE);
       link=t->next;
       t->next = t->next->next;
       if(t->next == NULL)
       {
	t->next = link;
	t->next->length=0;
       }else
       {
	 FreeLine(link);
	 --e->numlines;
       }
    }
    else
    {
	link = e->head->next;
	if(link == NULL)
	    e->head->length = 0;
	else
	{
	   FreeLine(e->head);
	   --e->numlines;
	   e->head = link;
	}
    }
    e->curoffset = 0;
    return(TRUE);
}
BOOL Home(editstruct *e)
{
    e->curoffset = 0;
    return(TRUE);
}
BOOL End(editstruct *e)
{
    editline far *t;
    t = get_nth_line(e,e->curline);
    if(t==NULL)
	return(FALSE);
    e->curoffset = t->length;
}
