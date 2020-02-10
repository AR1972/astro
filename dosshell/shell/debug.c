;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <bios.h>
#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>
#include <stdio.h>

#undef FILE1
#define COM1 foo

extern WORD ReturnScreenMode;
extern BOOL gfSwapHandlerInstalled;

void com1(str)
char *str;

{
#ifdef COM1
	long l;

	_bios_serialcom(_COM_INIT, 0, _COM_CHR8 | _COM_STOP1 | _COM_NOPARITY |
			_COM_9600);
	while (*str)
	{
		_bios_serialcom(_COM_SEND, 0, (unsigned) (*str));
		if (*str == '\n')
		{
			_bios_serialcom(_COM_SEND, 0, (unsigned) '\r');
			for (l=0; l < 10000; l++)
				;
		}
		str++;
	}
#endif
#ifdef FILE1
	static FILE *fp = NULL;

	if (!fp)
	{
		fp = fopen("data", "w");
	}
	fputs(str, fp);
	flushall();
#endif
}

VOID com1i(i)
int i;

{
	char str[15];
	
	itoa(i, str, 10);
	strcat(str, " ");
	com1(str);
}

void PrintAndExit(char *s, int i)
{
	SetUpExitToDos();
	if (gfSwapHandlerInstalled)
		RemoveSwapHandler() ;

	SetScreenMode(ReturnScreenMode);

  	EndScreen(TRUE);
  	EndCow(TRUE);

	printf("%s = %d\n", s, i) ;
	
	DTS_API_Exit();
	exit(0);
}

VOID FlopMsg(dlm)
WORD dlm;

{
	static WORD lastdlm = 0;
	char *s;
	
	switch (dlm)
	{
		case dlmKey: s = "dlmKey"; break;
		case dlmInit: s = "dlmInit"; break;
		case dlmIdle: s = "dlmIdle"; break;
		case dlmTerm: s = "dlmTerm"; break;
		case dlmClick: s = "dlmClick"; break;
		case dlmChange: s = "dlmChange"; break;
		case dlmDblClk: s = "dlmDblClk"; break;
		case dlmUnclick: s = "dlmUnclick"; break;
		case dlmSetFocus: s = "dlmSetFocus"; break;
		case dlmKillFocus: s = "dlmKillFocus"; break;
		case dlmClientMouse: s = "dlmClientMouse"; break;
		case dlmNonClientMouse: s = "dlmNonClientMouse"; break;
		default: s = "Unknown"; break;
	}
	if (dlm == dlmIdle && lastdlm == dlmIdle)
	{
	} else
	{
		com1(s);
		com1("\n");
	}
	lastdlm = dlm;
}

/****	catfname - appends name of file in record to a string
**	This exists because the strings in a record are in far memory, hence
**	the medium-model strxxx fns won't work.
**
**	ENTRY
**		dest - string to append to
**		rcd  - record to fetch name from
**	EXIT
**		dest - has the filename appended to it.
*/
void catfname(dest, rcd)
char dest[];
PENTRY rcd;
{
	int spos;								// position in src
	int dpos;								// position in dest
	
	spos = 0;
	dpos = 0; 
	while (dest[dpos])
		dpos++;
	
	while (spos < NAMELEN && rcd->name[spos])
	{
		dest[dpos] = rcd->name[spos];
		dpos++;
		spos++;
	}
		
	if (rcd->name[NAMELEN])
	{
		dest[dpos++] = '.';

		spos = NAMELEN;
		while (spos < NAMELEN+EXTLEN && rcd->name[spos])
		{
			dest[dpos] = rcd->name[spos];
			dpos++;
			spos++;
		}
		dest[dpos] = EOS;
	} else
	{
		dest[dpos] = EOS;
	}
	return;
} /* proc catfname */

/****
**
**
**	ENTRY
**
**	EXIT
**
**	WARNING:
**
**	EFFECTS:
**
*/
void DumpBranch(walk, indent)
PENTRY walk;
{
	char str[255];
	int i;
	
	if (!walk)
	{
		com1("   -- none --\n");
		return;
	}
	
	while (walk)
	{
		str[0] = EOS;
		for (i=1; i <= indent; i++)
			strcat(str, "   ");
		
		catfname(str, walk);

		if (walk->LASTDIR)
			strcat(str, " LAST");
		if (walk->nosib)
			strcat(str, " NOSIB");
		if (walk->attribs & _A_SUBDIR)
			strcat(str, " DIR");

		strcat(str, "\t");
		
		if (walk->nosib)
		{
			strcat(str, " parent: ");
			if (walk->sibling)
				catfname(str, walk->sibling);
			else
				strcat(str, "--");
		} else
		{
			strcat(str, " sib: ");
			if (walk->sibling)
				catfname(str, walk->sibling);
			else
				strcat(str, "--");
		}
		
		if (walk->attribs & _A_SUBDIR)
		{
			strcat(str, " child: ");
			if (walk->x.d.child)
				catfname(str, walk->x.d.child);
			else
				strcat(str, "--");
			strcat(str," dnext: ");
			if (walk->x.d.dnext)
				catfname(str, walk->x.d.dnext);
			else
				strcat(str, "--");
			strcat(str, "\n");
			com1(str);

			if (walk->x.d.child)
				DumpBranch(walk->x.d.child, indent+1);
		} else
		{
			strcat(str, " snext: ");
			if (walk->x.f.snext)
				catfname(str, walk->x.f.snext);
			else
				strcat(str, "--");
			strcat(str, "\n");
			com1(str);
		}
		
		if (walk->nosib)
			walk = NULL;
		else
			walk = walk->sibling;
	}
} /* proc DumpBranch */

VOID DumpSel()
{
	DumpBranch(listinfo[glob.FocusBox].tree->head, 0);
}

int  SelCountCheck(PTREE tree)
{
	int temp ;
	unsigned long sizesel ;

	temp = GetTreeSelectedInfo(tree, &sizesel) ;
	if (temp != tree->NumSel)
	{
		printf("*** SelCount error! tree->numsel=%d, count=%d\n",
					tree->NumSel, temp) ;
		exit (0) ;
	}
}

int NumTreeFiles(PTREE tree)
{
	PENTRY fil ;
	int cnt = 0 ;

	for (fil = tree->FirstFile ; fil ; fil = fil->x.f.snext)
		cnt++ ;

	if (cnt != tree->filecount)
	{
		Beep() ; Beep() ; Beep() ;
	}
	return cnt ;
}

void DumpsnextChain(char *title, PTREE tree)
{
	/* ZZZZZ */
	char a[13] ;
	PENTRY parent, node ;

	com1(title) ;
	for (node = tree->FirstFile ; node ; node = node->x.f.snext)
	{
		Internal2Normal(a, node->name) ;
		com1("file name: ") ; com1(a) ;
		if (node->FIRSTDIRFILE)
			com1(" first") ;

		if (node->LASTDIRFILE)
			com1(" last") ;

		parent = FindParent(node) ;
		if (parent)
		{
			Internal2Normal(a, parent->name) ;
			if (parent->DIRSORTED)
				com1(" sorted ") ;
		}
		else
		{
			strcpy(a, "d:\\") ;
			if (!tree->SortRequired)
				com1(" sorted ") ;
		}
		com1("  parent name: ") ; com1(a) ;
		com1("\n") ;
	}
	com1("Leaving Dumpsnextchain()\n") ;
} /* DumpsnextChain */

void Shelldumpsnext(void)
{
	PTREE temptree ;
	BYTE t ;
	int cnt ;

	temptree = FindTree("D:\\", &t) ;
	DumpsnextChain("----------------\n", temptree) ;
	cnt = NumTreeFiles(temptree) ;
	com1("Number of tree files = ") ; com1i(cnt) ; com1("\n") ;
	
} /* Shelldumpsnext */

#if 0
void PrintList(TOKEN tkList)
{
    char tstr[256];
    TOKEN tkTemp;
    SYMBOL_TABLE_ENTRY far *syTemp;

    if(tkList <= 0)
        return;

    syTemp = Token_To_Symbol(tkList);
    while((tkList=Get_Symbol_Next(syTemp)) > 0) {
        fprintf(stdaux, "%Fs = ", Get_Token_Identifier(tkList));

        syTemp = Token_To_Symbol(tkList);
        if((tkTemp=Get_Symbol_Value(syTemp)) == TK_NOTHING) {
            fprintf(stdaux, "No Value");
        } else {
            if(Get_Symbol_Next(Token_To_Symbol(tkTemp)) == TK_NOTHING) {
                fprintf(stdaux, "%Fs", Get_Token_Identifier(tkTemp));
            } else {
                fprintf(stdaux, "List");
            }
        }
        fprintf(stdaux, "\n\r");
    }
}
#endif
