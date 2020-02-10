;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <text.h>
#include <menus.h>
#include <prot.h>
#include <groups.h>
#include <icons.h>

WORD GetIthGroupOrProgramIndex(TOKEN group, int isz);
int Num_Items_In_Group(TOKEN token);
VOID DeleteTask(void);
BOOL FDelItemDialog(char *theMessage);

extern struct ListBoxData ProgramList;
extern TOKEN gGroupLevel;
extern BOOL gTaskListEnabled;
extern ListBoxData TaskList;

static VOID NEAR PASCAL DoDelete(void)
{
    int element;
    TOKEN group;
    BOOL doorpresent;

    if(gGroupLevel == TK_PROGRAMSTARTER) {
        group = Get_KeyWord_Assignment(gGroupLevel, TK_GROUP);
        doorpresent = FALSE;
    } else {
        group = gGroupLevel;
        doorpresent = TRUE;
    }

    element = Get_List_Focus(&ProgramList) - (int)doorpresent;
    if(element >= 0) {
        element = GetIthGroupOrProgramIndex(group, element);
        Delete_Ith_Element(group, element);
    }
    InsertListItem(&ProgramList, 0);
    InitIconCache();
}

VOID FAR DeleteProgram(void)
{
    TOKEN token, token2;
    BOOL doorpresent;
    int i;
    char tstr[256];

    if(gTaskListEnabled && TaskList.hasglobalfocus) {
        DeleteTask();
    } else { /* Confirm if user wants to delete the item */
        if(gGroupLevel == TK_PROGRAMSTARTER) {
            token = Get_KeyWord_Assignment(gGroupLevel, TK_GROUP);
            doorpresent = FALSE;
        } else {
            token = gGroupLevel;
            doorpresent = TRUE;
        }
        if(token<0 || (i=Get_List_Focus(&ProgramList)-doorpresent)<0 ||
 				(Num_Items_In_Group(token) <= 0 ) ) {
            Shell_Beep();
            return;
        }

        token2 = Get_Ith_Element(token, GetIthGroupOrProgramIndex(token, i));
        token = Get_Symbol_Value(Token_To_Symbol(token2));
        if(Get_Symbol_Type(Token_To_Symbol(token2))==TK_GROUP &&
                Num_Items_In_Group(token)) {
            ShellMessageBox(szDelGroupTitle, szDelGroupLine1);
            return;
        }

        if((token = Get_KeyWord_Assignment(token, TK_PASSWORD)) > 0)
            strfcpy(tstr, Get_Token_Identifier(token));
        else
            tstr[0] = 0;

        if(FDelItemDialog(*tstr ? szPasswordProtected : NullString)) {
            DoDelete();
        }
    }
}

