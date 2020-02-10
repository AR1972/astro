;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/* WARNING this is a hardcoded number for the number of bytes in the SFT
 */
#define DOS5SFTSIZE 0x3B

#define ER_NO_MEMORY            1
#define ER_NO_LIST_MEMORY  2
#define ER_LOW_MEMORY      3
#define ER_COMM_NOLOAD     4
#define ER_NO_XMS_DRV      5
#define ER_LOW_XMS_MEM     6
#define ER_WND_SWAP_OUT    8
#define ER_RELOCATE        9
#define ER_GRABBER_LOAD    11   
#define ER_WND_SWAP_IN     12
#define ER_APP_SWAP_OUT    13   
#define ER_APP_SWAP_IN     14   
#define ER_LIST_SWAP            15
#define ER_EXEC_FAILS      16
#define ER_WINOLDAP_ACTIVE 17
#define ER_BAD_INIT_DIR    18
#define ER_ANOTHER_SW_ACTIVE  19
#define ER_TOO_MANY_SWITCHERS 20
#define ER_APP_NOT_STARTED_YET 21       

#define ER_SWAPI_CREATE_FAILS 80

#define MAX_PROGRAM_LENGTH 80
#define MAX_TITLE  30

#define MAX_NUM_PROGRAMS 16


#define F_NO_SWITCH    1
#define F_GRAPHICS     2
#define F_NO_PAUSE     4
#define F_NO_ALT_TAB      256
#define F_NO_ALT_ESC      512
#define F_NO_CTRL_ESC  1024
#define F_FREE                    0x8000

//BUG BUG zero based, but should it be total 16?
#define MAXTASKS 13

#define FSHELL 64

typedef struct Switch_Entry
{
    char Program_Name[MAX_PROGRAM_LENGTH];
    char Program_Title[MAX_TITLE];
    WORD Conv_Req;
    WORD XMS_Req;
	 WORD XMS_Want;
	 BYTE HK_Scan_Code_1;
	 BYTE HK_Scan_Code_2;
	 BYTE HK_Shift_State;
    WORD Program_Flags;
    BYTE Next_In_List;
    WORD Program_Id;
    BYTE Path_Id;
	 WORD Shell_Cookie;
}Switch_Entry;

typedef struct Switch_Info
{
	BYTE Switcher_Id;
	WORD CPU_Type;
	WORD SFT_Size;
	char Parameters[130];
	char Grabber_Name[80];
	char Swap_Path1[68];
	char Swap_Path2[68];
	WORD Min_Path1; 
	WORD Min_Path2;
	WORD XMS_Handle;
	DWORD XMS_Size;
	WORD Int_15_Users_Id;
	WORD Id_Serial;
	WORD Exit_Code;
	BYTE Num_Lines;
	BYTE Global_Flags;
	BYTE Screen_Back;
	BYTE Title_Fore;        
	BYTE Title_Back;
	BYTE Num_Programs;
	BYTE First_In_List;
	Switch_Entry Program_list[MAX_NUM_PROGRAMS];
}Switch_Info;

extern void far cdecl C_INIT_PROGRAM_LIST(void);
extern void far cdecl C_GO_NEXT(void);
extern void far cdecl C_GO_Z_NEXT(void);
extern Switch_Entry far * far cdecl C_GET_ITH_ENTRY_DATA(int i);
extern char far cdecl C_GET_LIST_LENGTH(void);
extern char far * far cdecl C_GET_ITH(int i);

extern Switch_Info far * far cdecl C_GET_GLOBAL_SWITCH_DATA(void);
extern unsigned far cdecl C_GET_EXITCODE (void) ;
extern void far cdecl C_DELETE_PROGRAM(int i);
extern BOOL RunningUnderMStasker(void);


extern void far cdecl C_ADD_PROGRAM(char far *s);
extern void far cdecl C_ADD_PARAMS(char far *s,int length);
extern void far cdecl C_GO_Z_NEXT(void);
