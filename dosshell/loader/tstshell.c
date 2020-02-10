;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>

unsigned char far *realdma_ptr;
char temp[256];
char command[256];
char parameters[256];

extern void fstrncpy(unsigned char far *d,unsigned char far *s,unsigned int len);
extern unsigned char far * far cdecl GET_COMMAND_PTR(void);
extern unsigned char far * far cdecl GET_ARGS_PTR(void);
extern unsigned char far cdecl GET_WAIT_FLAG(void);
void far cdecl C_INIT_PROGRAM_LIST(void);
void far cdecl C_GO_Z_NEXT(void);
void far cdecl C_GO_Z_PREV(void);
void far cdecl C_GO_NEXT(void);
void far cdecl C_ADD_PARAMS(char far *s,int length);
void far cdecl C_ADD_PROGRAM(char far *s);
unsigned far cdecl C_GET_EXITCODE () ;
void far cdecl C_DELETE_PROGRAM(int i);
char far * far cdecl C_GET_GLOBAL_SWITCH_DATA(void);
char far * far cdecl C_GET_ITH_ENTRY_DATA(int i);
char far * far cdecl C_GET_ITH(int i);
char far cdecl C_GET_LIST_LENGTH(void);

void cdecl main()
{
    int i,len;
    int c;
	 unsigned ec ;

	    /* if there has been an error, display the error code and info */
		 ec = C_GET_EXITCODE () ;
		 if (ec)
		 {
		   printf ("\nError = 	%x : ** %Fs ** \n", ec, C_GET_ITH (0)) ;
	      C_DELETE_PROGRAM(0);
		 }


       printf("\nCurrent Task List\n");
       len = C_GET_LIST_LENGTH();
       for(i=0;i<=len;i++)
       {
	    printf("** %Fs **\n",C_GET_ITH(i));
       }
       printf("hit a key: 1=Next Z 2 = Prev 3 = Next 4 = delete top 5 = Top \n, other cont.");
       while(!kbhit())
	  ;
       c= getch();
       if(c == '1')
	   C_GO_Z_NEXT();
       if(c == '2')
	   C_GO_Z_PREV();
       if(c == '3')
	   C_GO_NEXT();
       if(c == '4')
	   C_DELETE_PROGRAM(0);

    realdma_ptr = GET_COMMAND_PTR();
    if ((c>= '1') && (c<='5') && len >= 0) goto RestartApp ;

    printf("RUNNING THE SHELL INTERFACE NOW>>>\n");
    printf("Run...: ");
    gets(command);
    /* exit means leave */
    if ((command[0] == 'e') && (command[1] == 'x'))
    {
	printf("EXITING!\n");
	realdma_ptr[0] = 0xFF;
	return;
    }
    printf("Parameters: ");
    gets(parameters);
    C_ADD_PROGRAM((char far *) command);
    C_ADD_PARAMS((char far *) parameters,strlen(parameters));

RestartApp:

    fstrncpy((char far *)realdma_ptr,(char far *) "winoldap.exe",13);
    realdma_ptr = GET_ARGS_PTR();
    realdma_ptr[6] = ' '; /* be sure it is not equal to VOODOO */
    fstrncpy((char far *)realdma_ptr+2,(char far *)parameters,53);
    realdma_ptr[0] = (char) strlen(parameters)+1;
    realdma_ptr[1] = ' ';
    realdma_ptr[strlen(parameters)+2] = '\r';
    /* exit to launch */
}

/* fstrncpy() -
**
** DESCRIPTION
**	Copies exactly len bytes from far null-terminated source string 
**	to destination string.
*/
void fstrncpy(d, s, len)
unsigned char far *d;
unsigned char far *s;
unsigned int	len;
	{
	while (len--)
		*d++ = *s++;
	}
