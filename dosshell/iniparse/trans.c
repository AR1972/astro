;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <stdio.h>
#include <fcntl.h>
#include <io.h>
#include <dos.h>
#include <malloc.h>

#include "transtxt.h"

extern void _CDECL exit(int);
extern	int Read_Ini_File(char *inifilename);
extern  int Write_Ini_File(char *inifilename);


#define HELP_OFFSET	   (0x139)
//#define HELP_LEN	     (0x318-HELP_OFFSET)
#define HELP_LEN	   (30*16-1)
#define JUNK1_OFFSET	   (HELP_OFFSET+HELP_LEN)
#define JUNK1_LEN	   (7*16+20)
#define TITLE_OFFSET	   (HELP_OFFSET+HELP_LEN)
#define TITLE_LEN	   (40)
#define JUNK2_OFFSET	   (TITLE_OFFSET+TITLE_LEN)
#define JUNK2_LEN	   (15)
#define COMMAND_OFFSET	   (JUNK2_OFFSET+JUNK2_LEN)
#define COMMAND_LEN	   (31*16+6)

void fatal(char *message)
{
	puts(message);
	putchar(7);
	exit(-1);
}

char buffer[10];
/*
**				Main program
*/
void cdecl main(void)
{
	int fhandle;
	unsigned int size;
	unsigned int rsize;
	char far *buffer;
	unsigned int offset;
	int i;

	Read_Ini_File("shell.ini");

	/*
	 * open the file
	 */
	if ((_dos_open("test.meu",O_RDONLY,&fhandle)) != 0)
		fatal(szCantOpen);
	size = (int) lseek(fhandle,0L,SEEK_END);
	if(!size)
	{
		_dos_close(fhandle);
		fatal(szZeroLength);
	}
	lseek(fhandle,0L,SEEK_SET);
	/*
	 * allocate memory for the file contents we are about to read in.
	 */
	if (buffer = (char far *) _fmalloc(size))
	{
		/* read in the file */
		if ( (_dos_read(fhandle, buffer, (int) size, &rsize) == 0) && 
															(rsize == size) )
		{
			offset = HELP_OFFSET;
			i = 0;
			while(offset < size)
			{
				if(buffer[offset]==0)
					buffer[offset] = ' ';
				printf("HELP (%x) = %Fs\n",offset,&buffer[offset]);
				if(++i == 1)
					offset+=JUNK1_LEN;

				offset+=TITLE_OFFSET-HELP_OFFSET;
				if(buffer[offset]==0)
					buffer[offset] = ' ';
				printf("TITLE (%x) = %Fs\n",offset,&buffer[offset]);
				offset+=COMMAND_OFFSET-TITLE_OFFSET;
				if(buffer[offset]==0)
					buffer[offset] = ' ';
				printf("COMMAND (%x) = %Fs\n",offset,&buffer[offset]);
				offset+=COMMAND_LEN;
			}

		}
		else
		{
			_dos_close(fhandle);
			fatal(szReadError);
		}
		_dos_close(fhandle);
	}
	else
	{
		_dos_close(fhandle);
		fatal(szOutOfMemory);
	}

	Write_Ini_File("shell.out");
} /* main */
