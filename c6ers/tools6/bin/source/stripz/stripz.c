//
/* This program is free software. It comes without any warranty, to
* the extent permitted by applicable law. You can redistribute it
* and/or modify it under the terms of the Do What The Fuck You Want
* To Public License, Version 2, as published by Sam Hocevar. See
* http://sam.zoy.org/wtfpl/COPYING for more details. */
//
/*recration of the utility stripz. strips number of bytes from the 
beginning of a file as defined by the first two bytes of the file
needed to complete the build of MSDOS.SYS
*/ 
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <stdlib.h>
#define	MAXLINE	200
#define	MAXWORD	64

//void strip_ext(char *fname)
//{
//   char *end = fname + strlen(fname);
//
//    while (end > fname && *end != '.') {
//       --end;
//    }
//
//    if (end > fname) {
//        *end = '\0';
//    }
//}

int main(int argc, char *argv[])
{
	FILE *fp1;
	FILE *fp2;
	unsigned char *trim[2];
	int size = 0;
	int final_size = 0;
	int position = 0;
	int *buffer = 0;
	int *filename = 0;
	
	if( argc != 1)
	{
		if ( (fp1 = fopen(argv[1], "rb")) == NULL )
		{
			printf("stripz: cannot open %s\n", argv[1]);
			exit(0);
		}
		printf("stripz: processing file %s\n", argv[1]);
		fseek(fp1,0,SEEK_SET);
		fseek(fp1,0, SEEK_END);
		size = (int)ftell(fp1);
		fseek(fp1,0,SEEK_SET);
		fread(&trim,1,2,fp1);
		position = (int)trim[0];
		final_size = size - position;
		printf("stripz: trimming 0x%X bytes from beginning of file\n", position);
		printf("strpiz: file size is 0x%X size after trim is 0x%X\n",size, final_size);
		fseek(fp1,0,SEEK_SET);
		fseek(fp1,position,SEEK_SET);
		buffer = malloc(final_size);
		//filename = malloc(strlen(argv[1]));
		//strip_ext(argv[1]);
		//sprintf(filename,"%s.sys",argv[1]);
		filename = argv[2];
		if (buffer == NULL)
		{
			printf("stripz: out of memory");
			exit(0);
		}
		if (filename == NULL)
		{
			printf("stripz: out of memory");
			exit(0);
		}
		fread(buffer, 1, final_size, fp1);
		fclose(fp1);
		printf("stripz: saving to %s\n",filename);
		if ( (fp2 = fopen(filename, "wb")) == NULL )
		{
			printf("stripz: cannot open %s\n",filename);
			free(buffer);
			//free(filename);
			exit(0);
		}
		fwrite(buffer, 1, final_size,fp2);
		fclose(fp2);
		free(buffer);
		//free(filename);
	}
	else
	{
		printf("stripz: supply a file to process\n");
		exit(0);
	}
	return(0);
}