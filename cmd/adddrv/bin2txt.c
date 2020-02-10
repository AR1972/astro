;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
 * Binary file convert text to stdout
 *	(it is insted of hd)
 *
 *	Created by yoshim	May 27, 1987
 */

#include <stdio.h>
#ifdef MSDOS
#include <fcntl.h>
#include <io.h>
#endif

main(argc, argv)
int argc;
char *argv[];
{
	FILE *f;
	int c;
	int i;

	if (argc < 2) {
		printf("usage: bin2txt filename\n");
		exit(1);
	}

	if ((f = fopen(argv[1], "r")) == NULL) {
		printf("bin2txt: cannot open %s\n", argv[1]);
		exit(2);
	}

#ifdef MSDOS
	setmode(fileno(f), O_BINARY);
#endif

	i = 0;
	while((c = getc(f)) != EOF) {
		if (i == 0)
			printf("\t");
		printf("0x%02X,", c);
		if ((i = ++i % 8) == 0)
			printf("\n");
	}
	fclose(f);
}
