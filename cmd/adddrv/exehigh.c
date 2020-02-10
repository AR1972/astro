;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
 * EXE file High-loding program
 *
 *	Created by yoshim	May 27, 1987
 */

#include <stdio.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef MSDOS
#define	OPENMODE	(O_RDWR|O_BINARY)
#else
#define	OPENMODE	(O_RDWR)
#endif

typedef unsigned short WORD;
typedef struct {
	WORD	wSignature;
	WORD	cbLastp;
	WORD	cpnRes;
	WORD	irleMax;
	WORD	cparDirectory;
	WORD	cparMinAlloc;
	WORD	cparMaxAlloc;
	WORD	saStack;
	WORD	raStackInit;
	WORD	wchksum;
	WORD	raStart;
	WORD	saStart;
	WORD	rbrgrle;
	WORD	iovMax;
	WORD	doslev;
} RUNTYPE;

RUNTYPE header;
char copybuf[256];
char init[] = {
#include <init.i>
};

main(argc, argv)
int argc;
char *argv[];
{
	int src, dst;
	long min;
	long module;
	long rest;
	short packbuf;


	printf("Microsoft (R) EXE File High-Loading Utility  Version 0.01\n");
	printf("Copyright (C) Microsoft KK 1987.  All rights reserved.\n\n");

	if (argc < 3) {
		fprintf(stderr, "usage: exehigh <infile> <outfile>\n");
		exit(1);
	}

	if ((src = open(argv[1], OPENMODE)) == -1) {
		fprintf(stderr, "Cannot open %s\n", argv[1]);
		exit(2);
	}

	lseek(src, 0L, 0);
	read(src, (char *)&header, sizeof(RUNTYPE));
	if (header.wSignature != 0x5A4D) {
		close(src);
		fprintf(stderr, "%s is not an .EXE\n", argv[1]);
		exit(3);
	}

	module = (((long)header.cparDirectory)<<4);
	lseek(src, module + (((long)header.saStart)<<4) + header.raStart -2, 0);
	read(src, (char *)&packbuf, sizeof(short));
	if (packbuf == 0x4252) {
		close(src);
		fprintf(stderr, "%s is packed\n", argv[1]);
		exit(4);
	}

	if (header.cparMinAlloc == 0 && header.cparMaxAlloc == 0) {
		close(src);
		fprintf(stderr, "%s is high\n", argv[1]);
		exit(5);
	}

	dst = open(argv[2], O_CREAT|O_TRUNC|OPENMODE, S_IREAD|S_IWRITE);
	if (dst == -1) {
		fprintf(stderr, "Cannot create %s\n", argv[2]);
		close(src);
		exit(6);
	}

	*(WORD *)&init[0] = header.raStackInit;
	*(WORD *)&init[2] = header.saStack;
	*(WORD *)&init[4] = header.raStart;
	*(WORD *)&init[6] = header.saStart;
	*(WORD *)&init[8] = (((unsigned)sizeof(init))>>4);

	exe_copy(src, dst, module, filelength(src) - module);
	exe_rel(dst, (long)header.rbrgrle, header.irleMax, module);
	close(src);

	min = (((long)header.cparMinAlloc)<<4);

	header.raStackInit = 0x80;
	header.saStack = 0;
	header.raStart = 0x80;
	header.saStart = 0;
	rest = min + header.cbLastp + sizeof(init);
	header.cpnRes += rest / 512;
	header.cbLastp = rest % 512;
	header.cparMinAlloc = 0;
	header.cparMaxAlloc = 0;

	/* write new header */
	lseek(dst, 0L, 0);
	write(dst, (char *) &header, sizeof(RUNTYPE));

	/* add dummy page */
	add_rest(dst, min);
	close(dst);
}

add_rest(h, len)
int h;
long len;
{
	int count;

	for (count = 0; count < sizeof(copybuf); count++)
		copybuf[count] = 0;

	lseek(h, 0L, 2);

	while (len > 0) {
		count = (len > sizeof(copybuf))? sizeof(copybuf): len;
		len -= count;
		write(h, copybuf, count);
	}
}

exe_copy(src, dst, header, module)
int src, dst;
long header, module;
{
	lseek(src, 0L, 0);
	lseek(dst, 0L, 0);
	copy(src, dst, header);
	write(dst, init, sizeof(init));
	copy(src, dst, module);
}

copy(src, dst, len)
int src, dst;
long len;
{
	int count;

	while (len > 0) {
		count = (len > sizeof(copybuf))? sizeof(copybuf): len;
		len -= count;
		read(src, copybuf, count);
		write(dst, copybuf, count);
	}
}

exe_rel(h, pos, count, module)
int h;
long pos;
unsigned short count;
long module;
{
	WORD relbuf[2];
	long targ;

	while(count-- != 0) {
		lseek(h, pos, 0);
		read(h, (char *)relbuf, 4);
		relbuf[1] += (((unsigned)sizeof(init))>>4);
		lseek(h, pos, 0);
		write(h, (char *)relbuf, 4);
		pos += 4;

		targ = module + (((unsigned long)relbuf[1])<<4) + relbuf[0];
		lseek(h, targ, 0);
		read(h, (char *)relbuf, 2);
		relbuf[0] += (((unsigned)sizeof(init))>>4);
		lseek(h, targ, 0);
		write(h, (char *)relbuf, 2);
	}
}
