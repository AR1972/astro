/*
	mkdrv driver_file [ [(-|/)flags] service ]*

	Make driver_file from the given services.

	Valid service file extensions are:
		.kbd
		.csd
		.gsd
		.prd
		.gpd
		.syd
		.scd

	Valid flags are: L[fsrp]	 (default is Lfs).
		f => fixed
		s => standard, dual mode (real+protect)
		r => standard service, real mode only
		p => standard service, protect mode only

	ex. mkdrv dos3.drv -Lfs std3.kbd -Lf std3.prd

	See "Installable Drivers Interface (1.00.01)" and "API Guide
	Installable Drivers (2.20.02)" for more details.

	Revision history:
	88.03.24 mikedon	original
	88-04-22 scottra	add real/protect, general extensions, ...
	88-07-12 johng		add SCD (indtSerialComm)

*/


#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#include <fcntl.h>
#include <memory.h>

#include <cow.h>
#include <indrv.h>


#define	floadDefault	(floadFixed | floadStandard | floadAnyMode)

#define	lcbCodeMax	((long)(65535 - sizeof(INSH)))
#define iindsMax	20
#define cbBuffMax	(8 * 1024)


char		rgb[cbBuffMax];
unsigned	cbBuff = 0;
int		fload;

char	szMagic[5] = szMagicDRV;

typedef struct
	{
	char *	sz;
	WORD	indt;
	} SVI;		/* service info */

SVI rgsvi[] =
	{
	{ ".kbd",	indtKeyboard },
	{ ".csd",	indtCharacterScreen },
	{ ".gsd",	indtGraphicScreen },
	{ ".prd",	indtCharacterPrinter },
	{ ".gpd",	indtGraphicPrinter },
	{ ".syd",	indtSystem },
	{ ".scd",	indtSerialComm },
	{ NULL,		indtNil }
	};

void		main(int, char *([]));
char		IndtFromSzFile(char *);
void		SetFlagsSz(char *);
void		SetFlagsDefaults();
void		OutputFlush(int);
void		OutputHFile(int, int, unsigned long);
void		OutputB(int, void *, unsigned);
void		OutputTableHead(int, unsigned);
void		OutputPinds(int, INDS *);
void		OutputTail(int, unsigned long);


#define	block	/* block */


void
main(csz, rgsz)
int  csz;
char *rgsz[];
	{
	char		**hsz, **hszMax;
	INDS		rginds[iindsMax], *pinds, *pindsMax;
	long		lfaMac = 0;
	int		hFileOut;
	unsigned	cinds;

#ifdef DEBUG
	block
		{
		/* display commandline parameters */

		int i;
	
		for (i = 0; i < csz; i++)
			{
			printf("%s\n", rgsz[i]);
			}
		}
#endif

	/* check usage */

	if (csz <= 1)
		{
		printf("\nUsage: mkdrv output_file [[-Lfds] service]*\n");
		exit(1);
		}

	/* open output file */

	hFileOut = open(rgsz[1], O_CREAT | O_TRUNC | O_WRONLY | O_BINARY, S_IWRITE);
	if (hFileOut == -1)
		{
		printf("cannot write %s\n", rgsz[1]);
		exit(5);
		}

	/* process given parameters */

	hszMax = &(rgsz[csz]);
	pindsMax = &(rginds[iindsMax]);
	SetFlagsDefaults();
	for (	hsz = &(rgsz[2]), pinds = rginds;
		hsz < hszMax && pinds < pindsMax;
		hsz++)
		{
		char ch;

		if ((ch = **hsz) == '-' || ch == '/')
			{
			/* process flags */

			SetFlagsSz(*hsz + 1);
			}
		else
			{
			/* assume we have a file name of a service, process it */

			int  hFile;
			long lcbCode;
	
			hFile = open(*hsz, O_BINARY | O_RDONLY);
			if (hFile == -1)
				{
				printf("cannot read %s\n", *hsz);
				exit(2);
				}
			lcbCode = filelength(hFile);
			if (lcbCode > lcbCodeMax)
				{
				printf("%s is too big (%l > %l)\n", *hsz, lcbCode, lcbCodeMax);
				exit(3);
				}

			/* build INDS record for this service */

			if ((pinds->indt = IndtFromSzFile(*hsz)) == indtNil)
				{
				printf("warning: %s is an unknown service\n", *hsz);
				}
			pinds->fload = (fload == 0) ? floadDefault : fload;
			pinds->cbCode = (int)lcbCode;
			pinds++;

			/* transfer service to output file */

			OutputHFile(hFileOut, hFile, lcbCode);
			lfaMac += lcbCode;

			SetFlagsDefaults();
			}
		}

	/* check for too many services */

	if (hsz < hszMax)
		{
		printf("too many services (%d max)\n", iindsMax);
		exit(4);
		}

	/* output master table */

	pindsMax = pinds;
	cinds = pindsMax - rginds;
	OutputTableHead(hFileOut, cinds);
	block
		{
		/* output INDS records */

		long dlfa = 0;
	
		while (pinds-- > rginds)
			{
			dlfa += pinds->cbCode;
			pinds->dlfaCode = dlfa;
			OutputPinds(hFileOut, pinds);
			}
		}

	/* output file end header (INDH) */

	OutputTail(hFileOut, (unsigned long)(sizeof(INDT) + (cinds - 1) * sizeof(INDS) + sizeof(INDH)));
		/* cinds - 1: 1 INDS already in INDT */
	OutputFlush(hFileOut);
	close(hFileOut);

	exit(0);
	}


void
SetFlagsDefaults()
	{
	fload = 0;
	}


void
SetFlagsSz(pch)
char *pch;
	{
	/* set flag values accordingly */

	char ch = *pch;

	if (ch == 'L')
		{
		++pch;
		while ((ch = *pch++) != '\0')
			{
			if (ch == 'f')
				fload |= floadFixed;
			else if (ch == 's')
				fload |= floadStandard | floadAnyMode;
			else if (ch == 'r')
				fload |= floadStandard | floadRealMode;
			else if (ch == 'p')
				fload |= floadStandard | floadProtMode;
			else
				printf("bad load flag: %c\n", ch);
			}
		}
	else
		{
		printf("unknown flag: %c\n", ch);
		}
	}


char
IndtFromSzFile(szFile)
char *szFile;
	{
	/* return service type based on extension of service name
		(valid service names are assumed to be .xxx)
	*/

	unsigned cch = strlen(szFile);
	char    *pchExt;
	char    szExt[5];
	SVI *	psvi;

	if (cch < 4)
		{
		return(indtNil);
		}

	block
		{
		/* lower the file extension */

		char *pch = szExt;
		char *pchExtMax = szFile + cch;
	
		for (pchExt = pchExtMax - 4; pchExt < pchExtMax;)
			{
			*pch++ = tolower(*pchExt);
			pchExt++;
			}
		szExt[4] = '\0';
		pchExt = szExt;
		}

	/* determine service type */
	for (psvi = rgsvi; psvi->sz != NULL; psvi++)
		if (strcmp(pchExt, psvi->sz) == 0)
			return(psvi->indt);

	return(indtNil);
	}


/* Output Routines:

	Nothing is flushed from the buffer until it is filled.  Only
	OutputFlush does a write.  OutputHFile and OutputB transfer
	data to the buffer and flush accordingly.

*/


void
OutputFlush(hFile)
int hFile;
	{
	if (cbBuff > 0 && cbBuff != write(hFile, rgb, cbBuff))
		{
		printf("...write error\n");
		exit(10);
		}
#ifdef DEBUG
	printf("wrote %u bytes\n", cbBuff);
#endif
	cbBuff = 0;
	}


void
OutputHFile(hFileOut, hFile, lcb)
int      hFileOut;
int      hFile;
unsigned long lcb;
	{
	unsigned cbRead;

#ifdef DEBUG
	printf("process %lu bytes\n", lcb);
#endif
	while (lcb > 0)
		{
		if (cbBuff >= cbBuffMax)
			{
			OutputFlush(hFileOut);
			}
		cbRead = cbBuffMax - cbBuff;
		if ((unsigned long )cbRead > lcb)
			{
			cbRead = (int)lcb;
			}
		if (cbRead != read(hFile, &(rgb[cbBuff]), cbRead))
			{
			printf("...read error\n");
			exit(20);
			}
#ifdef DEBUG
		printf("read %u bytes\n", cbRead);
#endif
		cbBuff += cbRead;
		lcb -= cbRead;
		}
	}


void
OutputB(hFile, pb, cb)
int      hFile;
void     *pb;
unsigned cb;
	{
	unsigned cbMove;

#ifdef DEBUG
	printf("transfer %u bytes\n", cb);
#endif
	while (cb > 0)
		{
		if (cbBuff >= cbBuffMax)
			{
			OutputFlush(hFile);
			}
		cbMove = cbBuffMax - cbBuff;
		if (cbMove > cb)
			{
			cbMove = cb;
			}
		memcpy(&(rgb[cbBuff]), pb, cbMove);
		cbBuff += cbMove;
		cb -= cbMove;
		}
	}


void
OutputTableHead(hFileOut, cinds)
int      hFileOut;
unsigned cinds;
	{
	INDT indt;

	memcpy(indt.rgchMagic, szMagic, 4);
	indt.cinds = cinds;
#ifdef DEBUG
	printf("INDT: magic:%c%c%c%c, cinds:%u\n", indt.rgchMagic[0],
				       indt.rgchMagic[1],
				       indt.rgchMagic[2],
				       indt.rgchMagic[3],
				       indt.cinds);
#endif
	OutputB(hFileOut, &indt, sizeof(INDT) - sizeof(INDS));
		/* do not include the INDS in INDT */
	}


void
OutputPinds(hFileOut, pinds)
int  hFileOut;
INDS *pinds;
	{
#ifdef DEBUG
	printf("INDS: indt:%u, fload:%u, cbCode:%u, dlfaCode:%lu\n", pinds->indt, pinds->fload,
		pinds->cbCode, pinds->dlfaCode);
#endif
	OutputB(hFileOut, pinds, sizeof(INDS));
	}


void
OutputTail(hFileOut, dlfa)
int hFileOut;
unsigned long dlfa;
	{
	INDH indh;

	memcpy(indh.rgchMagic, szMagic, 4);
	indh.dlfaTable = dlfa;
#ifdef DEBUG
	printf("INDH: magic:%c%c%c%c, dlfaTable:%lu\n", indh.rgchMagic[0],
					indh.rgchMagic[1],
					indh.rgchMagic[2],
					indh.rgchMagic[3],
					indh.dlfaTable);
#endif
	OutputB(hFileOut, &indh, sizeof(indh));
	}
