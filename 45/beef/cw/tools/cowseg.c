/*
	COW : Character Oriented Windows

	cowseg.c : segment order fix utility

	NOTE : This is my (SAR) copy of the DOSSEG tool.
	It only allows addition of the Force segment comment to MASM .OBJ files
*/

#include <stdio.h>
#include <fcntl.h>

#ifdef X86
#define O_BINARY 0
#endif

#define cbInsert 6
char rgbInsert[] = { 0x88, 3, 0, 0, 0x9e, 0x29 };	/* insert Force seg ordering */

#define cbCopy 1024


main(argc, argv)
int argc;
char *argv[];
	{
	int	fdIn, fdOut;
	unsigned char rgbCopy[cbCopy];
	int	cb;
	int	cbHeader;

	if (argc != 3)
		{
		printf("usage: cowseg <src file> <dest file>\n");
		exit(1);
		}

	if ((fdIn = open(argv[1], O_RDONLY | O_BINARY)) == -1)
		{
		printf("cowseg: can't open %s\n", argv[1]);
		exit(1);
		}

	if ((fdOut = open(argv[2], O_WRONLY | O_BINARY | O_CREAT | O_TRUNC, 0777)) == -1)
		{
		printf("cowseg: can't open %s\n", argv[2]);
		exit(1);
		}

	/* read in first 3 bytes: should be 0x80, word size of record */
	if (read(fdIn, rgbCopy, 3) != 3 ||
	    rgbCopy[0] != 0x80 ||		/* valid THEADR ? */
	    rgbCopy[2] != 0 ||			/* < 255 chars ? */
	    read(fdIn, rgbCopy+3, rgbCopy[1]) != rgbCopy[1])
		{
		printf("cowseg: file %s in invalid format\n", argv[1]);
		exit(1);
		}
	cbHeader = 3 + rgbCopy[1];

	/* Copy the header + insert portion */
	if (write(fdOut, rgbCopy, cbHeader) != cbHeader ||
	    write(fdOut, rgbInsert, cbInsert) != cbInsert)
		{
		printf("cowseg: write error\n");
		exit(1);
		}

	/* copy the rest of the file */
	do
		{
		if ((cb = read(fdIn, rgbCopy, cbCopy)) == -1)
			{
			printf("cowseg: read error\n");
			exit(1);
			}

		if (write(fdOut, rgbCopy, cb) != cb)
			{
			printf("cowseg: write error\n");
			exit(1);
			}
		} while (cb>0);

	close(fdIn);
	close(fdOut);
	}

