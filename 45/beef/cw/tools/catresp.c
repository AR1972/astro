/*
	CW : Character Windows

	catresp.c : helper program for New Make

	Flags "-string" => break lines using string

	1st parm = file to concatenate to
	2nd parm = "@tmp" -- response file containing file name list

	ConCatenate response file to end of specified file
*/

#include <stdio.h>
#include <fcntl.h>


main(argc, argv)
int argc;
char *argv[];
	{
	FILE *	pfileList;
	FILE *	pfileOut;
	int	ch;			/* char + EOF */
	int	cchLine;
	char *	szBreak = NULL;

	if (argc == 4 && argv[1][0] == '-')
		{
		szBreak = &argv[1][1];
		argv++; argc--;
		}

	if (argc != 3 || argv[2][0] != '@')
		{
		printf("usage: catresp [-string] <out_file> @<response_file>\n");
		exit(1);
		}

	if ((pfileList = fopen(argv[2]+1, "rt")) == NULL)
		{
		printf("catresp: can't open response file\n");
		exit(1);
		}

	if ((pfileOut = fopen(argv[1], "at")) == NULL)
		{
		printf("catresp: can't open output file\n");
		exit(1);
		}

	cchLine = 0;
	while ((ch = getc(pfileList)) != EOF)
		{
		if (ch == '\n')
			cchLine = 0;
		else
			cchLine++;

		// optional break lines
		if (szBreak != NULL && ch == ' ' && cchLine > 70)
			{
			/* end that line, start another */
			fputs(szBreak, pfileOut);
			ch = '\n';
			cchLine = 0;
			}
		putc(ch, pfileOut);
		}

	exit(0);
	}

