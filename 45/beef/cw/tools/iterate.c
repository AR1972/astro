/*
	CW : Character Windows

	iterate.c : helper program for New Make

	argv[1] = program string to run (%s replaced by file name)
	argv[2] = "@tmp" -- response file containing file name list

	-- iterate over the file name list running program
*/

#include <stdio.h>
#include <fcntl.h>


char	rgch[5000];		/* buffer for response file */
				/* Nmake spews out just 1 line */

main(argc, argv)
int argc;
char *argv[];
	{
	FILE *	pfileList;

	if (argc != 3 || argv[2][0] != '@')
		{
		printf("usage: iterate <cmd> @<response_file>\n");
		exit(1);
		}

	if ((pfileList = fopen(argv[2]+1, "rt")) == NULL)
		{
		printf("iterate: can't open response file\n");
		exit(1);
		}

	while (fgets(rgch, sizeof(rgch), pfileList) != NULL)
		{
		char *	sz = rgch;

		while (*sz != '\0' && *sz != '\n')
			{
			/* skip whitespace */
			if (*sz == ' ' || *sz == '\t')
				sz++;
			else
				{
				/* we have a name */
				char *	szFile;
				char	szT[128];

				/* we have a file name */
				szFile = sz;
				while (1)
					{
					sz++;
					if (*sz == '\0' || *sz == '\n')
						{
						/* end of line & name */
						*sz = '\0';
						break;
						}
					else if (*sz == ' ' || *sz == '\t')
						{
						/* just end of name */
						*sz++ = '\0';
						break;
						}
					}

				sprintf(szT, argv[1], szFile);
				printf("\t%s\n", szT);
				if (system(szT) != 0)
					{
					printf("error running command\n");
					exit(2);
					}
				}
			}
		}
	exit(0);
	}
