/*
	COW : Character Oriented Windows

	echotime.c : a simple ECHOTIME utility
		echo arguements, replace /t with ctime time

*/

#include <stdio.h>
#include <fcntl.h>

main(argc, argv)
int argc;
char *argv[];
	{
	int iarg;

	for (iarg = 1; iarg < argc; iarg++)
		{
		char *pch = argv[iarg];

		while (*pch)
			{
			if (*pch == '/' && *(pch+1) == 't')
				{
				long time();
				long l = time(NULL);
				char *ctime();
				char *sz = ctime(&l);

				sz[strlen(sz)-1] = '\0';
				printf("%s", sz);
				pch += 2;
				}
			else
				putchar(*pch++);
			}
		putchar(' ');
		}
	putchar('\n');
	exit(0);
	}

