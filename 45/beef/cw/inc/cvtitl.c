/*
	cvtitl.c : pipe : convert illegal characters to \xxx characters
*/

#include <stdio.h>

main()
	{
	int	ch;

	while ((ch = getchar()) != EOF)
		{
		if (ch < 0x80)
			putchar(ch);
		else
			printf("\\%3o", ch);
		}
	}
