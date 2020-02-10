/*
 * direct.h
 *
 * This include file contains the function declarations for the library
 * functions related to directory handling and creation.
 *
 *
 *   Microsoft Confidential
 *   Copyright (C) Microsoft Corporation 1991
 *   All Rights Reserved.
 *
 *
 */

/* function declarations for those who want strong type checking
 * on arguments to library function calls
 */

#ifdef LINT_ARGS		/* arg. checking enabled */

int chdir(char *);
char *getcwd(char *, int);
int mkdir(char *);
int rmdir(char *);

#else

extern char *getcwd();

#endif	/* LINT_ARGS */
