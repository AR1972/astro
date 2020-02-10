;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; *	   IPG-changes by Terry Farrel on October 1991
; */

/* error.c - return text of error corresponding to the most recent DOS error */

// #include "tools.h" - no longer needed   TF

extern int errno;
extern system_nerr;             // IPG- was sys_nerr
extern char *sys_errorlist[];   // IPG- changed from sys_errlist, found in C libs.

#define UNKNOWN 37              /* TF -> points to sys_errorlist         */

char *error ()
{
    if (errno < 0 || errno >= system_nerr)
        return sys_errorlist[UNKNOWN];
    else
        return sys_errorlist[errno];
}

