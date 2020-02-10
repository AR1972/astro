;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

int     (*signal())();

#define NSIG    17      /* (one greater than) number of signals */

/*      Signal numbers */
#define SIGINT  1               /* ^C or user defined key */
#define SIGKB0  1               /* ^C or user defined key */
#define SIGKB1  2               /* alternate key intercept */
#define SIGKB2  3               /* alternate key intercept */
#define SIGMUF  4               /* special key intercept for MUF */
#define SIGDIVZ 5               /* divide by zero trap */
#define SIGOVFL 6               /* INTO instruction */
#define SIGHDERR 7              /* INT 24 type things */
#define SIGTERM 8               /* program termination */
#define SIGPIPE 9               /* broken pipe */
#define SIGUSR1 13              /* reserved for user definition */
#define SIGUSR2 14              /* reserved for user definition */

/*      Signal actions */
#define SIG_DFL (int (*)())0    /* terminate process on receipt */
#define SIG_IGN (int (*)())1    /* ignore */
#define SIG_GET 2               /* signal is accepted */
#define SIG_ERR 3               /* sender gets error */
#define SIG_ACK 4               /* acknowledge received signal */


#ifndef SIGKILL
#define SIGKILL 0
#endif
