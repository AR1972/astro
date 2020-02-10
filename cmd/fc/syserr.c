/***
*syserr.c - clone of the system error list that appears in the
*           standard library.
*
*   Copyright (c) 1987-1990, Microsoft Corporation. All rights reserved.
*   Adapted by TerryF for IPG October 1991.
*
*Purpose:
*   Defines the System Error List, containing the full messages for
*   all errno values set by the library routines.
*   Defines sys_errlist.
*
*******************************************************************************/

/* standard lib uses the name sys_errlist */
char *sys_errorlist[] =
{
#include "syserr.msg"    /* TerryF -> IPG, localize syserr.msg */
};

/* standard lib uses the name sys_nerr */
int system_nerr = sizeof( sys_errorlist ) / sizeof( sys_errorlist[ 0 ] ) - 1;

/* The above array contains all the errors including unknown error # 37
   which is used if msg_num is unknown */
