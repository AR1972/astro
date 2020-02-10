/*****************************************************************/
/**		     Microsoft LAN Manager			**/
/**	       Copyright(c) Microsoft Corp., 1987-1990		**/
/*****************************************************************/
/********************************************************************
 *								    *
 *  About this file ...  CONFIG.H				    *
 *								    *
 *  This file contains information about the NetConfig APIs.	    *
 *								    *
 ********************************************************************/

/*NOINC*/
#ifndef NETCONFIG_INCLUDED

#define NETCONFIG_INCLUDED
/*INC*/


/****************************************************************
 *                                                              *
 *              Function prototypes                             *
 *                                                              *
 ****************************************************************/

extern API_FUNCTION
  NetConfigGet ( const char far *     pszComponent,
		 const char far *     pszParameter,
		 char far *	      pbBuffer,
                 unsigned short       cbBuffer,
		 unsigned short far * pcbParmlen );

extern API_FUNCTION
  NetConfigGetAll ( const char far *	 pszComponent,
		    char far *		 pbBuffer,
                    unsigned short       cbBuffer,
		    unsigned short far * pcbReturned,
		    unsigned short far * pcbTotalAvail );

extern API_FUNCTION
  NetConfigGet2 ( const char far *     pszServer,
		  const char far *     pszReserved,
		  const char far *     pszComponent,
		  const char far *     pszParameter,
		  char far *	       pbBuffer,
                  unsigned short       cbBuffer,
		  unsigned short far * pcbParmlen );

extern API_FUNCTION
  NetConfigGetAll2 ( const char far *	  pszServer,
		     const char far *	  pszReserved,
		     const char far *	  pszComponent,
		     char far * 	  pbBuffer,
                     unsigned short       cbBuffer,
		     unsigned short far * pcbReturned,
		     unsigned short far * pcbTotalAvail );


/**INTERNAL_ONLY**/

struct config_info_0 {
	char far * Key;
	char far * Data;
};

extern API_FUNCTION
  NetConfigSet (char far * Server, char far * Reserved1, char far * Component,
		unsigned short Level, unsigned short Reserved2, char far * Buffer,
		unsigned short Buflen, unsigned long Reserved3);
/**END_INTERNAL**/

/*NOINC*/
#endif /* NETCONFIG_INCLUDED */
/*INC*/
