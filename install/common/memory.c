/***************************************************************************/
/*																									*/
/*	MEMORY.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*																									*/
/* The following code will do error checking for malloc() calls #ifndef		*/
/*	MEM_BUG																						*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include    <malloc.h>
#include		<dos.h>
#include 	<string.h>

#include		<alias.h>
#include		<message.h>
#include		<window.h>

/************************************************************************/

void  InitializeMemory( void );
unsigned GetCallerAddr( void );

/* #define		MEM_BUG	1 */

/************************************************************************/

#ifndef		MEM_BUG

/************************************************************************/
/* Used in place of the malloc() functions so the caller will not have	*/
/* to do error checking on the return value. It is assumed that the		*/
/* program cannot continue if the requested memory cannot be allocated 	*/
/* so this function will do the honors of calling the FatalError() 		*/
/* function to display the error window and do the necessary cleanup.	*/
/* If you want to do the error handling yourself don't use this			*/
/* and call malloc directly.															*/
/*																								*/
/* void *GetMemory( unsigned int Bytes )											*/
/* 																							*/
/* ARGUMENTS:	unsigned	- Number of bytes of memory to allocate.			*/
/* RETURNS:		char *	- Returns a pointer to the requested block of	*/
/*								  memory. NOTE: This function will not return	*/
/*								  if the requested block of memory is not 		*/
/*								  available, instead it will abort the program	*/
/*								  with a FatalError() call.							*/
/*																								*/
/************************************************************************/

void *GetMemory( unsigned int Bytes )
{
	void		*Memory;
	
	if ( (Memory = malloc( Bytes )) == NULL )
		FatalError( FATAL_MEMORY_ERROR );
	return( Memory );
}


#else

/************************************************************************/
/* All of the rest of the code in this module is only here for				*/
/* memory allocation problem. To turn the code on define MEM_BUG.			*/
/* These functions are replaced by macros or very simple functions		*/
/* when debugging is turned off so that there is no size of speed 		*/
/* restrictions imposed by these functions.										*/
/************************************************************************/

#include		<bios_io.h>

struct mems
{
   unsigned int   Size;
   void           *Ptr;
};

/***************************************************************************/
static struct mems   *Buffer = NULL;

/***************************************************************************/
/* Returns a pointer to a block of memory the size of the argument Byte.   */
/* if there is insuffient memory available to fill the request a fatal     */
/* error is initiated and the program is aborted. A control structure is   */
/* set up for each block of memory that is allocated. See the structure    */
/* definition "mems" for details of the control structure.                 */
/*                                                                         */
/* Bytes - the size of the request memory block in bytes                   */
/* RETURNS: pointer to a block of memory the requested size                */
/***************************************************************************/

void *GetMemory( unsigned int Bytes )
{
	struct _heapinfo  hinfo;
	int					hstat;
	register			i;									/* Loop counter					*/
	struct mems		*BufPtr;							/* Ptr to allocation struct	*/
	void				*Addr;							/* Ptr to allocated block		*/
	static char		*ErrorMess[] = {
												"Out of pointer space",
											 	"Aborting with Fatal Error"
											};

   if ( Buffer == NULL )
      InitializeMemory(); /* Initializes on the first call to this function */


	if ( _heapchk() != _HEAPOK ) 
      FatalError( FATAL_MEMORY_ERROR );

	hinfo._pentry = NULL;

	while( (hstat = _heapwalk( &hinfo)) == _HEAPOK )
		;
	if ( hstat != _HEAPEMPTY && hstat != _HEAPEND )
		FatalError( FATAL_MEMORY_ERROR );

   if ( ( Addr = malloc( Bytes )) == NULL )  /* Get memory from DOS */
      FatalError( FATAL_MEMORY_ERROR );

      /* Scan the control structure buffer for the first unused entry   */
      /* and fill in it's contents with the size of this block which    */
      /* the malloc() function has stored as a preceeding word to the   */
      /* start of the allocated memory area                             */

   BufPtr = Buffer;									/* Point to start of buf	*/
   for ( i = 0; i < 1000; i++, BufPtr++ )
   {
      if ( BufPtr->Ptr == NULL )					/* Is this an empty entry?	*/
      { 													/* Then use it					*/
         BufPtr->Ptr = Addr;
         BufPtr->Size = *( (unsigned *)(Addr) - 1 );
         break;
      }
   }

      /* If i >= 1000 there were no free pointers so abort program */
      /* This should never happen, if it does increase the buffer size */

   if ( i >= 1000 )
      FatalError( FATAL_MEMORY_ERROR );
   else
      return( Addr );      /* Return pointer to allocated memory block */
}   

/***************************************************************************/
/* Frees a block of memory allocated with GetMemory(). The argument is     */
/* is used to scan the control buffer and if a match is found the size of  */
/* the block is compared with the word which precedes the allocated block. */
/* If these match the block is valid and will be freed. If the block has   */
/* been corrupted a fatal error will be initiated and the program will     */
/* be aborted.                                                             */
/*                                                                         */
/* Addr - address of memory block being deallocated                        */
/***************************************************************************/

void  FreeMemory( void *Addr )
{
	struct _heapinfo  hinfo;
	int					hstat;
	register				i;									/* Loop counter		*/
	register				Status = OK;					/* Return status		*/
   struct mems			*BufPtr;							/* Allocation struct	*/
	static char			*apszError[] = { "Size of block is corrupt",
							  						  "Aborting with Fatal Error" };

	if ( _heapchk() != _HEAPOK ) 
      FatalError( FATAL_MEMORY_ERROR );

	hinfo._pentry = NULL;
	while( (hstat = _heapwalk( &hinfo)) == _HEAPOK )
		;

	if ( hstat != _HEAPEMPTY && hstat != _HEAPEND )
		FatalError( FATAL_MEMORY_ERROR );

      /* Scan the memory control buffer for a matching address and then */
      /* compare the size word to check for a corrupt block             */
   BufPtr = Buffer;
   for ( i = 0; i < 1000; i++, BufPtr++ )
   {
      if ( BufPtr->Ptr == Addr )
      {
         if ( BufPtr->Size != *( (unsigned *)(Addr) - 1) )
         {
            Status = -1;         /* Corrupted memory block so signal error */
            break; 
         }
         else
         {     /* Valid block so release it and clear it's control struct */
            BufPtr->Ptr = NULL;
            BufPtr->Size = 0;

            free( Addr );
            break;
         }
      }
   }
   if ( i >= 1000 || Status != 0 )
      FatalError( FATAL_MEMORY_ERROR );

	if ( _heapchk() != _HEAPOK ) 
      FatalError( FATAL_MEMORY_ERROR );
}

/***************************************************************************/
/* Scans the memory control buffer and returns the number of blocks        */
/* allocated. Call this function at the end of a program to be sure all    */
/* functions are releasing their memory blocks.                            */
/*                                                                         */
/* RETURNS: number of allocated memory blocks                              */
/***************************************************************************/

int   NumberAllocated( void )
{
	register			i;									/* Loop counter					*/
	register			count;							/* Number of items allocated	*/
	struct mems		*BufPtr;

   for ( i = 0, BufPtr = Buffer, count = 0; i < 1000; i++, BufPtr++ )
      if ( BufPtr->Ptr != NULL )
         count++;

   return( count );

}

/***************************************************************************/
/* Returns the maximum memory block size still available. Use this         */
/* before calling GetMemory() if the size of the memory block is not       */
/* critical and then call GetMemory() with the value returned. This will   */
/* guarantee that a fatal memory error won't happen.                       */
/***************************************************************************/

unsigned GetMemoryMax( void )
{
   return( (unsigned)(_memmax()) );
}

void ProgramAbort( void );

/***************************************************************************/
/* Initializes the array of memory control structures.                     */
/***************************************************************************/

void  InitializeMemory( void )
{
   register			i;							/* Loop index								*/
   struct mems    *BufPtr;					/* Ptr to allocated memory buf		*/

         /* Allocate memory for the buffer */
   if ( ( Buffer = malloc( 1000 * sizeof(struct mems) ) ) == NULL )
	{
      VideoPuts( "\nINSUFFIENT MEMORY TO RUN PROGRAM" );
		ProgramAbort();
	}

   else
   {     /* Initialize the array of memory control structures */
      BufPtr = Buffer;
      for ( i = 0; i < 1000; i++, BufPtr++ )
      {
         BufPtr->Size = 0;
         BufPtr->Ptr = NULL;
      }                        
   }
}

#endif

/************************************************************************/
/* End of module.																			*/
/************************************************************************/
