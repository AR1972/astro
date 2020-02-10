/***************************************************************************/
/*																									*/
/*	MAXINT.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Scans an array of integers and returns the index to the max int in the	*/
/* array. If 2 or more integers equal MAX the index to the first one in		*/
/* the array is used. If all elements in the array are 0 a -1 is returned.	*/
/*																									*/
/* ARGUMENTS:	aiMatch	- Array of integers to search.							*/
/*					iLen		- Number of elements in the array.						*/
/* RETURNS:		int		- First maximium value in the array or -1 if all	*/
/*								  elements == 0.												*/
/*                                                                         */
/* johnhe - 08/01/89																			*/
/***************************************************************************/

int IndexMaxInt( int aiMatch[], int iLen )
{
	int		i, iMax, iFirstMax;

										/* One loop for each element in the array */
	for ( i = iMax = iFirstMax = 0; i < iLen; i++ )
	{
		if ( aiMatch[i] > iMax )				/* Is this element > than max */
		{
			iMax = aiMatch[i];					/* Save new max value			*/
			iFirstMax = i;							/* Save new array index			*/
		}
	}

	return( iMax == 0 ? -1 : iFirstMax );	/* Index to max element or -1	*/
}
