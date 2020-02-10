#include <windows.h>
#include <stdio.h>


/*
 *  macros used to define functions:
 *
 *	EXPORT	    - This function can be called from external to this DLL
 *		      but be sure to list it in you DEF file!
 *
 *	PUBLIC	    - This function can be called from another file
 *
 *	PRIVATE     - This function can only be called from inside this file
 *
 *  eg	  int EXPORT foo();
 */

#define EXPORT	    FAR  PASCAL
#define PUBLIC	    FAR  PASCAL
#define PRIVATE     NEAR PASCAL

#ifdef DEBUG
	#define WinAssert(exp)\
				{\
					if (!(exp))\
						{\
							char szBuffer[40];\
							sprintf(szBuffer, "File %s, Line, %d",\
								__FILE__, __LINE__);\
							if (IDABORT == MessageBox(NULL, szBuffer,\
															  "Assertion Error",\
																MB_ABORTRETRYIGNORE | \
																MB_ICONSTOP))\
								FatalExit(-1);\
						}\
				}
#else
   #define WinAssert(exp)  
#endif
