/*
 * MC & CG                               Test stuff.
 * Copyright (C) Microsoft, 1989
 * March 15, 1989
 *
 */

#include <sys\types.h>
#include <io.h>           /* For file I/O  */
#include <sys\stat.h>     /* For file I/O  */
#include <fcntl.h>        /* For file I/O  */
#include <dos.h>          /* For file I/O  */
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include <stdio.h>   /* for null def ect. */
#include "..\dos\setup.h"

char szHimemSwitch[2];

/* Main()
*
*
*/
void main(argc,argv)
int  argc;
char **argv;
{

   int       back;
   int       iEGA;
   int       iHimem;
   int       iSmartdrvMAX;
   int       iSmartdrvMIN;
   int       iLim;
   int       iRamdrive;
   char      szMouseDrv[20];
   char      szMouseName[20];
   char      szVerString[20];

   fnGetSmartDrvVersion();

   fnGetRamDriveVersion((LPSTR)szVerString);

   bIsNetSetup = TRUE;

   if (XmsInstalled())
	   printf("XMS Version %lx\n", XmsVersion());
   else
   	printf("No XMS installed\n");

   printf("CMOS Ram reports extended memory = %d\n", get_ext());
   
   // Open the .inf file because we need to read the compatibility sections.

   if ( infOpen("setup.inf") == NULL )
      printf("Error opening setup.inf file\n");

   strcpy(szDiskPath,"C:\\netpath");
   strcpy(szSetupPath,"C:\\goober");

   // First, get all the current prams and print them !

   back = fnProcessFile(&iEGA,&iHimem,&iSmartdrvMIN,&iSmartdrvMAX,&iLim,
                        &iRamdrive,RETURN_PRAMS);
 
   printf("\nParameters from Config.Sys RETURN_PRAMS Call:\n");
   printf("SmartDrive MAX: %i \n", iSmartdrvMAX);
   printf("SmartDrive MIN: %i \n", iSmartdrvMIN);
   printf("RamDrive      : %i \n", iRamdrive);
   printf("LimDriver     : %i \n", iLim);

   if ( iHimem )
      printf("Himem Present\n");
   else
      printf("Himem Not Present\n");

   if ( iEGA )
      printf("EGA.SYS Present\n");
   else
      printf("EGA.SYS Not Present\n");

   /* Lets see if we need to rem out any incompatable drivers. */
   
//   back = fnProcessFile(&iEGA,&iHimem,&iSmartdrvMIN,&iSmartdrvMAX,&iLim,
//                        &iRamdrive,CHECK_COMPAT);

   // Now let's excersize the munger by making it add the world to your
   // config.sys and then write the file as config.win.

   iEGA          = TRUE;   // Add ega.sys
   iHimem        = TRUE;   // Add himem
   iSmartdrvMAX  = 2222;   // Put in a smartdrv with a min and max
   iSmartdrvMIN  = 1111;
   iLim          = REMOVE_DEVICE;   // Stick in a limulator while were at it !
   iRamdrive     = 4444;   // What the hell, why not add a ramdrive to !

   back = fnProcessFile(&iEGA,&iHimem,&iSmartdrvMIN,&iSmartdrvMAX,&iLim,
                        &iRamdrive,SET_PRAMS);

   back = fnProcessFile(&iEGA,&iHimem,&iSmartdrvMIN,&iSmartdrvMAX,&iLim,
                        &iRamdrive,MUNGE_AUTO);

   printf("Mouse Driver Version %x\n",fnMouseDriverVer());

   printf("*****************************\n");

   printf("\nEnter a mouse operation: ( add_y, mouse, or mousehp )\n");
   gets(szMouseName);
   printf("%s Operation Chosen\n",szMouseName);

//   fnTweekMouse(szMouseDrv,szMouseName);

   printf("\nDos Mouse Driver Needed: %s\n",szMouseDrv);

   back = fnProcessFile(&iEGA,&iHimem,&iSmartdrvMIN,&iSmartdrvMAX,&iLim,
                        &iRamdrive,ASSURE_MOUSE_POSITION);

   back = fnProcessFile(&iEGA,&iHimem,&iSmartdrvMIN,&iSmartdrvMAX,&iLim,
                        &iRamdrive,WRITE_BAK);
} 

void _Assert(void)
{
}


