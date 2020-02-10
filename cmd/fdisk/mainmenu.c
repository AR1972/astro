;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1983 - 1991
; *                      All Rights Reserved.
; */
/******************************************************************************
*
*  Change Log:
*
*    Date    Who   #                      Description
*  --------  ---  ---  ------------------------------------------------------
*  12/13/90  EGH  C36  Added code to support 8 fixed disks.
*
******************************************************************************/

#include "dos.h"                                                        /* AN000 */
#include "fdisk.h"                                                      /* AN000 */
#include "subtype.h"                                                    /* AN000 */
#include "extern.h"                                                     /* AN000 */
#include "fdiskmsg.h"                                                   /* AN000 */

/*  */
/******************* START OF SPECIFICATIONS *******************/
/*                                                             */
/* SUBROUTINE NAME: DO_MAIN_MENU                               */
/*                                                             */
/* DESCRIPTIVE NAME: Main menu display and input routine       */
/*                                                             */
/* FUNCTION:                                                   */
/*    Displays the main FDISK menu, accepts and validates      */
/*    input from menu and passes control to requested function */
/*                                                             */
/* NOTES: The following screen is managed by this routine:     */
/*                                                             */
/*       ³0000000000111111111122222222223333333333³            */
/*       ³0123456789012345678901234567890123456789³            */
/*     ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*     00³                                        ³            */
/*     01³                                        ³            */
/*     02³                                        ³            */
/*     03³                                        ³            */
/*     04³FDISK Options                           ³            */
/*     05³                                        ³            */
/*     06³Current Fixed Disk Drive: #             ³            */
/*     07³                                        ³            */
/*     08³Choose one of the following:            ³            */
/*     09³                                        ³            */
/*     10³    1.  Create DOS partition            ³            */
/*     11³    2.  Change Active Partition         ³            */
/*     12³    3.  Delete DOS Partition            ³            */
/*     13³    4.  Display Partition Data          ³            */
/*     14³    5.  Select Next Fixed Disk Drive    ³            */
/*     15³                                        ³            */
/*     16³                                        ³            */
/*     17³                                        ³            */
/*     18³Enter choice: [#]                       ³            */
/*     19³                                        ³            */
/*     20³                                        ³            */
/*     21³WARNING! No partitions marked active    ³            */
/*     22³                                        ³            */
/*     23³Press ESC to return to DOS              ³            */
/*     ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*                                                             */
/* ENTRY POINTS: do_main_menu                                  */
/*      LINKAGE: do_main_menu();                               */
/*               NEAR CALL                                     */
/*                                                             */
/* INPUT: None                                                 */
/*                                                             */
/* EXIT-NORMAL: ERROR=FALSE                                    */
/*                                                             */
/* EXIT-ERROR: ERROR=TRUE                                      */
/*             GOTO internal_program_error if case statement   */
/*             failure when branching to requested function    */
/*                                                             */
/* EFFECTS: No data directly modified by this routine, but     */
/*          child routines will modify data.                   */
/*                                                             */
/* INTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*      clear_screen                                           */
/*      display                                                */
/*      get_num_input                                          */
/*      create_partition                                       */
/*      change_active_partition                                */
/*      delete_partition                                       */
/*      display_partition_information                          */
/*      find_active_partition                                  */
/*      change_drive                                           */
/*      internal_program_error                                 */
/*                                                             */
/* EXTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*                                                             */
/******************** END OF SPECIFICATIONS ********************/

/*  */
void do_main_menu()

BEGIN

char   input;
char   max_input;

FLAG	error_switch;

unsigned    temp;
unsigned    i;


   input = c(NUL);                                                      /* AC000 */
   PercentFlag = (FLAG)FALSE;                                           /* AN000 */
   /* Intialize cur_disk indicator. It is 0 based for array usage */
   /* See if first disk readable */
   cur_disk = c(0);                                                     /* AC000 */
   if (!good_disk[0])
      BEGIN
       cur_disk++;
      END

   /* See if we have a valid combo of disk */
/*C36   if ((good_disk[0]) ||                                     */
/*C36          ((good_disk[1]) && (number_of_drives == uc(2))) || */                                            /* AC000 */
/*C36          ((good_disk[2]) && (number_of_drives == uc(3))) || */
/*C36          ((good_disk[3]) && (number_of_drives == uc(4))) || */
/*C36          ((good_disk[4]) && (number_of_drives == uc(5))) || */
/*C36          ((good_disk[5]) && (number_of_drives == uc(6))) || */
/*C36          ((good_disk[6]) && (number_of_drives == uc(7))) )  */
   temp = u(0);                                                         /*C36*/
   for (i=u(0);i<u(number_of_drives);i++)                               /*C36*/
       if (good_disk[i])                                                /*C36*/
           temp = u(1);                                                 /*C36*/
   if (temp == u(1))                                                    /*C36*/

      BEGIN
       clear_screen(u(0),u(0),u(24),u(79));                             /* AC000 */
       /* Display the copyright */
       display(menu_1);

       /* See if we couldn't access drive 1 */
		for (i = 0; i < number_of_drives; i++)
			BEGIN
			 if (!good_disk[i] && (error_switch == FALSE))
				{
           	insert[0] = c(i+'1');                                          /* AC000 */
           	display(error_30);
			 	error_switch = TRUE;
			   }
          END

       /* Display the menu every time this routine is returned to until ESC */
       input = c(NUL);                                                  /* AC000 */
       while (input !=c(ESC))                                           /* AC000 */
          BEGIN
            /* Put up most of the menu */
            display(menu_2);
            display(menu_3);
            display(menu_7);

            /* Put correct disk in current disk message */
            insert[0]=cur_disk+1+'0';
            display(menu_5);

            /* Display warning prompt if no active partitions */
            /* check to see if there is an avail partition                 */
            temp = u(0);                                                /* AC000 */
            for (i = u(0); i < u(4);i++)                                /* AC000 */
               BEGIN

                /* See if any non - zero system id bytes */
                temp = temp | part_table[cur_disk][i].sys_id ;
               END
            /* Any entry that isn't zero means */
            if (temp != u(0))                                           /* AC000 */
               BEGIN
                /* If there isn't an active partition and this is disk 1, then yell) */
                if ((!find_active_partition()) && (cur_disk == c(0)))   /* AC000 */
                   display(menu_6);
               END

            /* Get the menu input */

            /* See if more than one fixed disk */
            if (number_of_drives > uc(1))                              /* AC000 */
              BEGIN
               display(menu_4);
               max_input = c(5);                                        /* AC000 */
              END
            else     /* only 4 options */
                max_input = c(4);                                       /* AC000 */
            /* Setup default and go get input */
            input = get_num_input(c(1),max_input,input_row,input_col);  /* AC000 */
            switch(input)
              BEGIN
               case  '1': create_partition();
                          break;

               case  '2': change_active_partition();
                          break;

               case  '3': delete_partition();
                          break;

               case  '4': display_partition_information();
                          break;

               case  '5': change_current_drive();
				  				 break;

               case  ESC: break;  /* ESC case */

               default:   internal_program_error();
              END
          END
      END
   else
      BEGIN
       /* Can't read any drive, so quit */
       no_fatal_error = c(FALSE);                                       /* AC000 */
       display(error_2);
      END
   return;
END

