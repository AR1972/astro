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
*  03/07/90  EGH  C03  Problem fixed - Ctrl-C allowed as input causing screen
*                      to scroll.  Fix is to use INT21 function 07h instead of
*                      08h.
*  03/08/90  EGH  C00  Cleaned up build by removing unused variables, declaring
*                      functions properly, changing long JMPs to short JMPs,
*                      etc.
*  03/08/90  EGH  C09  Problem fixed - user selected input difficult to read.
*                      Fix is to use hi-white on black or hi-white on blue,
*                      depending on the video mode, for user input.
*  03/20/90  EGH  C12  Problem fixed - if user enters an invalid partition
*                      size, recovery is awkward.  Fix is to modify user
*                      interface.  STR #2027
*  03/20/90  EGH  C13  Problem fixed - the cursor is in the wrong position
*                      when the "Press ESC to continue" message is displayed.
*                      Fix is to to determine cursor position based on
*                      input_row and input_col variables.  This also makes
*                      internationalization easier.
*  05/22/90  EGH  C16  Problem fixed - graphic characters were appearing in
*                      input fields when CTRL or ALT key combinations were
*                      pressed.  Fix is to to determine whether or not input
*                      is valid before displaying it.  Invalid input will
*                      result in a beep and error message.
*
******************************************************************************/

#include "dos.h"                                                        /* AN000 */
#include "fdisk.h"                                                      /* AN000 */
#include "subtype.h"                                                    /* AN000 */
#include "extern.h"                                                     /* AN000 */
#include "ctype.h"                                                      /* AN000 */
#include "string.h"                                                     /* AN000 */
#include "stdio.h"
#include "fdiskmsg.h"                                                   /* AN000 */
#include "doscall.h"                                                    /* AN000 */

/*  */
char get_num_input(input_default,max_num,row,col)

char   max_num;
unsigned        row;
unsigned        col;
char            input_default;

BEGIN

    char  input;

    /* SR;9/25/89; Initialize this on entry */
    char   default_used = FALSE;
    char   input_value;

    char            attribute;
    char far        *attribute_ptr = &attribute;
    char far *input_ptr = &input;

    if (mono_flag == TRUE)
/*C09   attribute = GRAY_ON_BLACK;  */
        attribute = HIWHITE_ON_BLACK; /*C09*/
    else
/*C09   attribute = WHITE_ON_BLUE;  */
        attribute = HIWHITE_ON_BLUE;  /*C09*/

    /* print default entry if there is one */
    if (input_default != c(NUL))                                        /* AC000 */

        BEGIN
        default_used = TRUE;
        /* position the cursor */
        VIOSETCURPOS(row,col,u(0));                                     /* AC000 */

        /* Display the default character */

        input = c(input_default+'0');
        VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0));
        END

    /* Assume bad input */
    valid_input = FALSE;

    /* Loop until we get good stuff */
    while (valid_input == FALSE)
        BEGIN

        /* position the cursor */
        VIOSETCURPOS(row,col,u(0));                                     /* AC000 */

        /* Flush the keyboard buffer and get the next pressed key */
        input = get_char_input();

        /* Do world trade get country information */
        input = dos_upper(input);                                       /* AN000 */

        /* Go handle different inputs */
        switch(input)
            BEGIN
            case ESC:
                BEGIN
                valid_input = TRUE;
                break;
                END

            case  CR:
                BEGIN
                /* Set the input to the default if there is one there */
                if (default_used)
                    BEGIN
                    if (input_default != c(NUL))
                        BEGIN
                        input_value = input_default+'0';
                        END
                    else
                        BEGIN
                        /* Make the enter look like a blank for error message */
                        input_value = c(' ');                           /* AC000 */
                        END
                    END
                /* See if it is digit and less or equal to max */
                if ( (isdigit(input_value))         &&
                     (input_value <= (max_num+'0')) &&
                     (input_value != c('0')) )
                    BEGIN
                    valid_input = TRUE;
                    input = input_value;
                    END
                else
                    BEGIN
                    if (isdigit(input_value))
                        BEGIN
                        /* Setup error message */
                        insert[0] = input_value;
                        insert[1] = c('1');                    /* AC000 */
                        insert[2] = c('-');                    /* AC000 */
                        insert[3] = max_num+'0';
                        display(error_23);
                        END
                    else
                        BEGIN
                        insert[0] = c('1');                    /* AC000 */
                        insert[1] = c('-');                    /* AC000 */
                        insert[2] = max_num+'0';
                        display(error_31);
                        END
                    END
                break;
                END

            default:
                BEGIN
                /* SR; 10/2/89; If Ctrl-C, display blank and beep */
/*C16           if ( input != 0x03 )                                             */
/*C16               VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0)); */
/*C16           else                                                             */
/*C16               {                                                            */
/*C16               DOSBEEP( 900, 400 );                                         */
/*C16               input = c(' ');                                              */
/*C16               VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0)); */
/*C16               }                                                            */
                if ( (isdigit(input))         &&                        /*C16*/
                     (input <= (max_num+'0')) &&                        /*C16*/
                     (input != c('0')) )                                /*C16*/
                    clear_screen(u(23),u(0),u(23),u(79));               /*C16*/
                else                                                    /*C16*/
                    {                                                   /*C16*/
                    if (isdigit(input))                                 /*C16*/
                        {                                               /*C16*/
                        /* Setup error message */                       /*C16*/
                        insert[0] = input;                              /*C16*/
                        insert[1] = c('1');                             /*C16*/
                        insert[2] = c('-');                             /*C16*/
                        insert[3] = max_num+'0';                        /*C16*/
                        display(error_23);                              /*C16*/
                        }                                               /*C16*/
                    else                                                /*C16*/
                        {                                               /*C16*/
                        insert[0] = c('1');                             /*C16*/
                        insert[1] = c('-');                             /*C16*/
                        insert[2] = max_num+'0';                        /*C16*/
                        display(error_31);                              /*C16*/
                        }                                               /*C16*/
                    input = c(' ');                                     /*C16*/
                    }                                                   /*C16*/
                VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0)); /*C16*/
                default_used = FALSE;
                input_value = input;
                break;
                END
            END
        END
     return(input);
END



/*  */

char get_yn_input(input_default,row,col)

unsigned        row;
unsigned        col;
char            input_default;

BEGIN

    char   input;
    char   default_used;
    char   input_value;

    char            attribute;
    char far        *attribute_ptr = &attribute;
    char far *input_ptr = &input;

    if (mono_flag == TRUE)
/*C09   attribute = GRAY_ON_BLACK;  */
        attribute = HIWHITE_ON_BLACK; /*C09*/
    else
/*C09   attribute = WHITE_ON_BLUE;  */
        attribute = HIWHITE_ON_BLUE;  /*C09*/

    /* print default entry if there is one */
    if (input_default != c(NUL))                                      /* AC000 */

        BEGIN
        default_used = TRUE;
        /* position the cursor */
        VIOSETCURPOS(row,col,u(0));                                   /* AC000 */

        /* Display the default character */
		 input = input_default;
		 VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0));
        END

    /* Assume bad input */
    valid_input = FALSE;

    /* Loop until we get good stuff */
    while (valid_input == FALSE)
        BEGIN

        /* position the cursor */
        VIOSETCURPOS(row,col,u(0));                                   /* AC000 */

        /* Flush the keyboard buffer and get the next pressed key */
        input = get_char_input();
        input = dos_upper(input);

        /* Go handle different inputs */
        switch(input)
            BEGIN
            case ESC:
                BEGIN
                valid_input = TRUE;
                break;
                END

            case  CR:
                BEGIN
                /* Set the input to the default if there is one there */
                if (default_used)
                    BEGIN
                    if (input_default != c(NUL))                        /* AC000 */
                        BEGIN
                        input_value = input_default;
                        END
                    else
                        BEGIN
                        internal_program_error();
                        END
                    END
                /* See if YES or NO */

                /* Do world trade get country information */
                input = check_yn_input(input_value);                    /* AN000 */

                if ((input == c(1)) || (input == c(0)))                 /* AC000 */
                    BEGIN
                    valid_input = TRUE;
                    END
                else
                    BEGIN
                    /* Setup error message */
                    insert[0] = c(Yes);                        /* AC000 AC011 */
                    insert[1] = c('-');                        /* AC000 */
                    insert[2] = c(No);                         /* AC000 AC011 */
                    display(error_31);
                    END
                break;
                END

            default:
                BEGIN
                /* SR; 10/2/89; If Ctrl-C, display blank and beep */
/*C16           if ( input != 0x03 )                                             */
/*C16               VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0)); */
/*C16           else                                                             */
/*C16               {                                                            */
/*C16               DOSBEEP( 900, 400 );                                         */
/*C16               input = c(' ');                                              */
/*C16               VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0)); */
/*C16               }                                                            */
                if ((check_yn_input(input) == c(1)) ||                  /*C16*/
                    (check_yn_input(input) == c(0)))                    /*C16*/
                    clear_screen(u(23),u(0),u(23),u(79));               /*C16*/
                else                                                    /*C16*/
                    {                                                   /*C16*/
                    /* Setup error message */                           /*C16*/
                    insert[0] = c(Yes);                                 /*C16*/
                    insert[1] = c('-');                                 /*C16*/
                    insert[2] = c(No);                                  /*C16*/
                    display(error_31);                                  /*C16*/
                    input = c(' ');                                     /*C16*/
                    }                                                   /*C16*/
                VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0)); /*C16*/
                default_used = FALSE;
                input_value = input;
                break;
                END
            END
         END
     return(input);
END


/*  */
char wait_for_ESC()

BEGIN
    char  input;

    clear_screen(u(24),u(0),u(24),u(79));                               /*C13*/
    display(menu_46);                                                   /*C13*/
    while (input != c(ESC))                                             /* AC000 */
        BEGIN
        /* position the cursor at the end of the ESC prompt */
/*C13   VIOSETCURPOS(u(24),u(39),u(0));  */                             /* AC000 */
        VIOSETCURPOS(input_row,input_col,u(0));                         /*C13*/

        /* Get input */
        input = get_char_input();

        END
    return(c(ESC));                                                     /* AC000 */
END




XFLOAT get_large_num_input(input_default,max_num,max_percent,input_message,prompt_location,error_message)

unsigned    input_default;                                              /* AC000 */
unsigned    max_num;
unsigned    max_percent;
char far   *input_message;
char far   *error_message;
unsigned   prompt_location;

BEGIN

    char           input;
    XFLOAT         large_input;                                         /* AC000 */
    char           default_used;
/*C12    unsigned long  very_big_input; */
    FLAG           enter_flag;                                          /*C12*/

    /* Assume bad input */
    valid_input = FALSE;

    /* Assume no input, and use default */
    default_used = TRUE;

    /* Assume ENTER key not pressed yet */                              /*C12*/
    enter_flag = FALSE;                                                 /*C12*/

    /* Initialize the input value */
    large_input = u(0);                                                 /* AC000 */

    /* Loop until we get good stuff */
    while (valid_input == FALSE)

        BEGIN
        /* position the cursor */
        VIOSETCURPOS(input_row,input_col,u(0));                     /* AC000 */

        /* Flush the keyboard buffer and get the next pressed key */

        input = get_char_input();

        /* Go handle different inputs */
        switch(input)
            BEGIN
            case ESC:
                valid_input = TRUE;
                large_input = ((unsigned)(ESC_FLAG));
                break;

            case CR:
                BEGIN
                if (PercentFlag)
                    BEGIN
                    /* Set the input to the default if there is one there and nothing else entered */
                    if ((input_default != u(NUL)) && (default_used))  /* AC000 */
                        large_input = input_default;
                    /* See if input is less or equal to max_value */
                    if (large_input <= max_percent)
                        BEGIN
                        if (large_input != u(0))
                            valid_input = TRUE;
                        else
                            display(error_28);
                        END
                    else
                        display(error_message);
                    END
                else
                    BEGIN
                    /* Set the input to the default if there is one there and nothing else entered */
                    if ((input_default != u(NUL)) && (default_used))  /* AC000 */
                        large_input = input_default;
                    /* See if input is less or equal to max_value */
                    if (large_input <= max_num)
                        BEGIN
                        if (large_input != u(0))
                            valid_input = TRUE;
                        else
                            display(error_28);
                        END
                    else
                        display(error_message);
                    END
                enter_flag = TRUE;                                      /*C12*/
                break;
                END

            case BACKSPACE:
                if (PercentFlag)
                    PercentFlag = (FLAG)FALSE;                      /* AN000 */
                else
                    large_input = large_input / 10;

                /* Indicate that we are not using the default */
                default_used = FALSE;
                sprintf(&insert[prompt_location],"%4.0d",large_input);  /* AN000 */
                display(input_message);
                break;

            case PERCENT:                                           /* AN000 */

                if (PercentFlag == (FLAG)FALSE)
                    BEGIN                                           /* AN000 */
                    PercentFlag = (FLAG)TRUE;                       /* AN000 */
                    /* Round down if > 999.9 */
                    if (large_input > u(999))                       /* AN000 */
                        large_input = (large_input%1000);           /* AN000 */
                    sprintf(&insert[prompt_location],"%3.0d%%",large_input);  /* AN000 */
                    /* Indicate that we are not using the default */
                    default_used = FALSE;                           /* AN000 */
                    display(input_message);                         /* AN000 */
                    END                                             /* AN000 */
                else
                    display(error_33);                              /* AN000 */

                break;                                              /* AN000 */

            case '0':                                                   /*C12*/
            case '1':                                                   /*C12*/
            case '2':                                                   /*C12*/
            case '3':                                                   /*C12*/
            case '4':                                                   /*C12*/
            case '5':                                                   /*C12*/
            case '6':                                                   /*C12*/
            case '7':                                                   /*C12*/
            case '8':                                                   /*C12*/
            case '9':                                                   /*C12*/
                if (enter_flag == TRUE)                                 /*C12*/
                    BEGIN                                               /*C12*/
                    large_input = u(0);                                 /*C12*/
                    default_used = FALSE;                               /*C12*/
                    PercentFlag = FALSE;                                /*C12*/
                    enter_flag = FALSE;                                 /*C12*/
                    END                                                 /*C12*/
                if ((PercentFlag == FALSE) && (large_input < 1000))     /*C12*/
                    BEGIN                                               /*C12*/
                    /* Add this digit in */                             /*C12*/
                    large_input = (large_input * 10) +                  /*C12*/
                                  (XFLOAT)(input - '0');                /*C12*/
                                                                        /*C12*/
                    /* Put it in the message */                         /*C12*/
                    number_in_msg((XFLOAT)large_input,prompt_location); /*C12*/
                    display(input_message);                             /*C12*/
                                                                        /*C12*/
                    /* Indicate that we are not using the default */    /*C12*/
                    default_used = FALSE;                               /*C12*/
                    PercentFlag = (FLAG)FALSE;                          /*C12*/
                    END                                                 /*C12*/
                else                                                    /*C12*/
                    display(error_33);                                  /*C12*/
                break;                                                  /*C12*/
                                                                        /*C12*/
            default:                                                    /*C12*/
                if ((PercentFlag == FALSE) && (large_input < 1000))     /*C12*/
                    BEGIN                                               /*C12*/
                    /* Setup error message */                           /*C12*/
                    insert[0] = c('0');                                 /*C12*/
                    insert[1] = c('-');                                 /*C12*/
                    insert[2] = c('9');                                 /*C12*/
                    display(error_31);                                  /*C12*/
                    END                                                 /*C12*/
                else                                                    /*C12*/
                    display(error_33);                                  /*C12*/

#if 0                                                                   /*C12*/
            default:
                BEGIN

                /* Make sure it is numerical input */

                if ( (isdigit(input)) && ((!PercentFlag) || (default_used)) )     /* AN000 */
                    BEGIN
                    /* Add this digit in */
                    very_big_input= (((unsigned long)(large_input)) * 10) + ((unsigned long)input - '0');   /* AC000 */

                    /* Round down if > 9999.9 */
                    large_input = ((unsigned)(very_big_input%10000));

                    /* Put it in the message */
                    number_in_msg((XFLOAT)large_input,prompt_location);  /* AN000 */
                    display(input_message);

                    /* Indicate that we are not using the default */
                    default_used = FALSE;
                    PercentFlag = (FLAG)FALSE;                      /* AN000 */
                    END
                else
                    BEGIN
                    if (!PercentFlag)                               /* AN000 */
                        BEGIN                                       /* AN000 */
                        /* Setup error message */
                        insert[0] = c('0');                         /* AC000 */
                        insert[1] = c('-');                         /* AC000 */
                        insert[2] = c('9');                         /* AC000 */
                        display(error_31);
                        END                                         /* AN000 */
                    else                                            /* AN000 */
                        BEGIN                                       /* AN000 */
                        display(error_33);                          /* AN000 */
                        END                                         /* AN000 */
                    END
                END
#endif                                                                  /*C12*/
            END
        END

    return(large_input);
END


/*  */
char get_alpha_input(low_letter,high_letter,row,col,error_low_letter,error_high_letter)

unsigned     row;
unsigned     col;
char         low_letter;
char         high_letter;
char         error_low_letter;
char         error_high_letter;

BEGIN

    char   input;
    char   default_used;
    char   input_value;

    char            attribute;
    char far        *attribute_ptr = &attribute;
    char far *input_ptr = &input;

    if (mono_flag == TRUE)
/*C09   attribute = GRAY_ON_BLACK;  */
        attribute = HIWHITE_ON_BLACK; /*C09*/
    else
/*C09   attribute = WHITE_ON_BLUE;  */
        attribute = HIWHITE_ON_BLUE;  /*C09*/

    /* Assume bad input */
    valid_input = FALSE;

    /* Init input_value to something non-alpha */
    input_value = c(0);                                                 /* AC000 */

    /* Loop until we get good stuff */
    while (valid_input == FALSE)
        BEGIN

        /* position the cursor */
        VIOSETCURPOS(row,col,u(0));                                     /* AC000 */

        /* Flush the keyboard buffer and get the next pressed key */
        input = get_char_input();
        input = dos_upper(input);

        /* Go handle different inputs */
        switch(input)
            BEGIN
            case ESC:
                BEGIN
                valid_input = TRUE;
                break;
                END

            case  CR:
                BEGIN
                /* See if it is digit and between given letters*/
                /* Do world trade get country information */
                input = dos_upper(input_value);                         /* AN000 */
                if ((isalpha(input))       &&
                    (input >= low_letter)  &&
                    (input <= high_letter) &&
                    (isalpha(input_value)))
                    BEGIN
                    valid_input = TRUE;
                    END
                else
                    BEGIN
                    if (isalpha(input_value))
                        BEGIN
                        /* Setup error message */
                        insert[0] = input;
                        insert[1] = error_low_letter;
                        insert[2] = c('-');                             /* AC000 */
                        insert[3] = error_high_letter;
                        display(error_23);
                        END
                    else
                        BEGIN
                        insert[0] = error_low_letter;
                        insert[1] = c('-');                             /* AC000 */
                        insert[2] = error_high_letter;
                        display(error_31);
                        END
                    END
                break;
                END

            default:
                BEGIN
                /* SR; 10/2/89; If Ctrl-C, display blank and beep */
/*C16           if ( input != 0x03 )                                             */
/*C16               VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0)); */
/*C16           else                                                             */
/*C16               {                                                            */
/*C16               DOSBEEP( 900, 400 );                                         */
/*C16               input = c(' ');                                              */
/*C16               VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0)); */
/*C16               }                                                            */
                if ((isalpha(input))       &&                           /*C16*/
                    (dos_upper(input) >= low_letter)  &&                /*C16*/
                    (dos_upper(input) <= high_letter))                  /*C16*/
                    clear_screen(u(23),u(0),u(23),u(79));               /*C16*/
                else                                                    /*C16*/
                    {                                                   /*C16*/
                    if (isalpha(input))                                 /*C16*/
                        {                                               /*C16*/
                        /* Setup error message */                       /*C16*/
                        insert[0] = input;                              /*C16*/
                        insert[1] = error_low_letter;                   /*C16*/
                        insert[2] = c('-');                             /*C16*/
                        insert[3] = error_high_letter;                  /*C16*/
                        display(error_23);                              /*C16*/
                        }                                               /*C16*/
                    else                                                /*C16*/
                        {                                               /*C16*/
                        insert[0] = error_low_letter;                   /*C16*/
                        insert[1] = c('-');                             /*C16*/
                        insert[2] = error_high_letter;                  /*C16*/
                        display(error_31);                              /*C16*/
                        }                                               /*C16*/
                    input = c(' ');                                     /*C16*/
                    }                                                   /*C16*/
                VIOWRTCHARSTRATT(input_ptr,u(1),row,col,attribute_ptr,u(0)); /*C16*/
                default_used = FALSE;
                input_value = input;
                break;
                END
            END
        END
    return(input);
END


/*  */
char    get_char_input()

BEGIN
    regs.h.ah = uc(0x0C);                                           /* AC000 */
    regs.h.al = uc(0x07);                                           /* AC000 */
    intdos(&regs,&regs);
    if (regs.h.al == uc(0))                                         /* AC000 */
        BEGIN
        DOSBEEP(u(900),u(400));                                     /* AC000 */
        END
    return(((char)(regs.h.al)));

END



/*  */                                                                 /* AN000 */
                                                                        /* AN000 */
void get_string_input(StartRow,StartCol,string_ptr)                     /* AN000 */
                                                                        /* AN000 */
unsigned        StartRow;                                               /* AN000 */
unsigned        StartCol;                                               /* AN000 */
char far        *string_ptr;                                            /* AN000 */
                                                                        /* AN000 */
BEGIN                                                                   /* AN000 */

    #define MAX_STRING_INPUT_LENGTH 11
                                                                        /* AN000 */
    unsigned char   input;                                              /* AN000 */
/*C00     char   input_value; */                                                    /* AN000 */
    char   far *WorkingPtr;                                             /* AN000 */
    char   far *DeletePtr;                                              /* AN000 */
    char   Done;                                                        /* AN000 */
    unsigned        Row;                                                /* AN000 */
    unsigned        Col;                                                /* AN000 */
/*C00     int             i;  */                                                    /* AN000 */
                                                                        /* AN000 */

    char            attribute;
    char far        *attribute_ptr = &attribute;
    char far *input_ptr = &input;

    if (mono_flag == TRUE)
/*C09   attribute = GRAY_ON_BLACK;  */
        attribute = HIWHITE_ON_BLACK; /*C09*/
    else
/*C09   attribute = WHITE_ON_BLUE;  */
        attribute = HIWHITE_ON_BLUE;  /*C09*/

                                                                        /* AN000 */
    WorkingPtr = string_ptr;                                            /* AN000 */

    Row = StartRow;                                                     /* AN000 */
    Col = StartCol;                                                     /* AN000 */
    VIOSETCURPOS(Row,Col,u(0));                                         /* AN000 */

    while(*WorkingPtr != c(NUL))                                        /* AN000 */
        BEGIN                                                           /* AN000 */
        VIOWRTCHARSTRATT(WorkingPtr,u(1),Row,Col,attribute_ptr,u(0));
        WorkingPtr++;
        Col++;                                                          /* AN000 */
        VIOSETCURPOS(Row,Col,u(0));                                     /* AN000 */
        END                                                             /* AN000 */

    regs.h.ah = uc(12);                                                 /* AN000 */
/*C03    regs.h.al = uc(8); */                                                 /* AN000 */
    regs.h.al = uc(7); /*C03*/
    intdos(&regs,&regs);                                                /* AN000 */
    input = regs.h.al;                                                  /* AN000 */

    /* Loop until we get good stuff */                                  /* AN000 */
    Done = FALSE;                                                       /* AN000 */
    while (!Done)                                                       /* AN000 */
        BEGIN                                                           /* AN000 */

        /* Go handle different inputs */

        if (input < 32)                                                 /* AN000 */
            BEGIN                                                       /* AN000 */
            switch(input)                                               /* AN000 */
                BEGIN                                                   /* AN000 */
                case ESC:                                               /* AN000 */
                    Done=TRUE;                                          /* AN000 */
                    *string_ptr++ = c(ESC);                             /* AN000 */
                    *string_ptr++ = c('\0');                            /* AN000 */
                    break;                                              /* AN000 */

                case  CR:                                               /* AN000 */
                    Done=TRUE;                                          /* AN000 */
                    break;                                              /* AN000 */

                case 8: /* backspace */                                 /* AN000 */
                    if (Col > StartCol)                                 /* AN000 */
                        BEGIN                                           /* AN000 */
                        WorkingPtr--;                                   /* AN000 */
                        Col--;                                          /* AN000 */
                        VIOSETCURPOS(Row,Col,u(0));                     /* AN000 */
							  input = c(' ');
                        VIOWRTCHARSTRATT(input_ptr,u(1),Row,Col,attribute_ptr,u(0));
                        VIOSETCURPOS(Row,Col,u(0));                     /* AN000 */
                        DeletePtr = WorkingPtr;                         /* AN000 */
                        while ( *(DeletePtr+1) != c('\0')  )            /* AN000 */
                            BEGIN                                       /* AN000 */
                            *DeletePtr = *(DeletePtr+1);                /* AN000 */
                            VIOWRTCHARSTRATT(DeletePtr,u(1),Row,Col,attribute_ptr,u(0));
                            DeletePtr++;                                /* AN000 */
                            END                                         /* AN000 */
                        *DeletePtr = c('\0');                           /* AN000 */
							  input = c(' ');
                        VIOWRTCHARSTRATT(input_ptr,u(1),Row,Col+1,attribute_ptr,u(0));
                        VIOSETCURPOS(Row,Col,u(0));                     /* AN000 */
                        END                                             /* AN000 */
                    else DOSBEEP(u(900),u(400));                        /* AN000 */
                break;                                                  /* AN000 */

                case  0:                                                /* AN000 */
/*C03                    regs.h.ah = uc(0x08); */                              /* AN000 */
                    regs.h.ah = uc(0x07); /*C03*/
                    intdos(&regs,&regs);                                /* AN000 */
                    input = regs.h.al;                                  /* AN000 */
                    switch(input)                                       /* AN000 */
                        BEGIN                                           /* AN000 */
                        case 71: /* HOME */                             /* AN000 */
                            WorkingPtr = string_ptr;                    /* AN000 */
                            Row = StartRow;                             /* AN000 */
                            Col = StartCol;                             /* AN000 */
                            VIOSETCURPOS(Row,Col,u(0));                 /* AN000 */
                            break;                                      /* AN000 */

                        case 79: /* END  */                             /* AN000 */
                            while (*WorkingPtr != c('\0') )             /* AN000 */
                                BEGIN                                   /* AN000 */
                                WorkingPtr++;                           /* AN000 */
                                Col++;                                  /* AN000 */
                                VIOSETCURPOS(Row,Col,u(0));             /* AN000 */
                                END                                     /* AN000 */
                            break;                                      /* AN000 */


                        case 75: /* Cursor Left */                      /* AN000 */
                            if (Col > StartCol)                         /* AN000 */
                                BEGIN                                   /* AN000 */
                                WorkingPtr--;                           /* AN000 */
                                Col--;                                  /* AN000 */
                                VIOSETCURPOS(Row,Col,u(0));             /* AN000 */
                                END                                     /* AN000 */
                            else DOSBEEP(u(900),u(400));                /* AN000 */
                            break;                                      /* AN000 */


                        case 77: /* Cursor Right */                     /* AN000 */
                            if ( *WorkingPtr != c('\0') )               /* AN000 */
                                BEGIN                                   /* AN000 */
                                WorkingPtr++;                           /* AN000 */
                                Col++;                                  /* AN000 */
                                VIOSETCURPOS(Row,Col,u(0));             /* AN000 */
                                END                                     /* AN000 */
                            else DOSBEEP(u(900),u(400));                /* AN000 */
                            break;                                      /* AN000 */


                        default:                                        /* AN000 */
                            DOSBEEP(u(900),u(400));                     /* AN000 */
                            break;                                      /* AN000 */

                        END                                             /* AN000 */
                    break;                                              /* AN000 */

                default:                                                /* AN000 */
                        DOSBEEP(u(900),u(400));                         /* AN000 */
                        break;                                          /* AN000 */
                END                                                     /* AN000 */

            END                                                         /* AN000 */
         else     /* input is >= 32 */                                  /* AN000 */
            BEGIN                                                       /* AN000 */
            input = dos_upper(input);                                   /* AN000 */
            if ( (strchr(".\"/\\[]:|<>+=;,",input) == NULL) &&
                 (Col < StartCol + MAX_STRING_INPUT_LENGTH)    )        /* AN000 */
                BEGIN                                                   /* AN000 */
                /* SR; 10/2/89; If Ctrl-C, display blank and beep */
                if ( input != 0x03 )
                    VIOWRTCHARSTRATT(input_ptr,u(1),Row,Col,attribute_ptr,u(0));
                else
                    {
                    DOSBEEP( 900, 400 );
                    input = c(' ');
                    VIOWRTCHARSTRATT(input_ptr,u(1),Row,Col,attribute_ptr,u(0));
                    }
                *WorkingPtr = input;                                    /* AN000 */
                *(WorkingPtr+1) = c('\0');                              /* AN000 */
                if (Col < (StartCol + MAX_STRING_INPUT_LENGTH - 1) )    /* AN000 */
                    BEGIN                                               /* AN000 */
                    Col++;                                              /* AN000 */
                    WorkingPtr++;                                       /* AN000 */
                    END                                                 /* AN000 */
                VIOSETCURPOS(Row,Col,u(0));                             /* AN000 */
                END                                                     /* AN000 */
            else DOSBEEP(u(900),u(400));                                /* AN000 */
            END                                                         /* AN000 */

        if (!Done)                                                      /* AN000 */
            BEGIN                                                       /* AN000 */
            /* Get a character */                                       /* AN000 */
/*C03            regs.h.ah = uc(0x08); */                                    /* AN000 */
            regs.h.ah = uc(0x07); /*C03*/
            intdos(&regs,&regs);                                        /* AN000 */
            input = regs.h.al;                                          /* AN000 */
            END                                                         /* AN000 */
        END                                                             /* AN000 */

    return;                                                             /* AN000 */
END                                                                     /* AN000 */
