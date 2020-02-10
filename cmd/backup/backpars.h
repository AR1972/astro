/*************************************************************************/
/*                                                                       */
/*  FILE:    Backpars.H                                                  */
/*                                                                       */
/*  PURPOSE: Defines the structures for the DOS PARSE service            */
/*           routines.                                                   */
/*                                                                       */
/*  LABEL:   Microsoft Confidential                                      */
/*           Copyright (c) Microsoft Corporation 1991                    */
/*           All Rights Reserved.                                        */
/*                                                                       */
/*************************************************************************/


/*
 *  Structure to define additional command line delimiters.
 */
struct p_parms
{
    WORD  parmsx_ptr;             /* pointer to parms structure */           
    BYTE  p_num_extra;            /* 1 says that a delimiter list follows */ 
    BYTE  p_len_extra_delim;      /* number of additional delimiters */      
    BYTE  p_extra_delim[2];       /* additional delimiters */                
};

/*
 *  Structure to define backup syntax requirements.
 */
struct p_parmsx
{
     BYTE  p_minpos;              /* there are 2 required positional parms */
     BYTE  p_maxpos;              /* there are 2 required positional parms */
     WORD  pos1_ptr;              /* pointer to source filespec def area */
     WORD  pos2_ptr;              /* pointer to target drive def area */
     BYTE  num_sw;                /* there are 7 switches (/S, /F, /M, /A, /L:, /T:, /D:) */
     WORD  sw1_ptr;               /* pointer to first switch definition area */
     WORD  sw2_ptr;               /* pointer to second switch definition area */
     WORD  sw3_ptr;               /* pointer to third switch definition area */
     WORD  sw4_ptr;               /* pointer to fourth switch definition area */
     WORD  sw5_ptr;               /* pointer to fifth switch definition area */
     WORD  sw6_ptr;               /* pointer to sixth switch definition area */
     WORD  sw7_ptr;               /* pointer to seventh switch definition area */
     WORD  num_keywords;          /* number of keywords in backup syntax */
};

/*
 *  Structure to define positional parms.
 */
struct p_pos_blk
{
    WORD  match_flag;             /* controls type matched */
    WORD  function_flag;          /* function should be taken */
    WORD  result_buf;             /* result buffer address */
    WORD  value_list;             /* value list address */
    BYTE  nid;                    /* # of keyword/SW synonyms (0) */
};

/*
 *  Structure to define switches.
 */
struct p_sw_blk
{
    WORD  p_match_flag;           /* controls type matched */
    WORD  p_function_flag;        /* function should be taken */
    WORD  p_result_buf;           /* result buffer address */
    WORD  p_value_list;           /* value list address */
    BYTE  p_nid;                  /* # of switches */
    BYTE  switch1[3];             /* save area for switch */
    BYTE  switch2[3];             /* save area for switch */
    BYTE  switch3[3];             /* save area for switch */
    BYTE  switch4[3];             /* save area for switch */
};

/*
 *  Structure to define switches.
 */
struct p_sw7_blk
{
    WORD  p_match_flag;           /* controls type matched */
    WORD  p_function_flag;        /* function should be taken */
    WORD  p_result_buf;           /* result buffer address */
    WORD  p_value_list;           /* value list address */
    BYTE  p_nid;                  /* # of switches */
    BYTE  switch7[3];             /* save area for switch */
};


/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

/*
 *  Return buffer for time.
 */
struct timebuff
{
    BYTE  t_type;                 /* type returned */  
    BYTE  t_item_tag;             /* space for item tag */
    WORD  t_synonym_ptr;          /* pointer to Synonym list returned */
    BYTE  hours;
    BYTE  minutes;
    BYTE  seconds;
    BYTE  hundreds;
};

/*
 *  Return buffer for date.
 */
struct datebuff
{
    BYTE  d_type;                 /* type returned */  
    BYTE  d_item_tag;             /* space for item tag */
    WORD  d_synonym_ptr;          /* pointer to Synonym list returned */
    WORD  year;
    BYTE  month;
    BYTE  day;
};

/*
 *  Return buffer for positional parameters.
 */
struct p_result_blk
{
    BYTE  p_type;                 /* type returned */
    BYTE  p_item_tag;             /* matched item tag */
    WORD  p_synonym_ptr;          /* pointer to Synonym list returned */
    DWORD p_string_ptr;           /* pointer to string */
};

/*
 *  Return buffer for switch information. 
 */
struct	switchbuff
{
    BYTE  sw_type;                /* type returned */
    BYTE  sw_item_tag;            /* matched item tag */
    WORD  sw_synonym_ptr;         /* pointer to synonym */
    DWORD sw_string_ptr;          /* pointer to string */
};


/*
 *  Value list for /F: parameter.
 */
struct val_list_struct
{
    BYTE  nval;
    BYTE  num_ranges;
    BYTE  num_choices;
    BYTE  num_strings;
    BYTE  item_tag01;
    WORD  val01;
    BYTE  item_tag02;
    WORD  val02;
    BYTE  item_tag03;
    WORD  val03;
    BYTE  item_tag04;
    WORD  val04;     
    BYTE  item_tag05;
    WORD  val05;     
    BYTE  item_tag06;
    WORD  val06;     
    BYTE  item_tag07;
    WORD  val07;     
    BYTE  item_tag08;
    WORD  val08;     
    BYTE  item_tag09;
    WORD  val09;     
    BYTE  item_tag10;
    WORD  val10;     
    BYTE  item_tag11;
    WORD  val11;     
    BYTE  item_tag12;
    WORD  val12;     
    BYTE  item_tag13;
    WORD  val13;     
    BYTE  item_tag14;
    WORD  val14;     
    BYTE  item_tag15;
    WORD  val15;     
    BYTE  item_tag16;
    WORD  val16;     
    BYTE  item_tag17;
    WORD  val17;     
    BYTE  item_tag18;
    WORD  val18;     
    BYTE  item_tag19;
    WORD  val19;     
    BYTE  item_tag20;
    WORD  val20;     
    BYTE  item_tag21;
    WORD  val21;     
    BYTE  item_tag22;
    WORD  val22;     
    BYTE  item_tag23;
    WORD  val23;     
    BYTE  item_tag24;
    WORD  val24;     
    BYTE  item_tag25;
    WORD  val25;     
    BYTE  item_tag26;
    WORD  val26;     
    BYTE  item_tag27;
    WORD  val27;     
    BYTE  item_tag28;             /* M007 Start */
    WORD  val28;
    BYTE  item_tag29;
    WORD  val29;
    BYTE  item_tag30;
    WORD  val30;
    BYTE  item_tag31;
    WORD  val31;
    BYTE  item_tag32;
    WORD  val32;
    BYTE  item_tag33;
    WORD  val33;                  /* M007 End */
};

/*
 *  Value table for /F: parameter. 
 */
struct val_table_struct
{
    BYTE  val01[7];
    BYTE  val02[7];
    BYTE  val03[7];
    BYTE  val04[7];
    BYTE  val05[7];
    BYTE  val06[7];
    BYTE  val07[7];
    BYTE  val08[7];
    BYTE  val09[7];
    BYTE  val10[7];
    BYTE  val11[7];
    BYTE  val12[7];
    BYTE  val13[7];
    BYTE  val14[7];
    BYTE  val15[7];
    BYTE  val16[7];
    BYTE  val17[7];
    BYTE  val18[7];
    BYTE  val19[7];
    BYTE  val20[7];
    BYTE  val21[7];
    BYTE  val22[7];
    BYTE  val23[7];
    BYTE  val24[7];
    BYTE  val25[7];
    BYTE  val26[7];
    BYTE  val27[7];
    BYTE  val28[7];               /* M007 Start */
    BYTE  val29[7];
    BYTE  val30[7];
    BYTE  val31[7];
    BYTE  val32[7];
    BYTE  val33[7];               /* M007 End */
};

