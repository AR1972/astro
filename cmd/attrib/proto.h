;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
WORD inmain(char *line);                               //M002 Change return type
WORD main(char *line);                                 //M002
void Display_msg(int msgnum,int msghan,int msgparms,int *msgsub,char msginput);
void Get_far_str(char *target,unsigned long *source,unsigned short length);
unsigned short *Dallocate(unsigned short s);
void Copy_far_ptr(unsigned long *p1_addr,unsigned short *p2_addr);
unsigned short Parse_it(char *line);
unsigned short Make_fspec(char *fspec);
void Dta_save(char *t,unsigned int l);
void Dta_restore(char *t,unsigned int l);
unsigned short Find_first(char *s,char *f,unsigned short *a,unsigned short *r);
unsigned short Find_next(char *f,unsigned short *r);
unsigned short Get_reg_attrib(char *fspec,unsigned char *attr_byte);
unsigned short Ext_open(char *fspec,unsigned short *handle);
unsigned short Set_reg_attrib(char *fspec,unsigned char attr_byte);
unsigned short CheckYN(char *fspec);
void Convert_date(unsigned short dosdate,unsigned short *msgdate1,unsigned short *msgdate2);
void Convert_time(unsigned short dostime,unsigned short *msgtime1,unsigned short *msgtime2);
unsigned short Regular_attrib(char *fspec);
unsigned short Special_attrib(unsigned short handle,char *fspec,unsigned short id);
unsigned short Attrib(char *path,char *file);
unsigned short Do_dir(char *path,char *file);
void Check_appendx(void );
void Reset_appendx(void );
unsigned short Check_DBCS(char *array,unsigned short position,char character);
void Get_DBCS_vector(void );
void Error_exit(int msg_class,int ext_err_num,int subcnt);
void Parse_err(unsigned short error_num);
void far _interrupt ctl_break_handler(void );
void sysloadmsg(union REGS *inregs, union REGS *outregs);
void sysdispmsg(union REGS *inregs, union REGS *outregs);
void Dexit(WORD s);
void Dfree(WORD segment);
void putpspbyte(unsigned, char);
char getpspbyte(unsigned);
void parse(union REGS *inregs, union REGS *outregs);
int crit_err_handler();                                          
