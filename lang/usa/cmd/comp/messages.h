//
#define YES              'Y'
#define NO               'N'
#define DECIMAL_SWITCH   'D'
#define ASCII_SWITCH     'A'
#define NO_CASE_SWITCH   'C'
#define LIMIT_SWITCH     'N'
#define LINE_CT_SWITCH   'L'
#define SWITCH_CHAR_1    '/'
#define SWITCH_CHAR_2    '-'
#define PPARG_MSG_OFF      8
#define USER_HELP_LINES   11
#define FAILURE            1
//messages
#define SUCCESS            0
#define NO_MEM_AVAIL       1
#define CANT_OPEN_FILE     2
#define CANT_READ_FILE     3
#define SYNT_ERR           4
#define BAD_NUMERIC_ARG    5
#define COMP_ERR           6
#define COMP2_ERR          7
#define FILE1              8
#define FILE2             11
#define OPTION_REQUEST    12
#define COMPARING         13
#define AND               14
#define ELLIPSES          15
#define DIFFERENT_SIZES   16
#define NEED_DELIM_CHAR   17
#define TOO_MANY_ARGS     18
#define COMPARE_MORE      19
#define COULD_NOT_EXP     20
#define USER_HELP1        21
#define TEN_MISM          32
#define INCORRECT_DOS_VER 33
#define UNEXP_EOF         34
#define INV_SWITCH        35
#define FILE1_LINES       36
#define FILE2_LINES       37
//
char *msg_tbl[] = {
"Files compare OK\n",
"No memory available\n",
"Can't find/open file:  ",
"Can't read file:  ",
"Bad command line syntax\n",
"Bad numeric argument : ",
"Compare error at OFFSET ",
"Compare error at LINE ",
"file1 = ",
"Name of first file to compare: ",
"Name of second file to compare: ",
"file2 = ",
"Option : ",
"Comparing ",
" and ",
"...",
"Files are different sizes\n",
"Format for /n switch is /n=XXXX",
"Too many command-line arguments",
"Compare more files (Y/N) ? ",
"Could not expand second filename so as to match first",
"Compares the contents of two files or sets of files.\n",
"COMP [data1] [data2] [/D] [/A] [/L] [/N=number] [/C]\n",
"  data1     Specifies location and name(s) of first file(s) to compare.",
"  data2     Specifies location and name(s) of second files to compare.",
"  /D        Displays differences in decimal format. This is the default",
"            setting.",
"  /A        Displays differences in ASCII characters.",
"  /L        Displays line numbers for differences.",
"  /N=number Compares only the first specified number of lines in each file.",
"  /C        Disregards case of ASCII letters when comparing files.\n",
"To compare sets of files, use wildcards in data1 and data2 parameters.",
"10 Mismatches - ending compare",
"Incorrect DOS version",
"Unexpected End of File",
"Invalid switch - \r\n",
"File1 Only Has %d Lines\n",
"\r\nFile2 Only Has %d Lines\n"
};

