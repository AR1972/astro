#define YES 'Y'
#define NO 'N'

char *msg_tbl[] = {
	"",
	"Incorrect DOS version\r\n",
	"Cannot label a JOINed, SUBSTed or ASSIGNed drive\r\n",
	"Invalid characters in volume label\r\n",
	" has no label",
	"Volume in drive ",
	" is ",
	"Volume label (11 characters, ENTER for none)? ",
	"Volume Serial Number is ",
	"-",
	"Delete current volume label (Y/N)? ",
	"Creates, changes, or deletes the volume label of a disk.\r\n",
	"LABEL [drive:][label]\r\n",
	"Cannot label a network drive",
	"Invalid drive specification",
	"Incorrect drive syntax - ",
	"Cannot make directory entry",
	"Unexpected End of File",
	"Multiple drive letters specified",
	"Too many characters in volume label\r\n",
	"Drive letter cannot be inside volume label"
};

enum{
	BAD_DOS_VERSION = 1,
	TRANSLATED_DRIVE,
	LABEL_SYNTAX_ERR,
	HAS_NO_LABEL,
	VOL_IN_DRIVE,
	IS,
	ENTER_LABEL,
	VOL_SER,
	DASH,
	DEL_CUR_VOL,
	HELP_MSG_1,
	HELP_MSG_2,
	REM_MEDIA,
	INVALID_DRIVE,
	INVALID_DRIVE_SYNTAX,
	TOO_MANY_FILES,
	UNEXP_EOF,
	MULTIPLE_DRIVE,
	LABEL_TOO_LONG,
	DRIVE_IN_LABEL
};