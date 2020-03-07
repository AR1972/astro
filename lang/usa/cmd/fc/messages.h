char BadSw[] = "Incompatible switches";
char Bad_ver[] = "Incorrect DOS version";
char BadOpn[] = "cannot open %s - %s";
char LngFil[] = "%s longer than %s";
char NoDif[] = "no differences encountered";
char NoMem[] = "out of memory\r";
char ReSyncMes[] = "Resync failed.  Files are too different";
char UknownErr[] = "Unknown error";

char *msg_tbl[] = {
	"File(s) not found : ",
	"Comparing files ",
	" and ",
	"Too many command-line arguments",
	"Could not expand second filename so as to match first",
	"Too many filenames\n",
	"Invalid switch\n",
	"Insufficient number of filespecs"
};
char *hlp_tbl[] = {
	"Compares two files or sets of files and displays the differences between\n",
	"them.\n\n",
	"FC [/A] [/C] [/L] [/LBn] [/N] [/T] [/W] [/nnnn] [drive1:][path1]filename1\n",
	"  [drive2:][path2]filename2\n",
	"FC /B [drive1:][path1]filename1 [drive2:][path2]filename2\n\n",
	"  /A     Displays only first and last lines for each set of differences.\n",
	"  /B     Performs a binary comparison.\n",
	"  /C     Disregards the case of letters.\n",
	"  /L     Compares files as ASCII text.\n",
	"  /LBn   Sets the maximum consecutive mismatches to the specified number of\n",
	"         lines.\n",
	"  /N     Displays the line numbers on an ASCII comparison.\n",
	"  /T     Does not expand tabs to spaces.\n",
	"  /W     Compresses white space (tabs and spaces) for comparison.\n",
	"  /nnnn  Specifies the number of consecutive lines that must match after a\n",
	"         mismatch.\n",
};
enum msg{
	FILES_NOT_FOUND_MSG,
	COMPARING,
	AND,
	FAILURE,
	COULD_NOT_EXP_MSG,
	TOO_MANY_FNAMES,
	INVALID_SWITCH,
	NOT_ENUF_FILES
};

int HELP_TEXT_LEN = sizeof( hlp_tbl ) / sizeof( hlp_tbl[ 0 ] ) - 1;
