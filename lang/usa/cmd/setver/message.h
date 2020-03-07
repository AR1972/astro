#pragma pack(2)
char SuccessMsg[] = "\r\nVersion table successfully updated";
char SuccessMsg2[] = "The version change will take effect the next time you restart your system";
char szMiniHelp[] = "       Use \"SETVER /?\" for help";
char szTableEmpty[] = "No entries found in version table";

char *ErrorMsg[] = {
	"\r\nERROR: ",
	"Invalid switch.",
	"Invalid filename.",
	"Insuffient memory.",
	"Invalid version number, format must be 2.11 - 9.99.",
	"Specified entry was not found in the version table.",
	"Could not find the file SETVER.EXE.",
	"Invalid drive specifier.",
	"Too many command line parameters.",
	"Missing parameter.",
	"Reading SETVER.EXE file.",
	"Version table is corrupt.",
	"The SETVER file in the specified path is not a compatible version.",
	"There is no more space in version table new entries.",
	"Writing SETVER.EXE file.An invalid path to SETVER.EXE was specified.",
	NULL
};

char *Help[] = {
	"Sets the version number that MS-DOS reports to a program.\r\n",
	"Display current version table:  SETVER [drive:path]",
	"Add entry:                      SETVER [drive:path] filename n.nn",
	"Delete entry:                   SETVER [drive:path] filename /DELETE [/QUIET]\r\n",
	"  [drive:path]    Specifies location of the SETVER.EXE file.",
	"  filename        Specifies the filename of the program.",
	"  n.nn            Specifies the MS-DOS version to be reported to the program.",
	"  /DELETE or /D   Deletes the version-table entry for the specified program.",
	"  /QUIET          Hides the message typically displayed during deletion of",
	"                  version-table entry.",
	NULL
};

char *Warn[] = {
	"\nWARNING - Contact your software vendor for information about whether a",
	"specific program works with MS-DOS version 6.0. It is possible that",
	"Microsoft has not verified whether the program will successfully run if",
	"you use the SETVER command to change the program version number and",
	"version table. If you run the program after changing the version table",
	"in MS-DOS version 6.0, you may lose or corrupt data or introduce system",
	"instabilities. Microsoft is not responsible for any loss or damage, or",
	"for lost or corrupted data.",
	NULL
};

char *szNoLoadMsg[] = {
	"NOTE: SETVER device not loaded. To activate SETVER version reporting",
	"      you must load the SETVER.EXE device in your CONFIG.SYS.",
	NULL
};