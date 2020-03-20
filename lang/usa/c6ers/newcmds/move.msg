#define chYES 'y'
#define chNO 'n'
char *apszHelp[] = {
"Moves files and renames files and directories.",
"",
"To move one or more files:",
"MOVE [/Y] [drive:][path]filename1[,...] destination",
"",
"  /Y              Suppresses prompting to confirm creation of a directory.",
"",
"  [drive:][path]filename1 Specifies the location and name of the file",
"                          or files you want to move.",
"  destination             Specifies the new location of the file. Destination",
"                          can consist of a drive letter and colon, a directory",
"                          name, or a combination. If you are moving only one",
"                          file, you can also include a filename if you want",
"                          to rename the file when you move it.",
"",
"To rename a directory:",
"MOVE [drive:][path]dirname1 dirname2",
"",
"  [drive:][path]dirname1  Specifies the directory you want to rename.",
"  dirname2                Specifies the new name of the directory.",
"",
"Note: If the destination you specify is an existing file, MOVE overwrites",
"      that file with the file you are moving."
};

char pszErrCannotMove[] = "Cannot move %s to %s: %s\n";
char pszErrCannotPath[] = "Cannot create path to %s\n";
char pszErrCannotMoveMultiple[] = "Cannot move multiple files to a single file\n";
char pszMsgOK[] = "[OK]\n";
char pszMsgSrcEqualsDst[] = "Source and destination are the same, %s\n";
char pszMsgNoParameters[] = "Required parameter missing\n";
char pszMsgInvalidSwitch[] = "Switch \"%s\" is not valid\n";
char pszMsgTooManyParameters[] = "Too many parameters\n";
char pszMsgMakeDirPrompt[] = "Make directory \"%s\"? (Y/N) ";
