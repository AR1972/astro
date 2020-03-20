#define chYES 'y'
#define chNO 'n'
char *apszHelp[] = {
"Deletes a directory and all the subdirectories and files within it.",
"",
"To Delete one or more directories:",
"DELTREE [/Y] [drive:]path [[drive:]path[...]]",
"",
"  /Y              Suppresses prompting to confirm whether you want to",
"                  delete the subdirectory.",
"  [drive:]path    Specifies the name of the directory you want to delete.",
"",
"Note: Use DELTREE with caution. Every file and subdirectory within the",
"specified directory will be deleted.",
""
};

char pszMsgDelFilePrompt[] = "Delete file \"%s\"? (Y/N) ";
char pszMsgDelDirPrompt[] = "Delete directory \"%s\" and all its subdirectories? (Y/N) ";
char pszMsgDeleting[] = "Deleting %s...\n";
char pszMsgNoParameters[] = "Required parameter missing\n";
char pszMsgInvalidSwitch[] = "Switch \"%s\" is not valid\n";
char pszMsgInvalidForamt[] = "Parameter format incorrect\n";
char pszMsgDirNotFound[] = "Directory not found - %s\n";