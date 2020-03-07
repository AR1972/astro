char pszHELP_SWITCH[] = "/?";
char pszINSERT_NEW_DISK[] = "Insufficient disk space.  Insert a new disk in drive %c: and press ENTER: ";
char pszNO_DELETE_OLD[] = "Error - Cannot delete %s.";
char pszNOT_ENOUGH_DISK[] = "Error - Insufficient disk space on drive %c:";
char pszNO_OPEN_INPUT[] = "Error - Can't open input file: %s";
char pszNO_READ_INPUT[] = "Error - Can't read from input file: %s";
char pszSOURCE_IS_DEST[] = "Error - Input file '%s' cannot be expanded onto itself.";
char pszNO_OPEN_OUTPUT[] = "Error - Can't open output file: %s";
char pszINPUT_NOT_COMPRESSED_FMT[] = "Input file '%s' already in expanded format.";
char pszPROGRESS[] = "%s  ->  %s";
char pszNO_WRITE_OUTPUT[] = "Error - Can't write to output file: %s";
char pszFORMAT_ERROR[] = "Error in compressed input file format: %s";
char pszOUT_OF_SPACE[] = "Error - Out of disk space while writing output file: %s";
char pszNO_COPY_TIME[] = "Error - Can't copy date and time stamp ";
char pszFROM_INPUT[] = "from input file: %s";
char pszTO_OUTPUT[] = "to output file: %s";
char pszSRC_PROMPT1[] = "Type the location and name of the\n"\
                        "compressed file you want to expand.\n"\
						"(Example: A:\EGA.SY_)\n\n";
char pszSRC_PROMPT2[] = "Compressed file: ";
char pszDEST_PROMPT1[] = "Type the location and/or name you\n"\
                         "want to give the expanded file.\n"\
						 "(Example: C:\DOS\EGA.SYS)\n\n";
char pszDEST_PROMPT2[] = "Expanded file: ";
char pszNO_DEST_DIR[] = "Error - Destination directory does not exist: %s";
char pszNOT_ENOUGH_MEM[] = "Error - Insufficient memory for buffers.";
char pszONE_FILE[] = "        %d file expanded";
char pszTOTAL_FILES[] = "        %d files expanded";
char pszHELP_TEXT1[] = "Expands one or more compressed files.\n\n";
char pszHELP_TEXT2[] = "EXPAND [drive:][path]filename [[drive:][path]filename [...]] destination\n\n";
char pszHELP_TEXT3[] = "  [drive:][path]filename  Specifies the location and/or name of a file"\
                       "                          or set of files to be expanded. You cannot use\n"\
                       "                          wildcards.\n"\
                       "  destination             Specifies the new location and/or name of an\n"\
                       "                          expanded file or set of files. Destination can\n"\
                       "                          be a drive letter and colon, directory name,\n"\
                       "                          filename, or combination.\n\n";
char pszHELP_TEXT4[] = "The destination can only be a filename if you have specified a single\n"\
                       "filename for the source filename parameter. To expand a file or set\n"\
                       "of files to a different directory and keep the original filename(s),\n"\
                       "specify only a directory as the destination.";
char pszINVALID[] = "Error - Invalid input parameters.";