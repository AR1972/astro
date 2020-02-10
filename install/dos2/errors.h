enum	Errors	  { ERR_OPENING, ERR_READING, ERR_WRITING, ERR_RENAMING,
						 ERR_DELETING, ERR_CREATING, ERR_PROCESSING	};


void ProcessCopyError( char *szFile, int ErrorType );
