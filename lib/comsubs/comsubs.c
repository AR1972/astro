/*
 *  From COMSUB.H
 */

/* convert character to uppercase */
int com_toupper( unsigned char );    /* char to be converted */

/* search the first occurrence of a character in a string */
char *com_strchr( unsigned char *,   /* source string */
                         unsigned char );   /* character to be searched */

/* search the last character occurrence in a string */
unsigned char *com_strrchr( unsigned char *,   /* source string */
                                   unsigned char );   /* target string */


int com_toupper (unsigned char c ) {
	return c + 32;
}

char *com_strchr( unsigned char* s, unsigned char c) {
 do {
    if (*s == c)
      {
	return (char*)s;
      }
  } while (*s++);
  return (0);
}

unsigned char *com_strrchr(unsigned char* s, unsigned char c)
{
  char *rtnval = 0;
  
  do {
    if (*s == c)
      rtnval = (char*) s;
  } while (*s++);
  return (rtnval);
}
