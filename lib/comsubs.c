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

//int main(int argc, char ** argv);

int com_toupper (unsigned char ch ) {
	return('A');
}

char *com_strchr( unsigned char* str, unsigned char ch) {
	char *retval = "abcdef";
	return(retval);
}

unsigned char *com_strrchr(unsigned char* str, unsigned char ch) {
	unsigned char* retval = "abcdef";
	return(retval);
}
/*
int main(int argc, char ** argv) {
	
	return(0);

}
*/