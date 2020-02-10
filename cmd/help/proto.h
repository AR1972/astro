extern  int main(int argc,char * *argv);
extern  void help_on_help(void );
extern  int dump_list(void );
extern  struct _iobuf *find_help_file(void );
extern  int get_next_record(struct _iobuf *help_handle,char *rec_buf);
extern  void send_record(char *rec_buf,int out_con, int screen_height);
extern  int check_console(struct _iobuf *fp);
extern  int dump_command(char *name);
extern  int lookup_name(char *name,struct _iobuf *help_handle);
extern  void dos_toupper(char *string);
extern  void get_ucase_tab(void );
extern  char make_upper(char );
extern  int get_screen_height(void );
extern  int is_internal(char *name); 
