/***
* $Workfile:   boxfunc.h  $
* $Revision:   1.3  $
*   $Author:   Dave Sewell  $
*     $Date:   04 May 1990  9:13:02  $
***/

extern void _cdecl reset_disk(void);
extern void make_server_id(void);
extern int bios_handler(word count);
extern int prn_write_handler(word count);
extern int prn_cmd_handler(word count);
extern void _pascal hook_int2f(void);
extern void _pascal unhook_int2f(void);
extern int _fastcall is_il_drive(unsigned drive_num);
/* File calldvr.asm */
extern void pascal call_driver(void far *rhp, struct device_header far *header);
/* File crctab.asm */
/* File setcerr.asm */
extern void pascal set_critical_error(void);
/* File criterr.c */ 
extern  int pascal criterr(int ax,int err_code,struct device_header far *driver);
/* File devinfo.c */ 
extern void _pascal read_volume_label(int dev);
extern  void load_device_information(void );
extern  struct device_header far *find_char_device(char *name);
extern  int open_file(int dev, char *filename);
/* File handler.c */ 
extern  void communication_error(char value);
extern  void master_id_error(char *handler);
extern  void invalid_device_error(int device);
extern  int unknown_handler(unsigned short count);
extern  int server_info_handler(unsigned short count);
extern  int init_handler(unsigned short count);
extern  int media_check_handler(unsigned short count);
extern  int build_bpb_handler(unsigned short count);
extern  int read_handler(unsigned short count);
extern  int write_handler(unsigned short count);
extern  int error_handler(unsigned short count);
extern  int ocrm_handler(unsigned short count);
extern  int gen_ioctl_handler(unsigned short count);
extern void print_modes(int in_memory);
extern void prepare_drive_size(int device_index, char *size_buff);
extern int drive_info_handler(word count);
extern void drive_mappings(int in_memory);
/* File main.c */ 
extern void make_printer_map(void);
extern void pascal show_status(int status);
extern void _fastcall show_dp_status(int status);
extern  void setup_auxiliary_devices(void );
extern  void make_scan_portal(void);
extern  void cdecl main(int argc,char * *argv);
extern  void pascal reset_all_serial_ports(void);
extern  void setup_block_devices(void);
/* File quit.c */ 
extern  void cdecl quit(void );
extern  int push_exit_routine(void (pascal *func)(void ));
extern  void (pascal *pop_exit_routine(void ))(void );
/* File screenio.c */ 
extern  void cdecl warn_printf(char *fmt,...);
extern  void cdecl inform_printf(char *fmt,...);
extern  void cdecl fatal_printf(char *fmt,...);
extern  void _fastcall fatal_error(int num);
extern  void set_expansion_box_colors(void );
extern  int cdecl verify_printf(char *header, char *fmt, ...);
extern void show_drive_mappings(byte *map_table, int show_master);
extern void generate_drive_mappings(byte *map_table, int num_master_units, byte *desired_map);
extern void draw_screen(char *string, int extra_lines, int color);
extern void _cdecl do_init_screen(void);
