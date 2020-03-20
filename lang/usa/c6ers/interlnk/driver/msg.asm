public m_prn_se_letter
public m_client_letter
public baud_msg
public m_not_installed
public par_port_name
public m_drive_syntax
public m_map_header
public signon_ndrives
public printer_default
public msg_no_ports
public doslink_help
public printer_paren
public ser_port_name
public m_bad_bios_par
public m_bad_bios_ser
public too_big
public msg_no_devices
public m_too_many_par
public m_connect_ok
public none_str
public m_prn_map
public product_name
public m_too_many_ser
public irq_loc
public m_no_driver
public signon_paren
public m_connect_try
public m_com_bad
public printer_ndrives
public no_drives
public m_max_baud
public m_dl_ver_mismatch
public m_connect_fail
public irq_msg
public m_prn_cl_letter
public auto_str
public signon_dfirst
public m_server_letter
public requires_dos3
public signon_dlast
public comma_space
public no_printers
public m_drive_map
public invalid_switch
public signon_default
public printer_signon
public m_baud_error
public port_address_loc
public cr_lf
public m_lost_connect
public printer_dfirst
public spaces_str
public m_lpt_bad
public printer_dlast
public par_port_number
public m_bad_par_addr
public drives_error
public baud_loc
public m_not_connected
public port_address_msg
public ser_port_number
public m_bad_ser_addr

_msg segment
m_prn_se_letter db "m_prn_se_letter"
m_client_letter db "m_client_letter"
baud_msg db "baud_msg"
m_not_installed db "m_not_installed"
par_port_name db "par_port_name"
m_drive_syntax db "m_drive_syntax"
m_map_header db "m_map_header"
signon_ndrives db "signon_ndrives"
printer_default db "printer_default"
msg_no_ports db "msg_no_ports"
doslink_help db "doslink_help"
printer_paren db "printer_paren"
ser_port_name db "ser_port_name"
m_bad_bios_par db "m_bad_bios_par"
m_bad_bios_ser db "m_bad_bios_ser"
too_big db "too_big"
msg_no_devices db "msg_no_devices"
m_too_many_par db "m_too_many_par"
m_connect_ok db "m_connect_ok"
none_str db "none_str"
m_prn_map db "m_prn_map"
product_name db "product_name"
m_too_many_ser db "m_too_many_ser"
irq_loc db "irq_loc"
m_no_driver db "m_no_driver"
signon_paren db "signon_paren"
m_connect_try db "m_connect_try"
m_com_bad db "m_com_bad"
printer_ndrives db "printer_ndrives"
no_drives db "no_drives"
m_max_baud db "m_max_baud"
m_dl_ver_mismatch db "m_dl_ver_mismatch"
m_connect_fail db "m_connect_fail"
irq_msg db "irq_msg"
m_prn_cl_letter db "m_prn_cl_letter"
auto_str db "auto_str"
signon_dfirst db "signon_dfirst"
m_server_letter db "m_server_letter"
requires_dos3 db "requires_dos3"
signon_dlast db "signon_dlast"
comma_space db ", "
no_printers db "no_printers"
m_drive_map db "m_drive_map"
invalid_switch db "invalid_switch"
signon_default db "signon_default"
printer_signon db "printer_signon"
m_baud_error db "m_baud_error"
port_address_loc db "port_address_loc"
cr_lf db 0Ah,0Dh
m_lost_connect db "m_lost_connect"
printer_dfirst db "printer_dfirst"
spaces_str db "spaces_str"
m_lpt_bad db "m_lpt_bad"
printer_dlast db "printer_dlast"
par_port_number db "par_port_number"
m_bad_par_addr db "m_bad_par_addr"
drives_error db "drives_error"
baud_loc db "baud_loc"
m_not_connected db "m_not_connected"
port_address_msg db "port_address_msg"
ser_port_number db "ser_port_number"
m_bad_ser_addr db "m_bad_ser_addr"
_msg ends
end
