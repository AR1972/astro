M000 01/23/90 DLB  getemm.asm   Bug #5421: Test for $MMXXXX0 device (when
                                "device=EMM386 NOEMS") as well as EMMXXXX0
                                device.

M001 01/23/90 DLB  getemm.asm   Bug #4763: If EMMXXXX0 or $MMXXXX0 device
                                exists, verify it is EMM386.EXE by checking
                                for "MICROSOFT" signature.

M002 01/23/90 DLB  himem.asm    Bug #5420: Close XMSXXXX0 device.
