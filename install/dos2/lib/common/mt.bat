cl -c -Zi -W2 -Od -AS -I. -DLINT_ARGS test.obj test.c

link /NOD/CO/LI/STACK:4096/MAP test.obj+..\dos\hardtest.obj+..\dos\net_pres.obj,test,test,slibcec.lib sulibd.lib lzcopy.lib;


