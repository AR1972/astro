all:
    cd comsubs
#    $(MAKE)
    cd ..\mapper
    $(MAKE)
    cd ..\master
    $(MAKE)
    cd ..\mem
    $(MAKE)
    cd ..\xmm
    $(MAKE)
    cd ..

clean:
    if exist comsubs.lib    del comsubs.lib
    if exist mapper.lib     del mapper.lib
    if exist master.lib     del master.lib
    if exist mem.lib        del mem.lib
    if exist xmm.lib        del xmm.lib
    if exist ldecompr.lib   del ldecompr.lib
    if exist mdecompr.lib   del mdecompr.lib
    if exist sdecompr.lib   del sdecompr.lib
    cd comsubs
    $(MAKE) clean
    cd ..\mapper
    $(MAKE) clean
    cd ..\master
    $(MAKE) clean
    cd ..\mem
    $(MAKE) clean
    cd ..\xmm
    $(MAKE) clean
    cd ..