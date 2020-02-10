set tl=\45\tl\bin
set include=.;\45\tl\inc
set lib=\45\tl\lib
set path=%tl%;%path%
%TL%\exp /R

REM build all the device drivers, and propogate them
cd lib
command /c makelqb
cd ..

command /e:1024 /c make cowlqb
%TL%\results log
