@echo off

set TESTOUT=test.out

touch %TESTOUT% > NUL
del   %TESTOUT% > NUL

touch empty > NUL
del   empty > NUL
touch empty > NUL


echo.                              >> %TESTOUT%
echo ***** DOS test cases *****     > %TESTOUT%
echo.                              >> %TESTOUT%

touch input  > NUL
del   input  > NUL
touch output > NUL
del   output > NUL
echo ***** scompb -a2 empty *****  >> %TESTOUT%
scompb -a2 empty input             >> %TESTOUT%
echo ***** sdecompb empty *****    >> %TESTOUT%
sdecompb input output              >> %TESTOUT%
echo ***** cmp -a2 empty *****     >> %TESTOUT%
cmp empty output                   >> %TESTOUT%

touch input  > NUL
del   input  > NUL
touch output > NUL
del   output > NUL
echo ***** scompr -a2 empty *****  >> %TESTOUT%
scompr -a2 empty input             >> %TESTOUT%
echo ***** sdecompr empty *****    >> %TESTOUT%
sdecompr input output              >> %TESTOUT%
echo ***** cmp -a2 empty *****     >> %TESTOUT%
cmp empty output                   >> %TESTOUT%
touch output > NUL
del   output > NUL
echo ***** tdecompr empty *****    >> %TESTOUT%
tdecompr input output              >> %TESTOUT%
echo ***** cmp -a2 empty *****     >> %TESTOUT%
cmp empty output                   >> %TESTOUT%


touch input  > NUL
del   input  > NUL
touch output > NUL
del   output > NUL
echo ***** scompb -a3 empty *****  >> %TESTOUT%
scompb -a3 empty input             >> %TESTOUT%
echo ***** sdecompb empty *****    >> %TESTOUT%
sdecompb input output              >> %TESTOUT%
echo ***** cmp -a3 empty *****     >> %TESTOUT%
cmp empty output                   >> %TESTOUT%

touch input  > NUL
del   input  > NUL
touch output > NUL
del   output > NUL
echo ***** scompr -a3 empty *****  >> %TESTOUT%
scompr -a3 empty input             >> %TESTOUT%
echo ***** sdecompr empty *****    >> %TESTOUT%
sdecompr input output              >> %TESTOUT%
echo ***** cmp -a3 empty *****     >> %TESTOUT%
cmp empty output                   >> %TESTOUT%
touch output > NUL
del   output > NUL
echo ***** tdecompr empty *****    >> %TESTOUT%
tdecompr input output              >> %TESTOUT%
echo ***** cmp -a3 empty *****     >> %TESTOUT%
cmp empty output                   >> %TESTOUT%


touch input  > NUL
del   input  > NUL
touch output > NUL
del   output > NUL
echo ***** scompb -a2 orig *****   >> %TESTOUT%
scompb -a2 orig  input             >> %TESTOUT%
echo ***** sdecompb orig *****     >> %TESTOUT%
sdecompb input output              >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch input  > NUL
del   input  > NUL
touch output > NUL
del   output > NUL
echo ***** scompr -a2 orig *****   >> %TESTOUT%
scompr -a2 orig  input             >> %TESTOUT%
echo ***** sdecompr orig *****     >> %TESTOUT%
sdecompr input output              >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%
touch output > NUL
del   output > NUL
echo ***** tdecompr orig *****     >> %TESTOUT%
tdecompr input output              >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** sfiler -a2 *****        >> %TESTOUT%
sfiler                             >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** cfiler -a2 *****        >> %TESTOUT%
cfiler                             >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** mfiler -a2 *****        >> %TESTOUT%
mfiler                             >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** lfiler -a2 *****        >> %TESTOUT%
lfiler                             >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** sbuffr -a2 *****        >> %TESTOUT%
sbuffr                             >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** cbuffr -a2 *****        >> %TESTOUT%
cbuffr                             >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** mbuffr -a2 *****        >> %TESTOUT%
mbuffr                             >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** lbuffr -a2 *****        >> %TESTOUT%
lbuffr                             >> %TESTOUT%
echo ***** cmp -a2 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%


touch input  > NUL
del   input  > NUL
touch output > NUL
del   output > NUL
echo ***** scompb -a3 orig *****   >> %TESTOUT%
scompb -a3 orig  input             >> %TESTOUT%
echo ***** sdecompb orig *****     >> %TESTOUT%
sdecompb input output              >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch input  > NUL
del   input  > NUL
touch output > NUL
del   output > NUL
echo ***** scompr -a3 orig *****   >> %TESTOUT%
scompr -a3 orig  input             >> %TESTOUT%
echo ***** sdecompr orig *****     >> %TESTOUT%
sdecompr input output              >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%
touch output > NUL
del   output > NUL
echo ***** tdecompr orig *****     >> %TESTOUT%
tdecompr input output              >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** sfiler -a3 *****        >> %TESTOUT%
sfiler                             >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** cfiler -a3 *****        >> %TESTOUT%
cfiler                             >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** mfiler -a3 *****        >> %TESTOUT%
mfiler                             >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** lfiler -a3 *****        >> %TESTOUT%
lfiler                             >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** sbuffr -a3 *****        >> %TESTOUT%
sbuffr                             >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** cbuffr -a3 *****        >> %TESTOUT%
cbuffr                             >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** mbuffr -a3 *****        >> %TESTOUT%
mbuffr                             >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%

touch output > NUL
del   output > NUL
echo ***** lbuffr -a3 *****        >> %TESTOUT%
lbuffr                             >> %TESTOUT%
echo ***** cmp -a3 orig *****      >> %TESTOUT%
cmp orig output                    >> %TESTOUT%


touch input  > NUL
del   input  > NUL
touch output > NUL
del   output > NUL


echo more %TESTOUT%

