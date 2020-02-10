.comment ******************************************************************
.comment *  Graphics Family - QBasic                                      *
.comment ******************************************************************
.comment *  7/13/90 stevesau                                              *
.comment *  Incorporated QA/copy edits.                                   *
.comment ******************************************************************
.comment *  6/28/90 stevesau                                              *
.comment *  Incorporated final review comments/edits.                     *
.comment *  Split up SCREEN function and statement.                       *
.comment ******************************************************************
.comment *  6/12/90 stevesau                                              *
.comment *  Incorporated RonS comments.                                   *
.comment ******************************************************************
.comment *  5/11/90 stevesau                                              *
.comment *  Replaced Attributes and Colors node with COLOR example        *
.comment *  showing color codes.                                          *
.comment *  Combined GET/PUT.                                             *
.comment *  Incorporated review comments.                                 *
.comment *  Completed second pass.                                        *
.comment ******************************************************************
.comment *  5/10/90 stevesau                                              *
.comment *  Reinserted X command in DRAW.                                 *
.comment *  Added Attributes and Colors node.                             *
.comment ******************************************************************
.comment *  5/4/90 stevesau                                               *
.comment *  Combined SCREEN function and statement (from Dev I/O family). *
.comment *  Created new Screen Modes topic.                               *
.comment ******************************************************************
.comment *  4/23/90 stevesau                                              *
.comment *  Added action lines.						  *
.comment *  Made COLOR Attributes link local.                             *
.comment *  Deleted references to X command in DRAW.                      *
.comment *  Added DRAW macro language screens, made links local.          *
.comment *  Added DRAW - Differences from BASICA.                         *
.comment ******************************************************************
.context CIRCLE
.context @CIRCLE
:nCIRCLE Statement
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Desenha um c¡rculo ou elipse na tela.

\bCIRCLE [STEP] (x!,y!),radius![,[color%] [,[start!] [,[end!] [,aspect!]]]]\p

    ş \bSTEP\p       Especifica que as coordenadas sao relativas … posi‡ao
                 atual do cursor no gr fico.
    ş \b(x!,y!)\p    As coordenadas para o centro do c¡rculo ou elipse.
    ş \bradius!\p    O raio do c¡rculo ou elipse em unidades do sistema atual
                 de coordenadas, determinado pelas instru‡oes SCREEN, VIEW e
                 WINDOW mais recentes.
    ş \bcolor%\p     Um atributo de cor que define a cor do c¡rculo. Os
                 atributos de cor dispon¡veis dependem de seu adaptador gr fico
                 e o modo exibi‡ao??? definido pela instru‡ao SCREEN mais recente.
    ş \bstart!\p     O ƒngulo inicial para o arco, em radianos.
    ş \bend!\p       O ƒngulo final para o arco, em radianos.
    ş \baspect!\p    A razao entre o comprimento do eixo y e o comprimento do
                 eixo x, utilizado para desenhar elipses.

    ş Para converter graus em radianos, multiplique os graus por (PI / 180).

Example:
    'This example requires a color graphics adapter.
    SCREEN 2
    CIRCLE (320, 100), 200
    CIRCLE STEP (0,0), 100

See Also    \i\p\aCOLOR\v@COLOR\v\i\p    \i\p\aDRAW\v@DRAW\v\i\p    \i\p\aLINE\v@LINE\v\i\p    \i\p\aSCREEN\v@SCREEN\v\i\p    \i\p\aVIEW\v@VIEW\v\i\p    \i\p\aWINDOW\v@WINDOW\v\i\p
            \i\p\aColor Attributes and Values\v@color.table\v\i\p    \i\p\aScreen Modes\v@screen.modes\v\i\p
.context COLOR
.context @COLOR
:nCOLOR Statement
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Define as cores de exibi‡ao da tela.

\bCOLOR [foreground%] [,[background%] [,border%]]\p    Screen mode 0 (text only)
\bCOLOR [background%] [,palette%]\p                    Screen mode 1
\bCOLOR [foreground%]\p                                Screen modes 4, 12, 13
\bCOLOR [foreground%] [,background&]\p                 Screen modes 7-10

    ş \bforeground%\p    Um n£mero que define a cor de primeiro plano da tela.
      \bforeground&\p    No modo de tela 0, foreground% ‚ um atributo de cor que
                     define a cor do texto. Em outros modos de tela, foreground%
                     ‚ um atributo de cor ou valor de cor de 4 bits (apenas no modo
                     de tela?? 4) que define a cor do texto e do desenho de linha.
    ş \bbackground%\p    Um n£mero que define a cor de segundo plano da tela. No
      \bbackground&\p    modo de tela 0, background% ‚ um atributo de cor. No
                     modo de tela 1, background% ‚ um valor de cor de 4 bits. Nos
                     modos de tela 7 a 10, background& ‚ um valor de cor.
    ş \bborder%\p        Um atributo de cor que define a cor da borda da tela.
    ş \bpalette%\p       Um n£mero (0 ou 1) que especifica qual dos dois conjuntos
                     de atributos de cores que ser  utilizado:

                     palette%    Atributo 1     Atributo  2    Atributo 3
                     ÍÍÍÍÍÍÍÍ    ÍÍÍÍÍÍÍÍÍÍÍ    ÍÍÍÍÍÍÍÍÍÍÍ    ÍÍÍÍÍÍÍÍÍÍÍÍ
                     0           Verde          Vermelha       Marrom
                     1           Ciano          Magenta        Branca clara

    ş Os valores e atributos de cores dispon¡veis dependem de seu adaptador
      gr fico e o modo de tela definido pela instru‡ao SCREEN mais recente.
    ş Se o seu sistema estiver equipado com um adaptador EGA, VGA ou MCGA,
      utilize a instru‡ao PALETTE para modificar as designa‡oes de cor dos
      atributos de cores.

Example:
    'This example requires a color graphics adapter.
    SCREEN 7
    FOR i% = 0 TO 15
        COLOR i%
        PRINT i%
    NEXT i%

See Also    \i\p\aDRAW\v@DRAW\v\i\p    \i\p\aPAINT\v@PAINT\v\i\p    \i\p\aPALETTE, PALETTE USING\v@PALETTE\v\i\p    \i\p\aSCREEN\v@SCREEN\v\i\p
            \i\p\aColor Attributes and Values\v@color.table\v\i\p    \i\p\aScreen Modes\v@screen.modes\v\i\p
.context @color.table
:nColor Attributes and Values
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
             \bColor monitor                      Monochrome monitor\p
             ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
\bColor        Default          Displayed         Default       Displayed\p
\battribute    color value\p(a)\b   color             color value   color\p
ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
\bSCREEN Modes 0, 7, 8, 9\p(b)\b, 12, and 13\p
0            0                Preta             0(c)          Desativada
1            1                Azul                            Sublinhado(d)
2            2                Verde             1(c)          Ativada(d)
3            3                Ciano             1(c)          Ativada(d)
4            4                Vermelha          1(c)          Ativada(d)
5            5                Magenta           1(c)          Ativada(d)
6            6                Marrom            1(c)          Ativada(d)
7            7                Branca            1(c)          Ativada(d)
8            8                Cinza             0(c)          Desativada
9            9                Azul Claro                      Alta intensidade
                                                              Sublinhado
10           10               Verde Claro       2(c)          Alta intensidade
11           11               Ciano Claro       2(c)          Alta intensidade
12           12               Vermelha Clara    2(c)          Alta intensidade
13           13               Magenta Clara     2(c)          Alta intensidade
14           14               Amarela           2(c)          Alta intensidade
15           15               Branca de alta    0(c)          Desativada
                              intensidade
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
\bSCREEN Modes 1 and 9\p(e)

0            0                Preta             0             Desativada
1            11               Ciano Claro       2             Alta intensidade
2            13               Magenta Clara     2             Alta intensidade
3            15               Branca de alta    0             Branco acinzentado???
                              intensidade
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
\bSCREEN Modes 2 and 11\p
0            0                Preta            0             Desativada
1            15               Branca de alta   0             Branco acinzentado??
                              intensidade
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
  (a)  n£meros de cores EGA. VGA e MCGA utilizam valores de cor de exibi‡ao
       que produzem cores visualmente equivalentes.
  (b)  Para VGA ou EGA com mem¢ria de v¡deo > 64K.
  (c)  Somente para o modo 0.
  (d)  Desligado quando utilizado para segundo plano.
  (e)  EGA com mem¢ria de v¡deo <= 64K.

See Also    \i\p\aCOLOR\v@COLOR\v\i\p    \i\p\aPALETTE, PALETTE USING\v@PALETTE\v\i\p    \i\p\aSCREEN\v@SCREEN\v\i\p
            \i\p\aScreen Modes\v@screen.modes\v\i\p
.context DRAW
.context @DRAW
:nDRAW Statement
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Desenha um objeto.

\bDRAW commandstring$\p

    ş \bcommandstring$\p    Uma expressao de seqˆncia que cont‚m um ou mais
                        dos seguintes comandos DRAW.

      Comandos de desenho de linha e movimenta‡ao do cursor:
        D[n%]            Move o cursor para baixo n% unidades.
        E[n%]            Move o cursor para cima e para a direita n% unidades.
        F[n%]            Move o cursor para baixo e para a direita n% unidades.
        G[n%]            Move o cursor para baixo e para a esquerda n% unidades.
        H[n%]            Move o cursor para cima e para a esquerda n% unidades.
        L[n%]            Move o cursor para a esquerda n% unidades.
        M[{+|-}]x%,y%    Move o cursor para o ponto x%,y%. Se x% for precedido
                         por + ou -, move em rela‡ao ao ponto atual.
        R[n%]            Move o cursor para a direita n% unidades.
        U[n%]            Move o cursor para cima n% unidades.
        [B]              Um prefixo opcional que movimenta o cursor sem desenhar.
        [N]              Um prefixo opcional que desenha e retorna o cursor para
                         a sua posi‡ao original.

      Comandos de cor, rota‡ao e escala:
        An%              Rotaciona um objeto n% * 90 graus (n% pode ser 0, 1,
                         2 ou 3).
        Cn%              Define a cor para o desenho (n% ‚ o atributo da cor).
        Pn1%,n2%         Define a cor de preenchimento e das bordas de um objeto
                         (n1% ‚ o atributo da cor de preenchimento, n2% ‚ o
                         atributo da cor das bordas).
        Sn%              Determina a escala do desenho definindo o comprimento
                         de uma unidade de movimenta‡ao do cursor. O n% padrao
                         ‚ 4, o que ‚ equivalente a 1 pixel.
        TAn%             Rotaciona??Movimenta?? um ƒngulo n% graus (-360 a 360).

    ş Se vocˆ omitir n% dos comandos de desenho de linha e movimenta‡ao do
      cursor, o cursor ser  movido 1 unidade.
    ş Para executar uma sub-seqˆncia do comando DRAW, de uma seqˆncia do
      comando DRAW, utilize o comando "X":

      DRAW "X"+ VARPTR$(seqˆncia do comando$)

Example:
    'This example requires a color graphics adapter.
    SCREEN 1
    Triangle$ = "F60 L120 E60"
    DRAW "C2 X" + VARPTR$(Triangle$)
    DRAW "BD30 P1,2 C3 M-30,-30"

See Also    \i\p\aPALETTE, PALETTE USING\v@PALETTE\v\i\p    \i\p\aSCREEN\v@SCREEN\v\i\p    \i\p\aVARPTR$\v@VARPTR$\v\i\p
            \i\p\aColor Attributes and Values\v@color.table\v\i\p    \i\p\aDifferences from BASICA\v@basica.diffs\v\i\p
.context .ggx
.context @ggx
.context .pgfx
.context @pgfx
:nGET, PUT Statements (Graphics)
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
GET captura uma imagem gr fica da tela. PUT exibe a imagem capturada por GET.

\bGET [STEP](x1!,y1!)-[STEP](x2!,y2!), arrayname[(index%)]\p
\bPUT [STEP] (x1!,y1!), arrayname[(index%)] [,actionverb]\p

    ş \bSTEP\p          Especifica que as coordenadas sao relativas … posi‡ao
                    atual do cursor no gr fico.
    ş \b(x1!,y1!)\p     As coordenadas no canto superior esquerdo da imagem
                    capturada por GET ou no local da tela onde PUT exibe a imagem.
    ş \b(x2!,y2!)\p     As coordenadas do canto inferior direito da imagem capturada.
    ş \barrayname\p     O nome da matriz onde a imagem est  armazenada.
                    See \i\p\aScreen Image Arrays and Compatibility\v@ggx.arrays\v\i\p to determine
                    the required size of the array.
    ş \bindex%\p        O ¡ndice da matriz na qual inicia o armazenamento da imagem.???
    ş \bactionverb\p    Uma palavra chave indicando como a imagem ser  exibida:

              Palavra Chave    A‡ao
                    ÍÍÍÍÍÍÍ    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
                    AND        Mescla a imagem armazenada com uma imagem existente.
                    OR         Sobrepoe a imagem armazenada sobre a imagem existente.??
                    PSET       Desenha a imagem armazenada, apagando a imagem existente.
                    PRESET     Desenha a imagem armazenada utilizando cores em reverso,
                               apagando a imagem existente.
                    XOR        Desenha uma imagem armazenada ou apaga uma imagem
                               pr‚-desenhada enquanto preserva o desenho de segundo
                               plano,?? produzindo efeitos de anima‡ao.

    ş Uma instru‡ao PUT deve sempre ser executada no mesmo modo de tela que a
      instru‡ao GET utilizada para capturar a imagem, ou num modo compat¡vel.
      See \i\p\aScreen Image Arrays and Compatibility\v@ggx.arrays\v\i\p.

Example:
    'This example requires a color graphics adapter.
    SCREEN 1
    DIM Box%(1 TO 200)
    x1% = 0: x2% = 10: y1% = 0: y2% = 10
    LINE (x1%, y1%)-(x2%, y2%), 2, BF
    GET (x1%, y1%)-(x2%, y2%), Box%
    DO
        PUT (x1%, y1%), Box%, XOR
        x1% = RND * 300
        y1% = RND * 180
        PUT (x1%, y1%), Box%
    LOOP WHILE INKEY$ = ""

See Also    \i\p\aSCREEN\v@SCREEN\v\i\p    \i\p\aScreen Modes\v@screen.modes\v\i\p
.context @ggx.arrays
:nScreen Image Arrays and Compatibility
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Utilize bits-por-pixel-por-plano e valores de plano para determinar o tamanho
requerido da matriz que armazena uma imagem gr fica da tela. Bits-por-pixel-por
-plano e valores de plano, juntamente com a resolu‡ao horizontal, tamb‚m
determinam quais modos de tela sao compat¡veis:

                                                                 Resolu‡ao
                                    Bits-por-pixel-              horizontal
    Modo de tela                    por-plano          Planos    (em pixels)
    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ    ÍÍÍÍÍÍ    ÍÍÍÍÍÍÍÍÍÍÍ
    1                               2                  1         320
    2, 4, 11                        1                  1         640
    3                               1                  1         720
    7                               1                  4         320
    8, 9(> mem¢ria de v¡deo de 64K), 12    1                  4         640
    9(mem¢ria de v¡deo de 64K), 10         1                  2         640
    13                              8                  1         320

A seguinte f¢rmula fornece o tamanho requerido, em bytes, de uma matriz
utilizada para armazenar uma imagem capturada:

size% = 4 + INT(((PMAP (x2!, 0) - PMAP (x1!, 0) + 1) *
        (bits-por-pixel-por-plano%) + 7) / 8) * planos% *
        (PMAP (y2!, 1) - PMAP (y1!, 1) + 1)

As opera‡oes GET e PUT sao compat¡veis nos modos de tela com a mesma
resolu‡ao horizontal e bits-por-pixel-por-plano e valores de plano. Por
exemplo, os modos de tela 2, 4 e 11 sao compat¡veis, e os modos de tela
8 e 12 sao compat¡veis.

See Also    \i\p\aSCREEN\v@SCREEN\v\i\p    \i\p\aScreen Modes\v@screen.modes\v\i\p
.context LINE
.context @LINE
:nLINE Statement
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Desenha uma linha ou retƒngulo na tela.

\bLINE [[STEP](x1!,y1!)]-[STEP](x2!,y2!) [,[color%] [,[B | BF] [,style%]]]\p

    ş \bSTEP\p          Especifica que as coordenadas sao relativas … posi‡ao
                    atual do cursor no gr fico.??
    ş \b(x1!,y1!)\p,    As coordenadas de tela do in¡cio da linha e do final
      \b(x2!,y2!)\p     da linha.
    ş \bcolor%\p        Um atributo de cor que define a cor da linha ou
                    retƒngulo. Os atributos de cores dispon¡veis dependem
                    de seu adaptador gr fico e o modo de tela definido pela
                    instru‡ao SCREEN mais recente.
    ş \bB\p             Desenha um retƒngulo ao inv‚s de uma linha.
    ş \bBF\p            Desenha uma caixa s¢lida.
    ş \bstyle%\p        Um valor de 16 bits cujos bits especificam se os pixels
                    sao desenhados ou nao. Utilize para desenhar linhas tracejadas
                    ou pontilhadas.

Example:
    'This example requires a color graphics adapter.
    SCREEN 1
    LINE (110, 70)-(190, 120), , B
    LINE (0, 0)-(320, 200), 3, , &HFF00

See Also    \i\p\aCIRCLE\v@CIRCLE\v\i\p    \i\p\aINPUT, LINE INPUT\v@INPUT\v\i\p    \i\p\aPRESET, PSET\v@PSET\v\i\p    \i\p\aSCREEN\v@SCREEN\v\i\p
            \i\p\aColor Attributes and Values\v@color.table\v\i\p    \i\p\aScreen Modes\v@screen.modes\v\i\p
.context PAINT
.context @PAINT
:nPAINT Statement
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Preenche uma  rea gr fica com uma determinada cor ou padrao.??????

\bPAINT [STEP] (x!,y!)[,[{color% | tile$}] [,[bordercolor%] [,background$]]]\p

    ş \bSTEP\p            Especifica que as coordenadas sao relativas … posi‡ao
                      atual do cursor no gr fico.???
    ş \b(x!,y!)\p         As coordenadas de tela onde a pintura inicia.
    ş \bcolor%\p          Um atributo de cor que define a cor de preenchimento.
    ş \btile$\p           Um padrao de preenchimento com 8 bits de largura e
                      at‚ 64 bytes de comprimento, definido como:

                        tile$ = CHR$(arg1) + CHR$(arg2) + ... + CHR$(argn%)

                      Os argumentos para CHR$ sao n£meros entre 0 e 255.
                      Cada CHR$(argn%) define uma fatia de 1-byte, 8-pixels
                      do padrao, baseado no formato bin rio do n£mero.
    ş \bbordercolor%\p    Um atributo de cor que especifica a cor da borda da
                       rea preenchida. PAINT encerra o preenchimento da  rea
                      quando encontrar a borda da cor especificada.
    ş \bbackground$\p     Uma fatia de 1-byte, 8-pixels do tipo lado-a-lado de
                      segundo plano. A especifica‡ao de uma fatia lado-a-lado de
                      segundo plano possibilita pintar sobre uma  rea j  pintada.

    ş Os atributos de cores dispon¡veis dependem de seu adaptador gr fico e do
      modo de tela definido pela instru‡ao SCREEN mais recente.

Example:
    'This example requires a color graphics adapter.
    SCREEN 1
    CIRCLE (106, 100), 75, 1
    LINE (138, 35)-(288, 165), 1, B
    PAINT (160, 100), 2, 1

See Also    \i\p\aASC, CHR$\v@CHR$\v\i\p    \i\p\aCIRCLE\v@CIRCLE\v\i\p    \i\p\aDRAW\v@DRAW\v\i\p    \i\p\aLINE\v@LINE\v\i\p    \i\p\aSCREEN\v@SCREEN\v\i\p
            \i\p\aColor Attributes and Values\v@color.table\v\i\p    \i\p\aScreen Modes\v@screen.modes\v\i\p
.context PALETTE
.context @PALETTE
:nPALETTE, PALETTE USING Statements
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Modifique as designa‡oes de cores dos atributos de cores no modo de tela atual.
PALETTE e PALETTE USING funcionam somente em sistemas equipados com adaptadores
EGA, VGA ou MCGA.

\bPALETTE [attribute%,color&]\p
\bPALETTE USING arrayname#[(index%)]\p

    ş \battribute%\p    O atributo de cor a ser modificado.
    ş \bcolor&\p        Um valor de cor a ser designado a um atributo.
    ş \barrayname#\p    Uma matriz de valores de cores a ser designada ao conjunto
                     de atributos do modo de tela atual. A matriz deve ser
                     suficientemente grande para designar cores a todos os atributos.
    ş \bindex%\p        O ¡ndice do primeiro elemento da matriz a ser designado a
                     um atributo.

    ş Os atributos de corees e valores dispon¡veis dependem de seu adaptador gr fico
      e do modo de tela definido pela instru‡ao SCREEN mais recente.

Example:
    'This example requires a color graphics adapter.
    PALETTE 0, 1
    SCREEN 1
    FOR i% = 0 TO 3: a%(i%) = i%: NEXT i%
    LINE (138, 35)-(288, 165), 3, BF
    LINE (20, 10)-(160, 100), 2, BF
    DO
        FOR i% = 0 TO 3
            a%(i%) = (a%(i%) + 1) MOD 16
        NEXT i%
        PALETTE USING a%(0)
    LOOP WHILE INKEY$ = ""

See Also    \i\p\aCOLOR\v@COLOR\v\i\p    \i\p\aSCREEN\v@SCREEN\v\i\p
            \i\p\aColor Attributes and Values\v@color.table\v\i\p    \i\p\aScreen Modes\v@screen.modes\v\i\p
.context PCOPY
.context @PCOPY
:nPCOPY Statement
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Copia uma p gina de mem¢ria de v¡deo??? para outra.

\bPCOPY sourcepage%,destinationpage%\p

    ş \bsourcepage%\p         O n£mero da p gina de mem¢ria de v¡deo a ser copiada.
    ş \bdestinationpage%\p    O n£mero da p gina de mem¢ria de v¡deo para onde copiar.

    ş O valor que indentifica a p gina de v¡deo ‚ determinado pelo tamanho
      da mem¢ria de v¡deo e o modo de tela atual.

Example:
    PCOPY 1, 3

See Also    \i\p\aSCREEN\v@SCREEN\v\i\p    \i\p\aScreen Modes\v@screen.modes\v\i\p
.context SCREEN
.context @SCREEN
:nSCREEN Statement
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Define o modo de tela e outras caracter¡sticas da tela.

\bSCREEN mode% [,[colorswitch%] [,[activepage%] [,visualpage%]]]\p

    ş \bmode%\p           Define o modo de tela. See \i\p\aScreen Modes\v@screen.modes\v\i\p.
    ş \bcolorswitch%\p    Um valor (0 ou 1) que alterna entre a exibi‡ao
                      monocrom tica e colorida (somente os modos 0 e 1):

                      Modo    Valor       A‡ao
                      ÍÍÍÍ    ÍÍÍÍÍÍÍÍ    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
                      0       0           Desativa cores
                      0       Nao zero    Ativa cores
                      1       0           Ativa cores
                      1       Nao zero    Desativa cores

    ş \bactivepage%\p     A p gina de tela na qual a sa¡da de gr fico ou texto ser  gravada.
    ş \bvisualpage%\p     A p gina de tela atualmente exibida na tela.

Example:
    'This example requires a color graphics adapter.
    SCREEN 1        '320 x 200 graphics
    LINE (110, 70)-(190, 120), , B
    LINE (0, 0)-(320, 200), 3, , &HFF00

See Also    \i\p\aCIRCLE\v@CIRCLE\v\i\p    \i\p\aCOLOR\v@COLOR\v\i\p    \i\p\aDRAW\v@DRAW\v\i\p    \i\p\aLINE\v@LINE\v\i\p      \i\p\aPAINT\v@PAINT\v\i\p
            \i\p\aSCREEN Function\v@screenf\v\i\p      \i\p\aVIEW\v@VIEW\v\i\p    \i\p\aWINDOW\v@WINDOW\v\i\p    \i\p\aScreen Modes\v@screen.modes\v\i\p
.context @screen.modes
:nScreen Modes
  \i\p\aContents\vhelpTableId\v\i\p  \i\p\aIndex\vhelpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
A seguinte tabela apresenta um resumo dos modos de tela:???

ÄÄÄÄÄÄÄÄÄÄAdaptadores MDPA, CGA, Hercules, Olivetti, EGA, VGA ou MCGAÄÄÄÄÄÄÄÄÄÄÄ
SCREEN 0: Somente modo texto
    ş Formato de texto 40 x 25, 40 x 43, 40 x 50, 80 x 25, 80 x 43 ou 80 x 50,
      caixa de caractere 8 x 8 (8 x 14, 9 x 14 ou 9 x 16 com EGA ou VGA)
    ş 16 cores designadas a qualquer um dos 16 atributos (com CGA ou EGA)
    ş 64 cores designadas a qualquer um dos 16 atributos (com EGA ou VGA)
    ş Dependendo da resolu‡ao do texto e do adaptador, 8 p ginas de mem¢ria de
      v¡deo (0-7), 4 p ginas (0-3), 2 p ginas (0-1) ou 1 p gina (0)

ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄAdaptadores CGA, EGA, VGA ou MCGAÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
SCREEN 1: gr fico??? 320 x 200
    ş formato de texto 40 x 25, caixa de caractere 8 x 8
    ş 16 cores de segundo plano e um de dois conjuntos de 3 cores de segundo
      plano designados com a instru‡ao COLOR com CGA
    ş 16 cores designadas a 4 atributos com EGA ou VGA
    ş 1 p gina de mem¢ria de v¡deo (0)
SCREEN 2: gr fico 640 x 200
    ş formato de texto 80 x 25, caixa de caractere 8 x 8
    ş 16 cores designadas a 2 atributos com EGA ou VGA
    ş 1 p gina de mem¢ria de v¡deo (0)

ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄAdaptadores Hercules, Olivetti ou AT&TÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
SCREEN 3: Adaptador Hercules requerido, somente monitor monocrom tico
    ş gr fico 720 x 348
    ş formato de texto 80 x 25, caixa de caractere 9 x 14
    ş Normalmente 2 p ginas de mem¢ria de v¡deo (0-1); 1 p gina (0) se for
      instalado um adaptador de exibi‡ao?? colorido secund rio
    ş A instru‡ao PALETTE nao ‚ suportada
    ş Ativa?? o controlador MSHERC.COM da Hercules antes de utilizar o modo de tela 3
SCREEN 4:
    ş Suporta os modelos M24, M240, M28, M280, M380, M380/C e M380/T da Olivetti
      Personal Computers e AT&T Personal Computers 6300 series
    ş gr fico 640 x 400
    ş formato de texto 80 x 25, caixa de caractere 8 x 16
    ş 1 de 16 cores designadas como cor de primeiro plano (selecionada pela
      instru‡ao COLOR); o segundo plano est  fixado na cor preta
    ş 1 p gina de mem¢ria de v¡deo (0)
    ş a instru‡ao PALETTE nao ‚ suportada

ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄAdaptadores EGA ou VGAÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
SCREEN 7: gr fico 320 x 200
    ş formato de texto 40 x 25, caixa de caractere 8 x 8
    ş Designa‡ao de 16 cores a qualquer um de 16 atributos
    ş Se for um adaptador EGA com 64K de mem¢ria, 2 p ginas de mem¢ria de v¡deo
      (0-1); caso contr rio, 8 p ginas (0-7)
SCREEN 8: gr fico 640 x 200
    ş formato de texto 80 x 25, caixa de caractere 8 x 8
    ş Designa‡ao de 16 cores a qualquer um de 16 atributos
    ş Se for um adaptador EGA com 64K de mem¢ria, 1 p gina de mem¢ria de v¡deo
      (0); caso contr rio, 4 p ginas (0-3)
SCREEN 9: gr fico 640 x 350
    ş formato de texto 80 x 25 ou 80 x 43, caixa de caractere 8 x 14 ou 8 x 8
    ş 16 cores designadas a 4 atributos (adaptador com de 64K de mem¢ria), ou
      64 cores designadas a 16 atributos (adaptador com mais que 64K)
    ş Se for um adaptador EGA com 64K de mem¢ria, 1 p gina de mem¢ria de v¡deo (0);
      caso contr rio, 2 p ginas (0-1)

ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄAdaptadores EGA ou VGA, Somente Monitor Monocrom ticoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
SCREEN 10: gr fico 640 x 350, somente monitor monocrom tico
    ş formato de texto 80 x 25 ou 80 x 43, caixa de caractere 8 x 14 ou 8 x 8
    ş At‚ 9 pseudocores designadas a 4 atributos
    ş 2 p ginas de mem¢ria de v¡deo (0-1), adaptador com mem¢ria de 256K exigido

ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄAdaptadores VGA ou MCGAÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Screen 11 (VGA ou MCGA)
    ş gr fico 640 x 480
    ş formato de texto 80 x 30 ou 80 x 60, caixa de caractere 8 x 16 ou 8 x 8
    ş Designa‡ao de at‚ 256K cores a 2 atributos
    ş 1 p gina de mem¢ria de v¡deo (0)
Screen 12 (VGA)
    ş Ggr fico 640 x 480
    ş Fformato de texto 80 x 30 ou 80 x 60, caixa de caractere 8 x 16 ou 8 x 8
    ş Designa‡ao de at‚ 256K cores a 16 atributos
    ş 1 p gina de mem¢ria de v¡deo (0)
Screen 13 (VGA ou MCGA)
    ş Ggr fico 320 x 200
    ş Formato de texto 40 x 25, caixa de caractere 8 x 8
    ş Designa‡ao de at‚ 256K cores a 256 atributos
    ş 1 p gina de mem¢ria de v¡deo (0)

See Also    \i\p\aSCREEN Statement\v@SCREEN\v\i\p
.context @screenf
:nSCREEN Function
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Fornece??? o valor ASCII ou atributo de cor de um caractere numa posi‡ao
especificada na tela.??????

\bSCREEN (row%,column% [,colorflag%])\p

    ş \brow%\p          A coordenada de linha de um caractere.
    ş \bcolumn%\p       A coordenada de coluna de um caractere.
    ş \bcolorflag%\p    Um valor (0 ou 1) que especifica o que ser  fornecido.

                    Valor             Fornecer 
                    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍ    ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
                    0 (ou omitido)    O c¢digo ASCII do caractere.
                    1                 O atributo de cor do caractere.

Example:
    CLS
    PRINT "Hello"
    PRINT "The ASCII value of character at 1,1 is"; SCREEN(1, 1)

See Also    \i\p\aPOINT\v@POINT\v\i\p    \i\p\aSCREEN Statement\v@SCREEN\v\i\p
            \i\p\aASCII Character Codes\v@ac\v\i\p    \i\p\aColor Attributes and Values\v@color.table\v\i\p
.context VIEW
.context @VIEW
:nVIEW Statement
  \i\p\aContents\v@helpTableId\v\i\p  \i\p\aIndex\v@helpIndexId\v\i\p  \i\p\aBack\v!B\v\i\p
ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
Define o tamanho e o local de uma porta de visualiza‡ao onde os gr ficos podem
ser exibidos na tela.

\bVIEW [[SCREEN] (x1!,y1!)-(x2!,y2!) [,[color%] [,border%]]]\p

    ş \bSCREEN\p                 Especifica que as coordenadas sao relativas …
                             tela ao inv‚s da porta de visualiza‡ao.
    ş \b(x1!,y1!)-(x2!,y2!)\p    As coordenadas de cantos diagonais opostos
                             da porta de visualiza‡ao.
    ş \bcolor%\p                 Um atributo de cor que define a cor de
                             preenchimento da porta de visualiza‡ao.
    ş \bborder%\p                Um atributo de cor que define a cor de borda da
                             porta de visualiza‡ao.

    ş Se todos os argumentos forem omitidos, a tela inteira ser  a porta de visualiza‡ao.
    ş Os atributos de cor dispon¡veis dependem de seu adaptador gr fico e do
      modo de tela definido pela instru‡ao SCREEN mais recente.

Example:
    'This example requires a color graphics adapter.
    SCREEN 1
    VIEW (10, 10)-(300, 180), , 1
    LOCATE 1, 11: PRINT "A big graphics viewport";
    VIEW SCREEN (80, 80)-(200, 125), , 1
    LOCATE 11, 11: PRINT "A small graphics viewport";

See Also    \i\p\aCLS\v@CLS\v\i\p    \i\p\aSCREEN\v@SCREEN\v\i\p    \i\p\aVIEW PRINT\v@vupri\v\i\p    \i\p\aWINDOW\v@WINDOW\v\i\p
            \i\p\aColor Attributes and Values\v@color.table\v\i\p    \i\p\aScreen Modes\v@screen.modes\v\i\p
