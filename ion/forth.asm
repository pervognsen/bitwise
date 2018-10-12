        .reg t1 x1
        .reg t2 x2
        .reg t3 x3
        .reg t4 x4
        .reg t5 x5
        .reg t6 x6
        .reg t7 x7
        .reg t8 x8

        .reg sp x25
        .reg rp x26
        .reg pc x27
        .reg xt x28

        LATEST = 0

        FLAGS_OFFSET = 0
        LINK_OFFSET = 4
        NAMELEN_OFFSET = 8
        NAME_OFFSET = 9

        .macro defentry label, name, flags
1:      .int $flags             // flags
        .ptr LATEST             // link
        LATEST = <1
        .byte >3 - >2           // namelen
2:      .str $name              // name
3:      .align 4
$label: .endmacro

        .macro defcode label, name
        $defentry $label, $name, 0
        .ptr >1
1:      .endmacro

        .macro defword label, name
        $defentry $label, $name, 0
        .ptr docol
        .endmacro

        .macro defop2 label, name, instr
        $defcode $label, $name
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sub sp, 4
        $instr t1, t2
        sw [sp, -4], t1
        $next
        .endmacro

        .macro defop1 label, name, instr
        $defcode $label, $name
        lw t1, [sp, -4]
        $instr t1
        sw [sp, -4], t1
        $next
        .endmacro

        .macro execute
        lw t1, [xt]
        jmp [t1]
        .endmacro

        .macro next
        lw xt, [pc]
        add pc, 4
        $execute
        .endmacro

init:
        la sp, stack
        la rp, return_stack
        la pc, program
        $next

sp0:    .int stack
rp0:    .int return_stack

docol:
        sw [rp], pc
        add rp, 4
        add pc, xt, 4
        $next

        $defcode _sp0, "sp0"
        la t1, sp0
        sw [sp], t1
        add sp, 4
        $next

        $defcode _getsp, "sp@"
        sw [sp], sp
        add sp, 4
        $next

        $defcode _setsp, "sp!"
        lw sp, [sp, -4]
        $next

        $defcode _getpc, "pc@"
        sw [sp], pc
        add sp, 4
        $next

        $defcode _setpc, "pc!"
        lw pc, [sp, -4]
        sub sp, 4
        $next

        $defcode _rp0, "rp0"
        la t1, rp0
        sw [sp], t1
        add sp, 4
        $next

        $defcode _getrp, "rp@"
        sw [sp], rp
        add sp, 4
        $next

        $defcode _setrp, "rp!"
        lw rp, [sp, -4]
        sub sp, 4
        $next

        $defcode _drop, "drop"
        sub sp, 4
        $next

        $defcode _dup, "dup"
        lw t1, [sp, -4]
        add sp, 4
        sw [sp, -4], t1
        $next

        $defcode _over, "over"
        lw t1, [sp, -8]
        add sp, 4
        sw [sp, -4], t1
        $next

        $defcode _swap, "swap"
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sw [sp, -4], t1
        sw [sp, -8], t2
        $next

        $defcode _rot, "rot"
        lw t1, [sp, -12]
        lw t2, [sp, -8]
        lw t3, [sp, -4]
        sw [sp, -12], t3
        sw [sp, -8], t1
        sw [sp, -4], t2
        $next

        // : -rot  rot rot ;
        $defcode _nrot, "-rot"
        lw t1, [sp, -12]
        lw t2, [sp, -8]
        lw t3, [sp, -4]
        sw [sp, -12], t2
        sw [sp, -8], t3
        sw [sp, -4], t1
        $next

        $defcode _lit, "lit"
        lw t1, [pc]
        add pc, 4
        add sp, 4
        sw [sp, -4], t1
        $next

        $defop2 _add, "+", add
        $defop2 _sub, "-", sub
        $defop2 _and, "and", and
        $defop2 _or, "or", or
        $defop2 _xor, "xor", xor
        $defop2 _sll, "<<", sll
        $defop2 _srl, ">>", srl
        $defop1 _eqz, "0=", seqz
        $defop1 _nez, "0<>", snez

        $defcode _invert, "invert"
        lw t1, [sp, -4]
        xor t1, ~0
        sw [sp, -4], t1
        $next

        $defcode _exit, "exit"
        sub rp, 4
        lw pc, [rp]
        $next

        $defcode _tor, ">r"
        sub sp, 4
        lw t1, [sp]
        sw [rp], t1
        add rp, 4
        $next

        $defcode _fromr, "r>"
        sub rp, 4
        lw t1, [rp]
        sw [sp], t1
        add sp, 4
        $next

        $defcode _getr, "r@"
        lw t1, [rp, -4]
        sw [sp], t1
        add sp, 4
        $next

        $defcode _load, "@"
        lw t1, [sp, -4]
        lw t1, [t1]
        sw [sp, -4], t1
        $next

        $defcode _store, "!"
        lw t1, [sp, -4]
        lw t2, [sp, -8]
        sub sp, 8
        sw [t1], t2
        $next

        $defcode _cload, "c@"
        lw t1, [sp, -4]
        lbu t1, [t1]
        sw [sp, -4], t1
        $next

        $defcode _cstore, "c!"
        lw t1, [sp, -4]
        lw t2, [sp, -8]
        sub sp, 8
        sb [t1], t2
        $next

        $defcode _execute, "execute"
        lw xt, [sp, -4]
        sub sp, 4
        $execute

        $defcode _break, "break"
        lw x13, [sp, -4]
        lw x14, [sp, -8]
        lw x15, [sp, -12]
        lw x16, [sp, -16]
        .print "Set next address as breakpoint:", $
        $next

        $defcode _find, "find"              // ( name len -- ent )
        lw t1, [sp, -8]                     // char *name = sp[-2];
        lw t2, [sp, -4]                     // uint32 len = sp[-1];
        sub sp, 4                           // sp--;
        lw t3, latest                       // entry *ent = latest;
        jmp >5                              // while (ent) {
1:          lbu t4, [t3, NAMELEN_OFFSET]   //      uint8 n = ent->namelen;
            bne t4, t2, >4                  //     if (len != n) goto next;
            mov t5, t1                      //     char *p1 = name;
            add t6, t3, NAME_OFFSET         //     char *p2 = &ent->name;
            jmp >3                          //     while (n) {
2:              lbu t7, [t5]                //         char c1 = *p1;
                lbu t8, [t6]                //         char c2 = *p2;
                bne t7, t8, >4              //         if (c1 != c2) goto next;
                add t5, 1                   //         p1++;
                add t6, 1                   //         p2++;
                sub t4, 1                   //         n--;
3:              bne t4, 0, <2               //     }
            jmp >6                          //     goto done;
4:          lw t3, [t3, LINK_OFFSET]        //     next: ent = ent->link;
5:          bne t3, 0, <1                   // }
6:      sw [sp, -4], t3                     // done: sp[-1] = ent;
        $next

        $defcode _word, "word"              // ( -- addr len )
        lw t1, input                        // char *src = input;
        lw t2, input_end                    // char *end = input_end;
        la t4, word_buf                     // char *start = word_buf;
        mov t5, t4                          // char *dest = start;
        li t7, ' '
        li t8, '\n'
        add sp, 8                           // sp += 2;
        sw [sp, -8], t4                     // sp[-2] = start;
                                            // skip:
1:      bne t1, t2, >2                      // if (src == end) {
            jmp >4                          //     goto done;
                                            // }
2:      lbu t3, [t1]                        // char c = *src;
        add t1, 1                           // src++;
        beq t3, t7, <1                      // if (c == ' ') goto skip;
        beq t3, t8, <1                      // if (c == '\n') goto skip;
                                            // for (;;) {
    3:      sb [t5], t3                     //     *dest = c;
            add t5, 1                       //     dest++;
            beq t1, t2, >4                  //     if (src == end) break;
            lbu t3, [t1]                    //     c = *src;
            add t1, 1                       //     src++;
            beq t3, t7, >4                  //     if (c == ' ') break;
            beq t3, t8, >4                  //     if (c == '\n') break;
            jmp <3                          // }
4:      sw input, t1, t6                    // done: input = src;
        sub t5, t4                          // uint32 len = dest - start;
        sw [sp, -4], t5                     // sp[-1] = len;
        $next

        $defcode _cfa, ">cfa"
        lw t1, [sp, -4]
        lbu t2, [t1, NAMELEN_OFFSET]
        add t2, NAME_OFFSET + 3
        add t1, t2
        and t1, ~3
        sw [sp, -4], t1
        $next

        $defword _0, "0"
        .int _lit, 0, _exit

        $defword _1, "1"
        .int _lit, 1, _exit

        $defword _2, "2"
        .int _lit, 2, _exit

        $defword _3, "3"
        .int _lit, 3, _exit

        $defword _4, "4"
        .int _lit, 4, _exit

        // : putchar <PUTCHAR> ! ;
        $defword _putchar, "putchar"
        .int _lit, putchar, _store, _exit

        // : getchar <GETCHAR> @ ;
        $defword _getchar, "getchar"
        .int _lit, getchar, _load, _exit

        // : _latest@ latest @ ; variable latest _latest@ latest !
        $defword _latest, "latest"
        .int _lit, latest, _exit

        // variable cp  here cp !
        $defword _cp, "cp"
        .int _lit, cp, _exit

        // : here cp @ ;
        $defword _here, "here"
        .int _cp, _load, _exit

        // : allot here + cp ! ;
        $defword _allot, "allot"
        .int _here, _add, _cp, _store, _exit

        // : , here ! 4 allot ;
        $defword _comma, ","
        .int _here, _store, _4, _allot, _exit

        // : c, here c! 1 allot ;
        $defword _ccomma, "c,"
        .int _here, _cstore, _1, _allot, _exit

        // : 2dup over over ;
        $defword _2dup, "2dup"
        .int _over, _over, _exit

        // : neg 0 swap - ;
        $defword _neg, "neg"
        .int _0, _swap, _sub, _exit

        // : mux 0<> neg -rot over invert and rot and or ;
        $defword _mux, "mux"
        .int _nez, _neg, _nrot, _over, _invert, _and, _rot, _and, _or, _exit

        // : jump r> @ >r ;
        $defword _jump, "jump"
        .int _fromr, _load, _tor, _exit

        // : branch r> dup 4 + swap @ -rot mux >r ;
        $defword _branch, "branch"
        .int _fromr, _dup, _4, _add, _swap, _load, _nrot, _mux, _tor, _exit

        // ' 1+ @ constant docol
        $defword _docol, "docol"
        .int _lit, docol, _exit

        // : 1+ 1 + ;
        $defword _add1, "1+"
        .int _1, _add, _exit

        // : 1- 1 - ;
        $defword _sub1, "1-"
        .int _1, _sub, _exit

        // : aligned 3 + 3 invert and ;
        $defword _aligned, "aligned"
        .int _3, _add, _3, _invert, _and, _exit

        // : align here aligned cp ! ;
        $defword _align, "align"
        .int _here, _aligned, _cp, _store, _exit

        // : 3drop drop drop drop ;
        $defword _3drop, "3drop"
        .int _drop, _drop, _drop, _exit

        // : cmove1 2dup swap c@ swap c! swap 1+ swap 1+ ;
        $defword _cmove1, "cmove1"
        .int _2dup, _swap, _cload, _swap, _cstore
        .int _swap, _add1, _swap, _add1, _exit

        // : cmove begin dup while rot cmove1 -rot 1- repeat 3drop ;
        $defword _cmove, "cmove"
1:      .int _dup, _branch, >2, _3drop, _exit
2:      .int _rot, _cmove1, _nrot, _sub1, _jump, <1

        // : create
        //   word here
        //   0 ,
        //   latest @ , latest !
        //   dup c,
        //   here over allot swap cmove
        //   align
        //   'docol , ;
        $defword _create, "create"
        .int _word, _here
        .int _0, _comma
        .int _latest, _load, _comma
        .int _latest, _store
        .int _dup, _ccomma
        .int _here, _over, _allot, _swap, _cmove
        .int _align
        .int _docol, _comma
        .int _exit

        // : ' word find >cfa ;
        $defword _tick, "'"
        .int _word, _find, _cfa, _exit

        // : interpret ' execute ;
        $defword _interpret, "interpret"
        .int _tick, _execute, _exit

        $defword _input, "input"
        .int _lit, input, _exit

        $defword _input_end, "input-end"
        .int _lit, input_end, _exit

        HERE = $

        .assert stack - $ >= 0

        .org 0x100000

stack:

        .org 0x200000

return_stack:

        .org 0x300000

latest:
        .uint32 LATEST

cp:
        .uint32 HERE

input_buf:
        .str """
        create exit
        ' r> ,  ' drop ,  ' r> ,  ' pc! ,

        create 0
        ' lit ,  1 1 - ,  ' exit ,

        create 2
        ' lit ,  1 1 + ,  ' exit ,

        create 3
        ' lit ,  2 1 + ,  ' exit ,

        create 4
        ' lit ,  3 1 + ,  ' exit ,

        create >flags
        ' exit ,

        create >namelen
        ' lit ,  4 4 + ,  ' + ,  ' exit ,

        create >name
        ' lit ,  4 4 + 1 + ,  ' + ,  ' exit ,

        create aligned
        ' 3 ,  ' + ,  ' lit ,  3 invert ,  ' and ,  ' exit ,

        create >cfa
        ' dup ,  ' >namelen ,  ' c@ ,  ' swap ,  ' >name ,  ' + ,  ' aligned ,  ' exit ,

        create immediate?
        ' >flags ,  ' @ ,  ' 1 ,  ' and ,  ' exit ,

        create (variable)
        ' r> ,  ' exit ,

        create mode
        ' (variable) ,  ' 1 ,

        create interpret
        ' word ,  ' find ,  ' dup ,  ' >cfa ,  ' swap ,
        ' immediate? ,  ' mode ,  ' @ ,  ' or ,  ' branch ,  here 0 ,
        ' , ,  ' exit ,
        here swap !  ' execute ,  ' exit ,

        create quit
        here  ' interpret ,  ' jump ,  ,

        quit

        create immediate
        ' latest ,  ' @ ,  ' >flags ,  ' dup ,  ' @ ,  ' 1 ,  ' or ,  ' swap ,  ' ! , ' exit ,
        immediate

        create :        
        ' create ,  ' 0 , ' mode , ' ! , ' exit , 

        create ;
        ' lit , ' exit ,  ' , ,
        ' 1 , ' mode , ' ! , ' exit ,
        immediate

        : immediate-mode?
          mode @ ;

        : immediate-mode
          1 mode ! ;

        : compilation-mode
          0 mode ! ; 

        : [
          immediate-mode ; immediate

        : ]
          compilation-mode ; immediate

        : not  0= ;
        : =  - 0= ;
        : <>  = not ;

        : [']  lit lit , ' , ; immediate

        : literal  ['] lit , , ; immediate
        : 2literal  ['] lit , ,  ['] lit , , ; immediate

        : if
          ['] 0= , ['] branch , here 0 , ; immediate
        : else
          ['] jump ,  here swap  0 ,  here swap ! ; immediate
        : then
          here swap ! ; immediate

        : begin
          here ; immediate
        : again
          ['] jump , , ; immediate
        : until
          ['] 0= , ['] branch , , ; immediate

        : 2*  1 << ;
        : 4*  2 << ;
        : 8*  3 << ;

        : 8  1 8* ;
        : 256  1 8 << ;

        : input?
          input @ input-end @ <> ;

        : key
          input? if
            input @ dup c@ swap 1+ input !
          else
            getchar
          then ;

        : (variable)
          r> ;

        : char
          ['] lit ,  key , ; immediate

        : bl
          char  ;
        
        : nl
          char 
        ;

        : cr
          nl putchar ;

        : ."
          begin
            key
            dup char " = if  drop exit  then
            immediate-mode? if 
              putchar
            else
              ['] lit ,  ,  ['] putchar ,
            then
          again ; immediate

        : test."
          ." Hello, world!" cr ;

        test."
        ." Goodbye, world!" cr

        : 2drop
          drop drop ;

        : type
          begin
            over c@ putchar
            swap 1+ swap 1-
          dup 0= until
          2drop ;

        : blank?
          dup bl = swap nl = or ;

        create wordbuf
        ' (variable) ,  256 allot

        : word
          wordbuf dup
          begin
            key
            dup blank? not if
              begin
                over c! 1+
                key 
              dup blank? until 
              drop  over -  exit
            then
            drop
          again ;

        : false
          0 ;

        : true
          1 ;

        : putdigit
          char 0 + putchar ;

        : getdigit
          getchar char 0 - ;

        : equal
          -rot over <> if  3drop false exit  then
          begin
            dup 0= if  3drop true exit  then    
            rot over c@ over c@ <> if  3drop false exit  then
            1+ swap 1+ swap -rot 1-
          again ;

        : >link
          4 + ;

        : name
          dup >name swap >namelen c@ ;

        : find
          latest @ >r
          begin
            r@ 0= if  2drop r> break exit  then
            2dup  r@ >name  r@ >namelen c@  equal if  2drop r> break exit  then
            r> >link @ >r
          again ;

        : interpret
          word find dup >cfa swap
          immediate? immediate-mode? or if  execute  else  ,  then ;

        : quit
          begin
            interpret
          again ;

        : abort
          sp0 @ sp!
          rp0 @ rp!
          quit ;

        abort

        : align
          here aligned cp ! ;

        : cell
          4 ;

        : cells
          4* ;

        : here
          cp @ ;
        
        : allot
          here + cp ! ;  
        
        : ,
          here !  cell allot ;
        
        : c,
          here c!  1 allot ;

        : cmove1
          2dup swap c@ swap c!
          swap 1+ swap 1+ ;

        : cmove
          begin
            dup 0= if  3drop exit  then
            rot cmove1 -rot 1-
          again ;

        : (create)
          here
          0 ,
          latest @ ,  latest !
          dup c,
          here  over allot  swap cmove
          align
          docol , ;

        : create
          word (create) ;

        : :
          create
          compilation-mode ;

        : '
          word find >cfa ;

        : [']
          lit lit , ' , ; immediate

        : variable
          create  ['] (variable) ,  0 , ; immediate
        
        : constant
          create  ['] lit ,  ,  ['] exit , ; immediate

        : postpone
          word find dup >cfa swap
          immediate? if
            ,
          else
            ['] lit ,  ,  ['] , ,
          then ; immediate

        : nip
          swap drop ;

        : cfa>
          latest @
          begin
            dup 0= if  nip exit  then
            over over >cfa = if  nip exit  then
            >link @
          again ;

        : my-+
          postpone + ; immediate

        : endif
          postpone then ; immediate

        : unless
          postpone not postpone if ; immediate

        : :noname
          0 0 (create)
          compilation-mode
          latest @ >cfa ;

        :noname 2 3 + putdigit cr ; execute

        : test-postpone
          4 4 my-+ putdigit cr
          1 unless  1  else  2  endif putdigit cr
          1 if  1  else  2  endif putdigit cr ;

        test-postpone

        variable counter
        3 constant three

        three putdigit
        counter @ putdigit
        getdigit counter !
        counter @ putdigit

        : bar
          begin
            getdigit
            dup 0= if 1+ else 1- then putdigit
          again ;
        """

input_buf_end:

        .align 4

input:
        .ptr input_buf                      // char *input;

input_end:
        .ptr input_buf_end                  // char *input_end;

word_buf:
        .fill 256                           // char word_buf[256];

        .assert program - $ >= 0

        .org 0x400000
program:
        .int _interpret
        .int _jump, program

        .org 0xFFFFFF00
getchar:

        .org 0xFFFFFF04
putchar:
