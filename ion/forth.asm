        .reg t1 x1
        .reg t2 x2
        .reg t3 x3
        .reg t4 x4
        .reg t5 x5
        .reg t6 x6
        .reg t7 x7
        .reg t8 x8

        .reg sp x25
        .reg rsp x26
        .reg pc x27
        .reg xt x28

        .macro execute
        lw t1, [xt]
        jmp [t1]
        .endmacro

        .macro next
        lw xt, [pc]
        add pc, 4
        $execute
        .endmacro

        LATEST = 0

        FLAGS_OFFSET = 0
        LINK_OFFSET = 4
        NAME_LEN_OFFSET = 8
        NAME_OFFSET = 9

        IMMEDIATE = 1

        .macro defentry label, name, flags
1:      .int $flags             // flags
        .ptr LATEST             // link
        LATEST = <1
        .byte >3 - >2           // name_len
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

        .macro defword_immediate label, name
        $defentry $label, $name, IMMEDIATE
        .ptr docol
        .endmacro

init:   lw t1, latest
        lw t1, [t1, FLAGS_OFFSET]
        la sp, stack
        la rsp, return_stack
        la pc, program
        $next

docol:  sw [rsp], pc
        add rsp, 4
        add pc, xt, 4
        $next

        $defcode _find, "find"              // ( name len -- ent )
        lw t1, [sp, -8]                     // char *name = sp[-2];
        lw t2, [sp, -4]                     // uint32 len = sp[-1];
        sub sp, 4                           // sp--;
        lw t3, latest                       // entry *ent = latest;
        jmp >5                              // while (ent) {
1:          lbu t4, [t3, NAME_LEN_OFFSET]   //     uint8 n = ent->name_len;
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

        $defcode _key, "key"
        lw t1, input
        lw t2, input_end
        beq t1, t2, >1
        lw t3, [t1]
        add t1, 1
        sw input, t1, t2
        jmp >2
1:      lw t3, getchar
2:      add sp, 4
        sw [sp, -4], t1
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

        // : >flags 0 + ;
        // : >link 4 + ;
        // : >namelen 8 + ;
        // : >name 9 + ;
        // : >cfa dup >namelen @ + 3 + 3 not and ;
        // : >dfa >cfa 4 + ;

        $defcode _flags, ">flags" // ( ent -- flags )
        lw t1, [sp, -4]
        add t1, FLAGS_OFFSET
        sw [sp, -4], t1
        $next

        $defcode _cfa, ">cfa" // ( ent -- cfa )
        lw t1, [sp, -4]
        lbu t2, [t1, NAME_LEN_OFFSET]
        add t2, NAME_OFFSET + 3
        add t1, t2
        and t1, ~3
        sw [sp, -4], t1
        $next

        $defword _dfa, ">dfa" // ( ent -- dfa )
        .int _cfa, _4, _add, _exit

        $defcode _dup, "dup" // ( x -- x x )
        lw t1, [sp, -4]
        add sp, 4
        sw [sp, -4], t1
        $next

        $defcode _over, "over" // ( x y -- x y x )
        lw t1, [sp, -8]
        add sp, 4
        sw [sp, -4], t1
        $next

        $defcode _swap, "swap" // ( x y -- y x )
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sw [sp, -4], t1
        sw [sp, -8], t2
        $next

        $defcode _rot, "rot" // ( x y z -- z x y )
        lw t1, [sp, -12]
        lw t2, [sp, -8]
        lw t3, [sp, -4]
        sw [sp, -12], t3
        sw [sp, -8], t1
        sw [sp, -4], t2
        $next

        $defcode _nrot, "-rot" // ( x y z -- y z x )
        lw t1, [sp, -12]
        lw t2, [sp, -8]
        lw t3, [sp, -4]
        sw [sp, -12], t2
        sw [sp, -8], t3
        sw [sp, -4], t1
        $next

        $defcode _drop, "drop" // ( x -- )
        sub sp, 4
        $next

        // : putchar <PUTCHAR> 0 ;
        $defcode _putchar, "putchar"
        lw t1, [sp, -4]
        sub sp, 4
        sw putchar, t1, t2
        $next

        // : getchar <GETCHAR> @ ;
        $defcode _getchar, "getchar"
        lw t1, getchar
        sw [sp], t1
        add sp, 4
        $next

        $defcode _push, "push"
        lw t1, [pc]
        add pc, 4
        sw [sp], t1
        add sp, 4
        $next

        $defcode _add, "+"
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sub sp, 4
        add t1, t2
        sw [sp, -4], t1
        $next

        $defcode _sub, "-"
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sub sp, 4
        sub t1, t2
        sw [sp, -4], t1
        $next

        $defcode _and, "and"
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sub sp, 4
        and t1, t2
        sw [sp, -4], t1
        $next

        $defcode _xor, "xor"
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sub sp, 4
        xor t1, t2
        sw [sp, -4], t1
        $next

        $defcode _not, "not"
        lw t1, [sp, -4]
        xor t1, ~0
        sw [sp, -4], t1
        $next

        $defcode _or, "or"
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sub sp, 4
        or t1, t2
        sw [sp, -4], t1
        $next

        $defcode _eqz, "0="
        lw t1, [sp, -4]
        seqz t1
        sw [sp, -4], t1
        $next

        $defcode _nez, "0<>"
        lw t1, [sp, -4]
        snez t1
        sw [sp, -4], t1
        $next

        $defcode _exit, "exit"
        sub rsp, 4
        lw pc, [rsp]
        $next

        $defcode _to_r, ">r"
        sub sp, 4
        lw t1, [sp]
        sw [rsp], t1
        add rsp, 4
        $next

        $defcode _from_r, "r>"
        sub rsp, 4
        lw t1, [rsp]
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

        $defcode _0, "0"
        add sp, 4
        sw [sp, -4], 0
        $next

        $defcode _1, "1"
        li t1, 1
        add sp, 4
        sw [sp, -4], t1
        $next

        $defcode _2, "2"
        li t1, 2
        add sp, 4
        sw [sp, -4], t1
        $next

        $defcode _3, "3"
        li t1, 3
        add sp, 4
        sw [sp, -4], t1
        $next

        $defcode _4, "4"
        li t1, 4
        add sp, 4
        sw [sp, -4], t1
        $next

        $defcode _break, "break"
        lw x13, [sp, -4]
        lw x14, [sp, -8]
        lw x15, [sp, -12]
        lw x16, [sp, -16]
        .print "Set next address as breakpoint"
        .print $
        $next

        // : '0' 48 ;

        // : putdigit '0' add putchar ;
        $defword _putdigit, "putdigit"
        .int _push, '0', _add, _putchar, _exit

        // : getdigit getchar '0' sub ;
        $defword _getdigit, "getdigit"
        .int _getchar, _push, -'0', _add, _exit

        // latest @  variable latest  latest !
        $defword _latest, "latest"
        .int _push, latest, _exit

        // here  variable cp  cp !
        $defword _cp, "cp"
        .int _push, cp, _exit

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

        // : mux <>0 neg -rot over not and rot and or ;
        $defword _mux, "mux"
        .int _nez, _neg, _nrot, _over, _not, _and, _rot, _and, _or, _exit

        // : jump r> @ >r ;
        $defword _jump, "jump"
        .int _from_r, _load, _to_r, _exit

        // : branch r> dup 4 add swap @ -rot mux >r ;
        $defword _branch, "branch"
        .int _from_r, _dup, _4, _add, _swap, _load, _nrot, _mux, _to_r, _exit

        // ' 1+ @ constant docol
        $defword _docol, "docol"
        .int _push, docol, _exit

        // : 1+ 1 + ;
        $defword _add1, "1+"
        .int _1, _add, _exit

        // : 1- 1 - ;
        $defword _sub1, "1-"
        .int _1, _sub, _exit

        // : aligned 3 add 3 not and ;
        $defword _aligned, "aligned"
        .int _3, _add, _3, _not, _and, _exit

        // : align here aligned cp ! ;
        $defword _align, "align"
        .int _here, _aligned, _cp, _store, _exit

        // : 3drop drop drop drop ;
        $defword _3drop, "3drop"
        .int _drop, _drop, _drop, _exit

        // variable mode  1 mode !
        $defword _mode, "mode"
        .int _push, mode, _exit

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

        // : immediate latest @ >flags dup @ 1 or swap ! ; immediate
        $defword_immediate _immediate, "immediate"
        .int _latest, _load, _flags, _load
        .int _push, IMMEDIATE, _or
        .int _latest, _load, _flags, _store, _exit

        // : immediate? >flags @ 1 and 0<> ;
        $defword _isimmediate, "immediate?"
        .int _flags, _load, _push, IMMEDIATE, _and, _nez, _exit

        // : ' word find cfa> ;
        $defword _quote, "'"
        .int _word, _find, _cfa, _exit

        // : interpret
        //   word find dup >cfa swap
        //   immediate? mode @ or if execute else , then ;
        $defword _interpret, "interpret"
        .int _word, _find, _dup, _cfa, _swap
        .int _isimmediate, _mode, _load, _or, _branch, >1
        .int _comma, _exit
1:      .int _execute, _exit

        CP = $

        .assert stack - $ >= 0

        .org 0x1000

stack:

        .org 0x2000

return_stack:

        .org 0x3000

latest:
        .uint32 LATEST

cp:
        .uint32 CP

mode:
        .uint32 1

input_buf:
        .str """
        create :        
        ' create ,  ' 0 , ' mode , ' ! , ' exit , 

        create ;
        ' push , ' exit ,  ' , ,
        ' 1 , ' mode , ' ! , ' exit ,
        immediate

        : [ 1 mode ! ; immediate
        : ] 0 mode ! ; immediate

        : ['] push push , ' , ; immediate

        : if ['] 0= , ['] branch , here 0 , ; immediate
        : then here swap ! ; immediate

        : begin here ; immediate
        : again ['] jump , , ; immediate
        : until ['] 0= , ['] branch , , ; immediate

        : (variable) r> ;
        : variable create ['] (variable) , 0 , ; immediate
        : constant create ['] push , , ['] exit , ; immediate

        variable counter
        3 constant three

        three putdigit
        counter @ putdigit
        getdigit counter !
        counter @ putdigit

        : foo begin 1 putdigit again ;

        : bar
          begin
            getdigit
            dup if 1+ putdigit then
          again ;

        bar
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

        .org 0x4000
program:
//        .int _getchar, _drop
        .int _interpret
//        .int _break
        .int _jump, program

        .org 0xFFFFFF00
getchar:

        .org 0xFFFFFF04
putchar:
