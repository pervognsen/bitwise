        .reg t1 x1
        .reg t2 x2
        .reg t3 x3
        .reg t4 x4
        .reg t5 x5
        .reg t6 x6
        .reg t7 x7
        .reg t8 x8

        .reg sp x25              // Parameter stack pointer
        .reg rsp x26             // Return stack pointer
        .reg pc x27              // Program counter
        .reg xt x28              // Execution token

        .macro execute
        lw t1, [xt]
        jmp [t1]
        .endmacro

        .macro next
        lw xt, [pc]
        add pc, 4
        $execute
        .endmacro

        latest_const = 0

        link_offset = 0
        name_len_offset = 4
        name_offset = 5

        .macro defentry label, name
1:      .ptr latest_const       // link
        latest_const = <1
        .byte >3 - >2           // name_len
2:      .str $name              // name
3:
        .align 4
$label:
        .endmacro

        .macro defcode label, name
        $defentry $label, $name
        .ptr >1
1:
        .endmacro

        .macro defword label, name
        $defentry $label, $name
        .ptr docol
        .endmacro

init:   la sp, stack
        la rsp, return_stack
        la pc, program
        $next

        sub x1, 0, x1

        $defcode _find, "find"              // ( name len -- ent )
        lw t1, [sp, -8]                     // char *name = sp[-2];
        lw t2, [sp, -4]                     // uint32 len = sp[-1];
        sub sp, 4                           // sp--;
        lw t3, latest                       // entry *ent = latest;
        jmp >5                              // while (ent) {
1:          lbu t4, [t3, name_len_offset]   //     uint8 n = ent->name_len;
            bne t4, t2, >4                  //     if (len != n) goto next;
            mov t5, t1                      //     char *p1 = name;
            add t6, t3, name_offset         //     char *p2 = &ent->name;
            jmp >3                          //     while (n) {
2:              lbu t7, [t5]                //         char c1 = *p1;
                lbu t8, [t6]                //         char c2 = *p2;
                bne t7, t8, >4              //         if (c1 != c2) goto next;
                add t5, 1                   //         p1++;
                add t6, 1                   //         p2++;
                sub t4, 1                   //         n--;
3:              bne t4, 0, <2               //     }
            jmp >6                          //     goto done;
4:          lw t3, [t3, link_offset]        //     next: t3 = t3->link;
5:          bne t3, 0, <1                   // }
6:      sw [sp, -4], t3                     // done: return ent;
        $next

        $defcode _word, "word"              // ( -- addr len )
        .print $
        li t7, ' '
        li t8, '\n'
        lw t1, input                        // char *src = input;
        lw t2, input_end                    // char *end = input_end;
        add sp, 8                           // sp += 2;
                                            // skip:
1:      bne t1, t2, >2                      // if (src == end) {
            sw [sp, -8], 0                  //     sp[-2] = NULL;
            sw [sp, -4], 0                  //     sp[-1] = 0;
            jmp >5                          //     return;
                                            // }
2:      lbu t3, [t1]                        // char c = *src;
        add t1, 1                           // src++;
        beq t3, t7, <1                      // if (c == ' ') goto skip;
        beq t3, t8, <1                      // if (c == '\n') goto skip;
        la t4, word_buf                     // char *start = word_buf;
        mov t5, t4                          // char *dest = start;
3:                                          // for (;;) {
            sb [t5], t3                     //     *dest = c;
            add t5, 1                       //     dest++;
            beq t1, t2, >4                  //     if (src == end) break;
            lbu t3, [t1]                    //     c = *src;
            beq t3, t7, >4                  //     if (c == ' ') break;
            beq t3, t8, >4                  //     if (c == '\n') break;
            add t1, 1                       //     src++;
        jmp <3                              // }
4:      sw input, t1, t7                    // input = src;
        sub t5, t4                          // uint32 len = dest - start;
        sw [sp, -8], t4                     // sp[-2] = start;
        sw [sp, -4], t5                     // sp[-1] = len;
5:      $next

docol:  sw [rsp], pc
        add rsp, 4
        add pc, xt, 4
        $next

        $defcode _xt, "xt" // ( ent -- xt )
        lw t1, [sp, -4]
        lbu t2, [t1, name_len_offset]
        add t2, name_offset + 3
        add t1, t2
        and t1, ~3
        sw [sp, -4], t1
        $next

        $defcode _dup, "dup" // ( x -- x x )
        lw t1, [sp, -4]
        sw [sp], t1
        add sp, 4
        $next

        $defcode _drop, "drop" // ( x -- )
        sub sp, 4
        $next

        $defcode _putchar, "putchar" // ( x -- )
        lw t1, [sp, -4]
        sub sp, 4
        sw putchar, t1, t2
        $next

        $defcode _getchar, "getchar" // ( -- x )
        lw t1, getchar
        sw [sp], t1
        add sp, 4
        $next

        $defcode _push, "push" // ( -- x )
        lw t1, [pc]
        add pc, 4
        sw [sp], t1
        add sp, 4
        $next

        $defcode _add, "+" // ( x y -- x+y )
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sub sp, 4
        add t1, t2
        sw [sp, -4], t1
        $next

        $defcode _nez, "0<>" // ( x -- x<>0 )
        lw t1, [sp, -4]
        snez t1
        sw [sp, -4], t1
        $next

        $defcode _exit, "exit" // ( -- )
        sub rsp, 4
        lw pc, [rsp]
        $next

        $defcode _to_r, ">r" // ( x -- )
        sub sp, 4
        lw t1, [sp]
        sw [rsp], t1
        add rsp, 4
        $next

        $defcode _from_r, "r>" // ( -- x )
        sub rsp, 4
        lw t1, [rsp]
        sw [sp], t1
        add sp, 4
        $next

        $defcode _load, "@" // ( addr -- data )
        lw t1, [sp, -4]
        lw t1, [t1]
        sw [sp, -4], t1
        $next

        $defcode _store, "!" // ( data addr -- )
        lw t1, [sp, -4]
        lw t2, [sp, -8]
        sub sp, 8
        sw [t1], t2
        $next

        $defcode _execute, "execute" // ( xt )
        lw xt, [sp, -4]
        sub sp, 4
        $execute

        $defcode _0, "0" // ( -- 0 )
        add sp, 4
        sw [sp, -4], 0
        $next

        $defcode _1, "1" // ( -- 1 )
        li t1, 1
        add sp, 4
        sw [sp, -4], t1
        $next

putdigit_entry:
        $defword _putdigit, "putdigit"
        .ptr _push
        .int '0'
        .ptr _add
        .ptr _putchar
        .ptr _exit

        $defword _getdigit, "getdigit"
        .ptr _getchar
        .ptr _push
        .int -'0'
        .ptr _add
        .ptr _exit

        $defword _twice, "twice"
        .ptr _dup
        .ptr _add
        .ptr _exit

        $defword _jump, "jump"
        .ptr _from_r
        .ptr _load
        .ptr _to_r
        .ptr _exit

        .org 0x1000

stack:

        .org 0x2000

return_stack:

        .org 0x3000

latest:
        .uint32 latest_const

temp_str:
        .str "dup"

        temp_str_len = $ - temp_str

input_buf:
        .str "1 1 + twice putdigit"

input_buf_end:

input:
        .ptr input_buf                      // char *input;

input_end:
        .ptr input_buf_end                  // char *input_end;

word_buf:
        .fill 256                           // char word_buf[256];

        .org 0x4000
program:
        .ptr _word
        .ptr _find
        .ptr _xt
        .ptr _execute
        .ptr _getdigit
        .ptr _drop
        .ptr _jump
        .ptr program

        .ptr _jump
        .ptr program

        .ptr _push
        .ptr temp_str
        .ptr _push
        .int temp_str_len
        .ptr _find
        .ptr _putdigit
        .ptr _getdigit
        .ptr _drop
        .ptr _jump
        .ptr program

        .org 0xFFFFFF00
getchar:

        .org 0xFFFFFF04
putchar:

