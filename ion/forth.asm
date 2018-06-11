        .reg sp x1              // Parameter stack pointer
        .reg rsp x2             // Return stack pointer
        .reg pc x3              // Program counter
        .reg xt x4              // Execution token

        .reg t1 x8
        .reg t2 x9
        .reg t3 x10
        .reg t4 x11
        .reg t5 x12
        .reg t6 x13
        .reg t7 x14
        .reg t8 x15

        .macro next
        lw xt, [pc]
        add pc, 4
        lw t1, [xt]
        jmp [t1]
        .endmacro

        latest_const = 0

        link_offset = 0
        name_len_offset = 4
        name_offset = 5

        .macro defentry label, name
1:      .uint32 latest_const    // link
        latest_const = <1
        .uint8 >3 - >2          // name_len
2:      .str $name              // name
3:
        .align 4
$label:
        .endmacro

        .macro defcode label, name
        $defentry $label, $name
        .uint32 >1
1:
        .endmacro

        .macro defword label, name
        $defentry $label, $name
        .uint32 docol
        .endmacro

init:   la sp, stack
        la rsp, return_stack
        la pc, program
        $next

        $defcode _find, "find"              // ( addr len -- entry found? )
        lw t1, [sp, -8]                     // char *t1 = addr;
        lw t2, [sp, -4]                     // uint32 t2 = len;
        lw t3, latest                       // entry *t3 = latest;
        jmp >5                              // while (t3) {
1:          lbu t4, [t3, name_len_offset]   //     uint8 t4 = t3->name_len;
            bne t4, t2, >4                  //     if (t4 != t2) goto next;
            mov t5, t1                      //     char *t5 = t1;
            add t6, t3, name_offset         //     char *t6 = &t3->name;
            jmp >3                          //     while (t4) {
2:              lbu t7, [t5]                //         char t7 = *t5;
                lbu t8, [t6]                //         char t8 = *t6;
                bne t7, t8, >4              //         if (t7 != t8) goto next;
                add t5, 1                   //         t5++;
                add t6, 1                   //         t6++;
                sub t4, 1                   //         t4--;
3:              bne t4, 0, <2               //     }
            li t1, 1                        //     return {1, t3};
            sw [sp, -4], t1
            sw [sp, -8], t3
            $next                           
4:          lw t3, [t3, link_offset]        //     next: t3 = t3->link;
5:          bne t3, 0, <1                   // }
        sw [sp, -4], 0                      // return {0, NULL};
        sw [sp, -8], 0
        $next

docol:  sw [rsp], pc
        add rsp, 4
        add pc, xt, 4
        $next

        $defcode _dup, "dup"
        lw t1, [sp, -4]
        sw [sp], t1
        add sp, 4
        $next

        $defcode _drop, "drop"
        sub sp, 4
        $next

        $defcode _putchar, "putchar"
        lw t1, [sp, -4]
        sub sp, 4
        sw putchar, t1, t2  
        $next

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

        $defcode _add, "add"
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sub sp, 4
        add t1, t2
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

        .org 0x1000

stack:

        .org 0x2000

return_stack:

        .org 0x3000

latest: .uint32 latest_const

temp_str:
        .str "asdf"

        temp_str_len = $ - temp_str

        .org 0x4000
program:
        .uint32 _push
        .uint32 temp_str
        .uint32 _push
        .uint32 temp_str_len
        .uint32 _find
        .uint32 _putdigit
        .uint32 _getdigit
        .uint32 _drop
        .uint32 _jump
        .uint32 program

        $defword _putdigit, "putdigit"
        .uint32 _push
        .uint32 '0'
        .uint32 _add
        .uint32 _putchar
        .uint32 _exit

        $defword _getdigit, "getdigit"
        .uint32 _getchar
        .uint32 _push
        .uint32 -'0'
        .uint32 _add
        .uint32 _exit

        $defword _twice, "twice"
        .uint32 _dup
        .uint32 _add
        .uint32 _exit

        $defword _jump, "jump"
        .uint32 _from_r
        .uint32 _load
        .uint32 _to_r
        .uint32 _exit

        .org 0xFFFFFF00
getchar:

        .org 0xFFFFFF04
putchar:
