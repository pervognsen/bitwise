        .reg sp x1
        .reg rsp x2
        .reg pc x3

        .reg t1 x8
        .reg t2 x9
        .reg t3 x10
        .reg t4 x11

        .macro next
        lw t1, [pc]
        add pc, 4
        lw t2, [t1]
        jmp [t2]
        .endmacro

        .macro defword name
$name:
        .uint32 >1
1:
        .endmacro

init:
        la sp, stack_start
        la pc, program_start
        $next

        $defword do_dup
        lw t1, [sp, -4]
        sw [sp], t1
        add sp, 4
        $next

        $defword do_putchar
        lw t1, [sp, -4]
        sub sp, 4
        sw putchar, t1, t2  
        $next

        $defword do_getchar
        lw t1, getchar
        sw [sp], t1
        add sp, 4
        $next

        $defword do_push
        lw t1, [pc]
        add pc, 4
        sw [sp], t1
        add sp, 4
        $next

        $defword do_add
        lw t1, [sp, -8]
        lw t2, [sp, -4]
        sub sp, 4
        add t1, t2
        sw [sp, -4], t1
        $next

docol:
        sw [rsp], pc
        add rsp, 4
        add pc, t1, 4
        $next

        $defword do_exit
        sub rsp, 4
        lw pc, [rsp]
        $next

        .org 0x1000
stack_start:

        .org 0x2000
program_start:
        .uint32 do_getchar
        .uint32 do_push, -'0'
        .uint32 do_add
        .uint32 do_twice
        .uint32 do_twice
        .uint32 do_push, '0'
        .uint32 do_add
        .uint32 do_putchar
        .uint32 do_getchar
        .uint32 do_push, -'0'
        .uint32 do_add
        .uint32 do_getchar
        .uint32 do_push, -'0'
        .uint32 do_add
        .uint32 do_add
        .uint32 do_push, '0'
        .uint32 do_add
        .uint32 do_putchar
        .uint32 do_getchar
        .uint32 do_exit

do_twice:
        .uint32 docol
1:      .uint32 do_dup
        .uint32 do_add
        .uint32 do_exit

    .org 0xFFFFFF00
getchar:

    .org 0xFFFFFF04
putchar:
