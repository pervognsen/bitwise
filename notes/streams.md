# Note

I'm taking a 3-day weekend since week 1 has been very busy. We'll be back Monday,
March 19th, same time and weekday we did the Day 0 kick-off. Most people should
have enough to do with the homework, but if you finish early you can think of cool
modifications to add to the basic language. Or you could do the abstract interpreter
for static bytecode verification that I mentioned in the homework description. Or
you could write a single-step debugger for the bytecode VM. Get creative and try
to think up fun extensions like that. One thing I've always found valuable for learning
is coming up with these kinds of throw-away mini-projects for myself to do.

Next week we will move to semidaily streams, 3 a week, since the feedback I'm getting
is that it's too hard to keep up with the video content, and I also need alone time
to focus engineering tasks. Long term I think the semidaily cadence will be good for
everyone, but let's try it and see.

# Day 3: More Programming & Parsing (March 15, 2018)
- Video: https://www.youtube.com/watch?v=L4P98pGhpnE
- Code: https://github.com/pervognsen/bitwise/blob/dbc670af1111e258b1306b25d7866122b6aacf12/ion/ion.c
- Follow up
- Continue with C programming
- Homework:

This is a multi-part exercise. I expect this to take a while for people to do,
so I won't write up new homework until people's feedback on their progress
indicates that most people have solved at least parts of it. In general, even
if you get stuck, working on the problem yourself means that when you see me
solve it on stream, you'll be ready to appreciate the solution in a way you
couldn't have been otherwise.

Remember to look up terms and concepts on Wikipedia/Google if they're unfamiliar.

Building on the parser you wrote yesterday, implement a calculator by recursively
evaluating the expression as part of the recursive descent, instead of outputting
S-expressions. [I just did this on stream.]

This is an interpreter.

Implement a bytecode stack machine for the same set of operators. The stack
machine's instructions like ADD and MUL correspond to the operators and pop their
arguments from the stack and push the result. There's also the LIT opcode, followed
in the bytecode stream by a 4-byte little endian integer, which is pushed onto the stack.
Finally, there's a HALT opcode to finish execution.

This is a virtual machine.

Code fragment to get you started or as inspiration--you don't have to do it this way.

    #define POP() \
        (*--top)

    #define PUSH(x) \
        (*top++ = (x))

    #define POPS(n) \
        assert(top - stack >= (n))

    #define PUSHES(n) \
        assert(top + (n) <= stack + MAX_STACK)

    // These assert macros are here to help with debugging. A correct compiler cannot trigger stack underflow and
    // can only trigger stack overflow if expression depth exceeds MAX_STACK, which can be checked in the compiler.
    // The Java Virtual Machine and WebAssembly specifications rely on statically verifying that kind of safety
    // property at load time for untrusted code, which lets them eliminate expensive runtime safety checks. That's
    // the best of both worlds: high performance without compromises in security or correctness. If you finish
    // the other exercises with time to spare, try to implement a static stack-effect checker for our bytecode.
    // Hint: Start with your vm_exec, remove the dynamic semantics, and just leave in the stack effect checks,
    // tracking only the changing top-of-stack offset, not the actual stack (which can then be removed). This is
    // called abstract interpretation. It's not very useful for our expression language, but it generalizes.

    int32_t vm_exec(const uint8_t *code) {
        enum { MAX_STACK = 1024 };
        int32_t stack[MAX_STACK];
        int32_t *top = stack;
        for (;;) {
            uint8_t op = *code++;
            switch (op) {
            case SUB: {
                POPS(2);
                // Note the stack's operand order!
                int32_t right = POP();
                int32_t left = POP();
                PUSHES(1);
                PUSH(left - right);
                break;
            }
            // ...
            case LIT:
                PUSHES(1);
                // Unaligned reads are legal and fast on modern PC-class processors.
                // This assumes the target architecture is little endian; as a bonus
                // exercise, figure out how to read this little endian int32 in a way
                // that's correct regardless of the target computer's native endianness.
                PUSH(*(uint32_t *)code);
                code += sizeof(uint32_t);
                break;
            case HALT:
                POPS(1);
                return POP();
            default:
                fatal("vm_exec: illegal opcode");
                return 0;
            }
        }
    }

Finally, starting with your old parser again, write a recursive-descent code generator
for this stack machine corresponding to the expression.

This is a compiler.

Congrats! Give yourself a pat on the back. You've just written a lexer, parser,
interpreter, virtual machine, compiler, all from scratch. This is a pretty
complete set of language tools, albeit for a simple language.

# Day 2: C Programming & Parsing (March 14, 2018)
- Video: https://www.youtube.com/watch?v=0woxSWjWsb8
- Code: https://github.com/pervognsen/bitwise/blob/edf8ba7aad3c53c98d289f9200585f3af76422e8/ion/ion.c
- Follow-up from yesterday
- Let's start doing a bit of programming
- Homework:

Page 9 of the Wirth book has a typo:

    E = T | A "+" T

should be

    E = T | E "+" T

Read chapter 2 through 4.1 (12 pages) of Wirth's Compiler Construction book:
https://www.inf.ethz.ch/personal/wirth/CompilerConstruction

Implement an expression parser for a simple arithmetic language:

Parse an infix expression composed of integer literals and the
following operators, highest to lowest precedence:

    unary -, unary ~    (right associative)
    * / % << >> &       (left associative)
    + - | ^             (left associative)

Output an [S-expression](https://en.wikipedia.org/wiki/S-expression) that corresponds to the parse tree, e.g.

    12*34 + 45/56 + ~25

should generate an S-expression that looks like this

    (+ (+ (* 12 34) (/ 45 56)) (~ 25))

Extra credit:

How would you support right associative binary operators like ** (exponentiation)?

How would you support parenthesized expressions for explicit grouping?

While still using recursive descent, factor out the repetitive structure so
that the parsing for operators is driven by table information for what
operators exist and their precedence and associativity.

How might you use this to implement a language that supports user defined
operator symbols with user defined precedence and associativity?

# Day 1: Introducing Ion (March 13, 2018)
- Video: https://www.youtube.com/watch?v=T6TyvsKo_KI
- Follow up to yesterday's stream, time zone confusion
- [Motivation](ion_motivation.md)
- Example code

# Day 0: Kickoff (March 12, 2018)
- Video: https://www.youtube.com/watch?v=ZjwvMcP3Nf0
- Welcome
- Expectations
- Repository: https://github.com/pervognsen/bitwise/
- Overview, with links: https://github.com/pervognsen/bitwise/blob/master/README.md
- FAQ: https://github.com/pervognsen/bitwise/blob/master/FAQ.md
- Stream production WIP
- Major FAQ items
- Structure, success criteria, sustainability, preventing burn-out
- Next steps
- Q&A
