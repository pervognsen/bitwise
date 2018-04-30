# Bitwise, Day 23: RISC-V Toolchain Implementation
- Video: https://www.youtube.com/watch?v=zDOd3inaOsY
- Extra: https://www.youtube.com/watch?v=kRmvjEpziuM
- Overview of basic toolchain components
- Jump into coding immediately

# Bitwise, Day 22: RISC-V Toolchain
- Video: https://www.youtube.com/watch?v=g6UJGcrQNY8
- Review
- Start coding

# Bitwise, Day 21: Packages Demo & RISC-V Intro
- Video: https://www.youtube.com/watch?v=dvaTa6UHR48
- Demo packages and other compiler changes
- RISC-V intro
- Instruction set manual: https://github.com/riscv/riscv-isa-manual/blob/master/release/riscv-spec-v2.2.pdf
- Plan for coming period

# Bitwise, Day 20: Packages
- Video: https://www.youtube.com/watch?v=AVrGUfymPQs
- Extra: https://www.youtube.com/watch?v=N6ndXk3DVg0
- Review diffs
- Package design
- Start working on implementation

# Bitwise, Day 19: Noir Demo & Dynamic Type Info
- Video: https://www.youtube.com/watch?v=PfDIP96xEdM
- Extra: https://www.youtube.com/watch?v=4i12O0S_Vjo
- Demo of Noir so far
- Ion dynamic type info

# Bitwise, Day 18: Making Noir
- Review/follow-up
- Noir's genesis in experimental Mu library: https://gist.github.com/pervognsen/6a67966c5dc4247a0021b95c8d0a7b72
- Old Mu videos: https://www.youtube.com/watch?v=NG_mUhc8LRw, https://www.youtube.com/watch?v=pAIdfsT7-EU
- Start working on Noir

# Bitwise, Day 17: Ion Version 0
- Video: https://www.youtube.com/watch?v=Oeqeqw4OHPE
- Extra: https://www.youtube.com/watch?v=DoPG4OCnh7A
- Plan for this week
- Review
- Homework
- Start work on Ion library/application

Homework: Choose-Your-Own-Adventure Compiler Hacking

Implement as many of the following features as you like in the Ion compiler (either the official Ion compiler or your own version). Make sure to reuse the existing functionality as much as possible. Each feature should take no more than ~50 lines of code to implement. If you run out of stuff to do, you can play around with adding other lightweight language extensions with a similar level of implementation complexity. 

    if (file := fopen("foo.txt", "r")) {
        // ...
    }
    =>
    {
        FILE *file = fopen("foo.txt", "r");
        if (file) {
            // ...
        }
    }

    if (n := fread(...); n != size) {
        // ...
    }
    =>
    {
        size_t n = fread(...);
        if (n != size) {
            // ...
        }        
    }

    while (x := init(); !done(x)) {
        // ...
    }
    =>
    {
        X x = init();
        while (!done(x)) {
            // ...
        }
    }

Extra credit:

Here is a more complicated feature you can try to implement if you want more of a challenge.

Defer:
- Defer statements have stack-order deferred execution.
- Type checked/resolved at definition site.
- Disallow returns, breaks, continues, defers in defer bodies, inits must be in blocks.

The easiest implementation of the code generation is to have a Stmt* stack of deferred statements.
You need to maintain pointers into the stack to track the top of the stack, the innermost enclosing
block and the innermost enclosing loop so you can determine what range of deferred statements to
generate when exiting scopes either normally or via return, break and continue.

For the sanity checking of defer bodies, you can add a bool deferred parameter to the resolve_stmt
family of functions so the different cases can check whether they're legal in the context of
a defer body. So STMT_BREAK, STMT_CONTINUE, STMT_RETURN, STMT_DEFER would be illegal if deferred
is set to true. Most statements just propagate the deferred flag down to the recursive calls,
except STMT_DEFER, which will pass true.

Example:

    window := create_window();
    defer destroy_window(window);
    while (...) {
        file := fopen("foo.txt", "r");
        defer fclose(file);
        if (...) {
            return 0;
        }
        if (...) {
            continue;
        }
        if (...) {
            mem := malloc(get_file_size(file));
            defer free(mem);
            if (...) {
                break;
            }
        }
    }
    return 0;
    =>
    Window *window = create_window();
    while (...) {
        FILE *file = fopen("foo.txt", "r");
        if (...) {
            fclose(file);
            destroy_window(window);
            return 0;
        }
        if (...) {
            fclose(file);
            continue;
        }
        if (...) {
            void *mem = malloc(get_file_size(file));
            if (...) {
                free(mem);
                fclose(file);
                break;
            }
            free(mem);
        }
        fclose(file);
    }
    destroy_window(window);
    return 0;

# Bitwise, Day 16: Weekend Edition
- Video: https://www.youtube.com/watch?v=jNbar0lj93g
- Review
- Random stuff

# Bitwise, Day 15: More Compiler Hacking
- Video: https://www.youtube.com/watch?v=s5_RV3y4L18
- Review diffs
- Continue coding

# Day 14: Types Revisited
- Video: https://www.youtube.com/watch?v=Dq2gKTdL1uI
- Extra: https://www.youtube.com/watch?v=amSiLEFcjq4
- More homework coming soon
- Review diffs
- Start filling in the rest of the type system

# Day 13: More Code Generation
- Video, Part 1: https://www.youtube.com/watch?v=uIjd094EKPM
- Video, Part 2: https://www.youtube.com/watch?v=PVnRVadB7g0
- Follow-up on failed optimization from last time
- Continue work on code generation

# Day 12: More Optimization & Clean-Up
- Video: https://www.youtube.com/watch?v=Hf5PevrUx4g
- Fun: http://nothings.org/computer/judy/
- Review code from extra stream
- Notes on hash tables
- More profiling and coding

# Day 11: End-to-End Workflow & Clean-Up
- Video: https://www.youtube.com/watch?v=Y4ykmb9zdJM
- Extra: https://www.youtube.com/watch?v=Fj4g7HrjZBU
- Quick review of code from extra stream
- Next steps
- Start coding

# Day 10: C Code Generation
- Video: https://www.youtube.com/watch?v=X9YWYlp8iQg
- Extra: https://www.youtube.com/watch?v=C4H8UJDXfak
- Overview
- C declaration syntax
- Start coding

# Day 9: Functions & Statements
- Video: https://www.youtube.com/watch?v=VRMxHYuW2BY
- Fun: [Compiler Construction - The Art of Niklaus Wirth](https://pdfs.semanticscholar.org/036f/c4effda4bbbe9f6a9ee762df717bd0af1324.pdf)
- Diff review
- Continue coding

# Day 8: Type Checking/Inference, Constant Evaluation (March 28, 2018)
- Video: https://www.youtube.com/watch?v=-eCwBwTbjAI
- Extra: https://www.youtube.com/watch?v=ls_YmJ21JZg
- Why separated algorithm from last time can't always work
- Demo code for new approach that does work
- Continue coding

# Day 7: More Order-Independent Declarations (March 26, 2018)
- Video: https://www.youtube.com/watch?v=4AOnbPDyQXw
- New: Git tags for each day, e.g. git checkout day6
- New: [Annotated Episode Guide](https://bitwise.handmade.network/episode/bitwise) by [Miblo del Carpio](https://twitter.com/miblo_)
- Please give feedback (how useful are the annotations, etc) on [forums](https://bitwise.handmade.network/forums)
- Review diffs
- Continue coding

# Day 6: Order-Independent Declarations (March 23, 2018)
- Video: https://www.youtube.com/watch?v=0WpCnd9E-eg
- Code: https://github.com/pervognsen/bitwise/blob/63060b2e009a00b73f93c4c5ae010697c9634a4b/ion/resolve.c
- Quick diff review
- Start on order-independent declarations

# Day 5: Ion Parser/AST Code Review (March 21, 2018)
- Video: https://www.youtube.com/watch?v=YvoyKQYNy20
- "Working" parser and AST printer, needs more test coverage
- Review/explain major high points of new code, design choices

# Day 4: Lexer, Parser, AST Marathon (March 19, 2018)
- Video: https://www.youtube.com/watch?v=bM_JOa-dadY
- Extra: https://www.youtube.com/watch?v=t0YAOfZcSfw
- Semidaily streams (Monday/Wednesday/Friday), random bonus streams
- New forums for discussions/questions: https://bitwise.handmade.network/forums
- Alternative video/audio download option: https://bitwise.handmade.network/forums/t/2999-bitwise_videos_on_resilio_sync
- Annotated videos hopefully coming soon
- Diff: https://github.com/pervognsen/bitwise/commit/147e36d9158ed34ea757b071fa7fe2b08186e0e2
- Today: Coding marathon on stream, less talking, more coding, see how much we can get done

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
to focus on engineering tasks. Long term I think the semidaily cadence will be good for
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
