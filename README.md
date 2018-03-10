# Announcing Bitwise

Feynman's blackboard quote "What I cannot create, I do not understand" never fails to give me goosebumps.

I've always been obsessed with how things work under the hood--physical contraptions as well as abstract concepts.
Most engineers and programmers I've met are motivated by building things, and their knowledge and skills are first
and foremost a tool for achieving that. But I'm motivated by learning how things work, and the process of solving
problems and designing and implementing systems has been a tool for increasing my understanding. And of course there's
the unrivaled thrill of seeing what you've built actually work and come alive!

After working as a game developer and systems programmer for over 15 years at places like Epic Games, NVIDIA, RAD Game
Tools and most recently Oculus, I decided it was time to take a break from professional programming and spend a few years
pursuing a long-time dream of mine, a project I've dubbed Bitwise, where I want to share my passion and try to demonstrate
by example how to build systems from scratch, with a low-level computing focus.

"From scratch" is a loaded phrase, so let me clarify. There's a pragmatic limit to how low we can go. Different people
have different notions of where the bottom lies. The popular term "full stack engineer" refers to programmers for whom
the stack's bottom is application-level server programming in a language like JavaScript or Python. A kernel programmer
would have qualms with that definition, and electrical engineers who design integrated circuits would in turn have qualms
with the kernel programmer. In my own progression, I started with game programming at a time where that was low level,
and have gradually expanded my understanding up and down the stack. It's only in the last few years I've really gotten
down to the nuts and bolts at the hardware design level, particularly with FPGAs and digital logic design. So
that's what will define the bottom for us in practical terms, although I do plan to explain some of the theory pertaining
to analog circuits and transistors at the IC level, so you have an idea of what we're abstracting out.

# Structure

NOTE: This is highly subject to change based on feedback from the community.

My plan is to treat Bitwise as a full-time commitment for several years. Avoiding burnout is a major priority.

I will be streaming daily or bidaily on Twitch, at least two hours per session. Videos will be posted later on YouTube.

When I'm not streaming, I will be working full-time on the project: programming, writing or doing other related activities.

I will be writing weekly or biweekly blog posts summarizing the progress since the last progress update.

Code streams will begin with a code review and walk-through of the diffs that were committed since last stream. Thus,
even though a lot of the code will necessarily be written off stream, you won't miss out on a single line of code. After
each review, I will be push the latest changes to the GitHub repository.

To keep things fun for myself and viewers, the streams will frequently alternate between different development tracks. In a
given week, streams might alternate between the primary software and hardware development tasks, but you can expect streams on
random, hopefully interesting side topics to be commonplace.

Most streams will be focused on live coding but others will be in a more traditional, semi-prepared presentation format.

Aside from the progress summary blog posts, I will try to distill what I cover on stream into standalone articles. Writing
articles is a lot of work, but my hope is that by using my explanations from the stream as a template, I will be able to
write them more quickly than if I had to start with a blank page. The audience for articles is much larger, so this will
extend our reach, and it helps prospective or lapsed viewers to catch up to the streams; it's not practical for most people
to catch up by watching archived streams. 

# Schedule

The currently planned stream schedule is Monday through Friday, 5 PM PST/8 PM EST/2 AM CET.

I'm located in Thailand, and this is my vain attempt at balancing timezone differences. I may balance it out by moving some
streams to Saturdays or Sundays during morning or noon hours for Europeans. I will be spending a month in Europe each year
in August, which will force a rotation of the schedule.

# Roadmap

NOTE: This is subject to change, but it gives you an idea of what I have planned.

We will be building not only software but hardware.

On the hardware side of things, we will be designing a computer from scratch that can be synthesized and deployed on a real
[FPGA](https://en.wikipedia.org/wiki/Field-programmable_gate_array). This will include a
[RISC-V](https://en.wikipedia.org/wiki/RISC-V), [GPU](https://en.wikipedia.org/wiki/Graphics_processing_unit),
IO interface controllers for [DDR3](https://en.wikipedia.org/wiki/DDR3_SDRAM) memory, HDMI video/audio, Ethernet networking, and more.

But before getting there, we will need to learn about digital logic and how to design hardware with a hardware description language
[HDL](https://en.wikipedia.org/wiki/Hardware_description_language). In fact, we will designing our own HDL and the associated
toolchain, including software-based simulators and debugging tools. Along the way we'll be doing lots of fun mini-projects,
like designing a version of [Pong](https://en.wikipedia.org/wiki/Pong) entirely using logic gates, and implementing a variant of the
toy parallel computer used in the Zachtronics game [TIS-100](https://en.wikipedia.org/wiki/TIS-100).

In tandem, we will also be building the software stack. This includes both the PC host-side tooling as well as the software that
runs on the Bitwise computer. Eventually most the host-side tooling will be portable to the Bitwise computer itself! The host-side
tooling will start with a simple but powerful C-like systems programming language that will initially be bootstrapped to run on the host
and which we will use to construct all the other software. As we first bring up the CPU, we will be writing the emulator, assembler,
disassembler and debugger in this language.

From there we'll be writing test programs in the assembly language to validate the CPU. Once the CPU is in a usable state, we will
code a backend for our systems language compiler that targets our CPU and so start running simple compiled programs on it. At that point
we will build a simple microcontroller-class task-switching operating system and some simple applications on top of it.

At first, the CPU will be a simple RV32IM microcontroller-class core with limited features and performance, and the only IO peripheral
will be a [UART](https://en.wikipedia.org/wiki/Universal_asynchronous_receiver-transmitter) for communicating with the host. Over time
we will make this more capable. We will extend the CPU to eventually support RISC-V's RV32G profile, which includes suppport for integer
multiply/divide and floating point instructions. To improve performance we will add instruction pipelining and branch prediction,
and caches once we have DRAM support. We will also be building out our peripherals to support audio/video output and
networking, and the software to support this will be constructed in parallel as the hardware comes online.

Eventually we will end up with a simple desktop-class operating system with a graphical user interface that can run games and other
applications we've written. We might eventually try to bring up Linux on the computer once we've implemented MMU support, but that
is not a primary goal.

My goal with Bitwise is to show that these things can be done much more simply and quickly than people realize if we strongly favor
simplicity over marginal gains in feature completeness or performance. The goal is not to outdo or compete with any existing product;
the goal is to show how things work with live, working examples.

Despite the hardware focus, I want to make it clear that nothing we're doing requires you to own any special hardware or FPGA development
boards. All our development will be done with free tools that run on a normal Windows, Linux or Mac machine. I will periodically demonstrate
code running on a real FPGA board, but it mostly be for debugging and demonstration purposes. In fact, we want to minimize the time we
spend dealing with real hardware since it's time inefficient in most cases compared to running in simulation during development.

= Links

Please follow me on Twitch, YouTube and Twitter if you want to see when streams go live and when new videos are uploaded.

I created a Discord chat server, so if you have questions or just want to hang out and chat, you can do it there.

- GitHub repository: https://github.com/pervognsen/bitwise
- Discord chat server: https://discord.gg/7TSA6ZF
- Twitch channel: https://twitch.tv/pervognsen
- YouTube channel: https://youtube.com/c/pervognsen
- Twitter: https://twitter.com/pervognsen
