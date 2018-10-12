**Q: What is Bitwise?**

Bitwise is my free educational project about building the software and hardware
stack for a simple computer from scratch, running on an FPGA. This includes all the
systems software, including operating system, compiler, etc, as well as the
HDL source code for the CPU, graphics chip, peripheral controllers, etc.

By watching daily or semidaily streams on Twitch, uploaded later as videos to YouTube,
reading blog posts and articles, and studying code pushed daily to GitHub, you will get
to watch over my shoulder as I build the system from scratch, starting with an empty
source control repository, and you will learn how everything works so you can build it
yourself too. Most code will be written off stream, but every new line of code will
be covered during code reviews at the beginning of each stream.

**Q: Who are you?**

I'm Per Vognsen. I've worked as a game engine programmer and systems programmer
at companies like Epic Games, NVIDIA, RAD Game Tools and Oculus for more than
15 years. In 2017, I decided to take a break from professional programming, partly
to pursue this project.

**Q: Why are you doing this project?**

I've always wanted something like this to exist, and now felt like the right time.
There have been some similar projects in the past. A big influence is Niklaus Wirth's
Oberon system, and Wirth's work in general.

**Q: What license is it released under?**

The code and associated artifacts will be released into the public domain.

**Q: How are you funding it?**

Bitwise is funded by self-sustaining savings. There is no need for financial support.

**Q: How can I help?**

For now, the biggest help would be tuning in, spreading the word, and participating
in the community. My long-term motivation will be largely driven by the community's
response.

**Q: What do I need to know in advance to follow along?**

The only hard requirement is fluency in the C programming language and understanding
the foundational ideas of computer science and computers that you'd typically pick up
as a computer science undergraduate student or as a working programmer, e.g. standard
textbook algorithms and data structures. But the more you already know, the easier it
will be. While the formal prerequisites for Bitwise are minimal, don't expect it to be
easy. Be ready to wrestle with new ideas on a recurring basis. Some topics will
out of necessity be covered at a rapid pace, with pointers to additional reading material
for people who need or want to learn more.

**Q: Do I need to spend money on hardware?**

Bitwise is designed so that you will get as close to the complete experience as possible even if
you don't own any FPGA development boards. We will be creating simulators and cycle-accurate
models of the major peripherals like DDR3 DRAM and other IO interfaces. Even if you own
FPGA boards, you wouldn't want to use them directly very often during development. That said,
if after progressing through the hardware design track you decide to spend some money to
be able to test your code on a real FPGA, we are targeting an entry-level board with a price
around $100 and a higher-end board for the serious enthusiast with a price around $400. But
it must be strongly emphasized that this is not required. More information on the recommended
boards will be available once we progress through the hardware design track.

**Q: How much time should I expect to spend per week to keep up?**

Bitwise is structured so people can participate with different levels of commitment depending
on their free time and interest. At one extreme, if you're only casually curious about Bitwise,
you might choose to just follow the blog and read the weekly summaries and articles, and maybe
watch an occasional video. That would only take about an hour per week. At the other extreme,
you could treat Bitwise like a high-intensity college course and devote several hours to it
per day, between watching the streams, studying the code, and doing your own coding. 

With the piecewise design and construction of the Bitwise software and hardware, you can decide
to drop off at any natural stopping point, having acquired some new self-contained knowledge and
skills. The goal is to have natural milestones every few weeks. And conversely, if you weren't
following from the beginning, past summaries and articles can help you fast-forward and get up
to speed quickly without having to go through all the old content if you don't want to.

It's also totally valid to follow along at your own pace without trying to keep up with the
real-time release of new material. All Bitwise materials will be kept available free of charge
long after the project has ended.

**Q: What languages will we be using?**

Most systems-level software will be written in our own C-like systems programming language, which
will be implemented in C99. We'll also be writing a smattering of RISC V assembly language by hand.
Along the way, we'll learn how to implement other kinds of languages, like Forth, Lisp, and an
embeddable scripting language. But the only language you need to know fluently in advance is C.

For hardware design, we'll be using a hardware description language (HDL) of our own design. It will
target Verilog as an intermediate language for the synthesis compiler, so we will need to learn to
read and write a basic subset of Verilog. It's not assumed you know Verilog or any other HDL in advance.

Python is used as a jack-of-all-trades language for ad-hoc scripting and prototyping. It will also be
used heavily as a language and platform for everything related to hardware design. Our custom HDL
will be implemented as an embedded domain-specific language in Python. It helps to know Python in
advance, but if you have experience with similar languages, you should be able to pick it up along the way.

**Q: What major supporting software is required?**

You need a C compiler. I will be using Visual C++ for Windows, but you can use GCC and Clang on
your operating system of choice. [Visual Studio Community](https://www.visualstudio.com/downloads/)
is free to use for individuals and contains a fully featured C/C++ compiler and IDE.

We'll be using Python 3.6 with the [Conda](https://conda.io/miniconda.html) package manager
to manage the Python environment and its package dependencies.

For Verilog, we'll be using the open source Verilator tool for cycle-accurate simulation and the free
WebPACK version of Xilinx's FPGA toolchain Vivado for everything related to Xilinx FPGAs.

**Q: Where can I can ask questions or have discussions about Bitwise?**

You use the [forums](https://bitwise.handmade.network/forums) or the [Discord](https://discord.gg/7TSA6ZF)
chat server. GitHub issues and pull requests are not an appropriate venue for discussions. Please read the
[community guidelines](GUIDELINES.md)
