# Day 2: C Programming & Parsing (March 14, 2018)
- Follow-up from yesterday
- Let's start doing a bit of programming
- Homework:

Read chapter 2 through 4.1 (12 pages) of Wirth's Compiler Construction book:
https://www.inf.ethz.ch/personal/wirth/CompilerConstruction/index.html

Implement an expression parser for a simple arithmetic language:

Parse an infix expression composed of integer literals and the
following operators, highest to lowest precedence:

	unary -, unary ~	(right associative)
	* / % << >> & 		(left associative)
	+ - | ^ 			(left associative)

Output an S-expression that corresponds to the parse tree, e.g.

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
