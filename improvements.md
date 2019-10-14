Static Analysis
===

* Make sure all code paths of a function return.
* (given static typing): make sure we can't == or != things of different types, or function objects


Static Typing
===

For reasons of space I just made everything an enum, with
single instructions for Add, etc.; but for performance reasons
we should staticize things (type inference???) and emit
specialized bytecode for each type (which also tells you how
to deserialize things from the stack, which would then just
be bytes).


Technical Improvements
===

* Make line numbers not always zero
* Put in the "start-run encoding" for lines as we add opcodes
* Support >256 variables in scope (VarDefineLong and so on)
* Make Value Copy again (and Debug and so on)


QoL Improvements
=== 

* In the disassembler, have the "same as last line" thing in the printouts


Test Coverage
=== 
* Swap parse errors from panics to Results, and get some coverage of them (???)
* Direct tests of parsing (comparing AST)
* Swap compile errors from panics to Results, and get some coverage of them
* Direct tests of compiling (comparing chunks)
