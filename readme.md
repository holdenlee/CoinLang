# CoinLang #

CoinLang gives a way to write Bitcoin scripts in a higher-level functional programming language (namely, Haskell) rather than the stack-based language. Because it is more intuitive and human-readable, CoinLang makes it easier to program complex scripts such as multiplayer lotteries. Hopefully it will encourage more experimentation with complex scripts to carry out contracts, etc.

# Writing in CoinLang #

For example, for a standard transaction, rather than write

	OP_DUP OP_HASH160 <pk hash> OP_EQUALVERIFY OP_CHECKSIG

one can write the following:

	std_trans' pkh =
     script [
      inputs 2,
      ver [arg 0, arg 1],
      eq [con pkh, hash [arg 1]]]

or alternatively,

	std_trans' pkh =
	  inputs 2 .>
      ver [arg 0, arg 1] .>
      eq [con pkh, hash [arg 1]]

This script takes 2 arguments (i.e., it assumes there are already 2 arguments on the stack), verifies that the 0th (bottom) argument is a signature for the public key given by the 1st (top) argument, and then checks that the pkh (public key hash) equals the hash of the 1st argument.

All programs are written as a Haskell file, so syntax follows Haskell. (To learn Haskell see https://www.haskell.org/haskellwiki/Learning_Haskell) First, name the function and give it some number of parameters:

	std_trans' pkh =

A script is internally represented as a "Circuit2 Int", which you can think of as commands. To make a script we typically sequence a bunch of these together, using "script [...,...]" or using ".>" after each individual command.

The first thing to do is specify the number of inputs (the number of blocks that are on the stack already). You can do this either by

	inputs n

or by naming the inputs explicitly,

	inputvars ["x","y","z"]

See ScriptDefs.hs for details on how to make functions out of the opcodes. 

	ver = makeFun "OP_CHECKSIG" False
	eq = makeFun "OP_EQUAL" True
	(.=) x y = eq [x,y]

Here, we define "verify" to be the function associated with OP\_CHECKSIG and "eq
the function associated with OP\_EQUAL. We declare (.=) to be infix for OP\_EQUAL.

We use these functions by passing them lists of arguments--so argument lists within scripts are written with [...,...] rather than (...,...). To refer to an argument, write "arg n" for argument n (counting from 0). Constants that are used as arguments must be flagged with con, as in "con pkh". (Otherwise you will get a compilation error.) To refer to variables you've set, say x, write

	var "x"

To set a variable, write

	set "x"

in the line immediately after the value you want to set it to. For example, in this script

	s = var "s"
	inS' k n = 
		script [
			inputs 1,
			size [arg 0],
			set "s",
			within [s, con (8*k), con (8*(k+n-1)+1)],
			(s .% con 8) .= con 0]

s is set to the size and referred to in the next 2 lines. Note how we defined s=var "s" outside of the function definition, so that we can refer to "s" by just "s" rather than having to write 'var "s"'.

# Advanced #

Every expression that is not later referenced will be verified; i.e., the script will check if the expression is true and fail if it is not. It will leave just one of the expression results on the stack, because a successful script leaves a value of True. (If you want the script to leave a specific numeric value, then be sure that it is the only expression at top level.)

You can use the full power of Haskell to construct scripts. In particular, if you want sum a variable number of arguments, say n, you can do (see the definition of sums in ScriptDefs)

	inputs n,
	sums (map arg [0..(n-1)]),
	...

map applies a function to a list. 

To use a user-defined script as a subroutine in another, you must first make it into a function as follows:

	verify x = fun $ verify' x

($ is Haskell for "parentheses around the rest of the expression.") Here verify' took 1 argument, so we have to include it when defining verify. Then we can use it like

	verify [arg 0, arg 2]

for instance in any other script.

If-then-else scripts are more complicated. The simplest way to use them is using ifscript n f g h

	ifscript 3 (arg 0)
		       [ver [arg 1, con pka]]
			   [ver [arg 2, con pkb]]

where n is the number of arguments, f is the expression for the "if", and g, h are the commands for the "then" and "else" part, omitting the "inputs n".

# Compilation #

You will need the Haskell Platform. See Example.hs for how to compile a script.

	main = writeFile "sample_scripts/verify.txt" (compile $ verify' 999)

writes the script "verify' 999" (the public key hash here is 999) to the given file. To write it in hex format, write instead

	main = writeFile "sample_scripts/verify.txt" (compile $ toHex $ verify' 999)

To print to command line:

    main = putStrLn (compile (std_trans 999))

# Implementation and files #

Utilities.hs: Various utility functions.

Circuit.hs: Defines the datatypes involved. In particular, the commands are represented as "Circuit"s, which are lists of "Gate"s. A Gate is either an input (Arg _), a function of previous gates (Fun _ _ _), a constant (Const _), or a variable (Var _).

Compiler.hs: Takes a "Circuit2", represents it as a directed graph--which expressions depend on which other expressions?--and topologically sorts it, optimizing to apply functions to things on top of the stack first to avoid having to pick blocks to the top. This is the bulk of the program. Note that by rewriting "compile" this could be adapted to other (say) stack-based programming languages.

ScriptDefs.hs: Associating functions to opcodes.

Example.hs: Compiles example scripts.

Verify.hs: Basic verification script.

MultipartyLottery.hs: An example of a complicated script. This implements the "compute" step of the multiparty lottery in the paper https://eprint.iacr.org/2013/784.pdf

CircuitViz.hs: Draws the circuit. For testing. 

sample_scripts/: Compiled scripts.

# Todo #

* Create a pay-to-script-sig transaction from a regular script.
* Make OP_VERIFY's manual (using an operator like (?))

Description of Bitcoin script: http://chimera.labs.oreilly.com/books/1234000001802/ch05.html#tx_script
