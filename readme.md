# CoinLang #

CoinLang gives a way to write Bitcoin scripts in a higher-level functional programming language rather than the stack-based language.

Example of use:

Define a standard transaction.

	std_trans pkh =
	  inputs 2 .>
      ver [arg 0, arg 1] .>
      eq [con pkh, hash [arg 1]]

OR

	std_trans pkh =
     script [
      inputs 2,
      ver [arg 0, arg 1],
      eq [con pkh, hash [arg 1]]]

Compile to bitcoin script and print out using the following. (Here 999 is the public key hash.)

    main = putStrLn (compile (std_trans 999))

See ScriptDefs.hs for how to include various script functions. Important: always remember to flag constants with "con", else you will get a compilation error.

Todo:

* Test compilation of if-then-else statements.
* Create a pay-to-script-sig transaction from a regular script.
* (Optional) Make OP_VERIFY's manual (using an operator like (?))

