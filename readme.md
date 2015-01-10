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

See ScriptDefs.hs for how to include various script functions.

Todo:

* Compile if-then-else statements.
