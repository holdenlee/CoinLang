Example of use:

Define a standard transaction.

std_trans pkh =
  inputs 2 .>
  ver [arg 0, arg 1] .>
  eq [con pkh, hash [arg 1]]

Compile to bitcoin script and print out. (Here 999 is the public key hash.)

main = putStrLn (compile (std_trans 999))
  
Todo:

* Optimize compilation to use things on top of the stack first
* Allow composition of user-defined functions
* Allow user to store intermediate results in variables (i.e., name gates)
