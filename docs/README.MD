F-Lisp
======

A Lisp-ish language written in swi-Prolog and GNU Forth. Will eventually include a 
compiler targeting GNU Forth as an intermediate language. This is currently not at 
an advanced enough stage to be considered finished or even usable. So far I've 
written a fairly simple parser and it can manipulate syntax trees (function definitions and
conditional statements) from pseudo-lisp into FORTH postfix notation. Some of the syntax the 
parser can handle however is not even remotely close to proper LISP (comma separated lists, 
tuples, arrays, etc.), and I do not intend to keep too closely to tradition as far as tradional
LISP features are concerned.
