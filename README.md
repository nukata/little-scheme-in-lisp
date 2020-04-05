# A Little Scheme in Common Lisp

This is a small interpreter of a subset of Scheme.
It runs on CLISP 2.49, SBCL 2.0.3, ECL 16.1.3, GCL 2.6.12 and ABCL 1.6.0.
It implements almost the same language as

- [little-scheme-in-crystal](https://github.com/nukata/little-scheme-in-crystal)
- [little-scheme-in-cs](https://github.com/nukata/little-scheme-in-cs)
- [little-scheme-in-dart](https://github.com/nukata/little-scheme-in-dart)
- [little-scheme-in-go](https://github.com/nukata/little-scheme-in-go)
- [little-scheme-in-java](https://github.com/nukata/little-scheme-in-java)
- [little-scheme-in-php](https://github.com/nukata/little-scheme-in-php)
- [little-scheme-in-python](https://github.com/nukata/little-scheme-in-python)
- [little-scheme-in-ruby](https://github.com/nukata/little-scheme-in-ruby)
- [little-scheme-in-typescript](https://github.com/nukata/little-scheme-in-typescript)

and their meta-circular interpreter, 
[little-scheme](https://github.com/nukata/little-scheme).

As a Scheme implementation, 
it optimizes _tail calls_ and handles _first-class continuations_ properly.

## How to run

```
$ sbcl --script scm.l
> (+ 5 6)
11
> (cons 'a (cons 'b 'c))
(A B . C)
> (list
1
2
3)
(1 2 3)
> 
```

Press EOF (e.g. Control-D) to exit the session.

```
> Goodbye
$ 
```

You can run it with a Scheme script.
Examples are found in 
[little-scheme](https://github.com/nukata/little-scheme);
download it at `..` and you can try the following:

```
$ sbcl --script scm.l ../little-scheme/examples/yin-yang-puzzle.scm | head

*
**
***
****
*****
******
*******
********
*********
$ sbcl --script scm.l ../little-scheme/examples/amb.scm
((1 A) (1 B) (1 C) (2 A) (2 B) (2 C) (3 A) (3 B) (3 C))
$ sbcl --script scm.l ../little-scheme/examples/nqueens.scm
((5 3 1 6 4 2) (4 1 5 2 6 3) (3 6 2 5 1 4) (2 4 6 1 3 5))
```

Put a "`-`" after the script in the command line to begin a session 
after running the script.

```
$ sbcl --script scm.l ../little-scheme/examples/fib90.scm -
2880067194370816120
> (globals)
(APPLY CALL/CC GLOBALS ERROR = < * - + SYMBOL? EOF-OBJECT? READ NEWLINE
         DISPLAY LIST NOT NULL? PAIR? EQV? EQ? CONS CDR CAR FIBONACCI)
> (fibonacci 16)
987
> (fibonacci 1000)
43466557686937456435688527675040625802564660517371780402481729089536555417949051
89040387984007925516929592259308032263477520968962323987332247116164299644090653
3187938298969649928516003704476137795166849228875
> 
```

For expression types and built-in procedures of this Scheme, see
[little-scheme](https://github.com/nukata/little-scheme).


## Caveat

It does not print `#t` nor `#f` properly yet.

```
> #f
:FALSE
> 
```
