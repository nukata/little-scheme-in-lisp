# A Little Scheme in Common/Emacs Lisp

This is a small interpreter of a subset of Scheme.
It runs on CLISP 2.49, SBCL 1.5.9, Emacs 22, Emacs 26 etc.

As a Scheme implementation, 
it optimizes _tail calls_ and handles _first-class continuations_ properly.

## How to run

```
$ emacs -batch -l scm.l
> (+ 5 6)
11
> (cons 'a (cons 'b 'c))
(a b . c)
> (list
1
2
3)
(1 2 3)
> 
```

It does not print `#t` nor `#f` properly yet.

```
> #f
:False
> 
```
