# A Little Scheme in Common/Emacs Lisp

As a Scheme implementation, 
it optimizes _tail calls_ and handles _first-class continuations_ properly.

## How to run

```
$ emacs -batch -l scm.l
> (+ 5 6)
11
> (cons 'a (cons 'b 'c))
(a b . c)
> (list 1 2 3)
(1 2 3)
> 
```

It does not read `#t` nor `#f` yet.

```
> #f
Invalid read syntax: "#"
```
