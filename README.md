# Symbol nicknames: a broken toy
## Discussion
### Package systems and module systems
The [Common Lisp package system](http://www.lispworks.com/documentation/HyperSpec/Body/11_.htm "Packages") is famously deficient: it is not coincidental that it is in chapter 11 both in [*Common Lisp the Language*](https://en.wikipedia.org/wiki/Common_Lisp_the_Language "CLtL entry on Wikipedia") and the CL standard.  But it is not easy to see what a fix to the package system would be: many people, including me, have produced 'improvements', some of which are improvements.  One such 'improvement' -- package-local nicknames -- is effectively now standardised, but is only actually useful if people fill their code with package prefixes: a depressingly common practice which, in my opinion, hurts the readability of code significantly.

Arguably the underlying problem of the package system is that it works at the wrong place: it controls the mapping between strings of characters and symbols, rather than between symbols and meanings.  At least in some historical contexts, a system which does the latter would be called a module system, although this usage conflicts with both [CL's incomplete notion of modules](http://www.lispworks.com/documentation/HyperSpec/Body/24_.htm "System construction") and the use of the term in many more recent languages such as Python.

An example of what I mean is Racket's module system: here's an example.

```lisp
#lang racket

(module one racket
  (provide foo)
  (define (foo x y)
    (values (eq? x foo)
            (eq? y 'foo))))

(require (prefix-in one: 'one))

(define foo 3)
```

Now `(one:foo foo 'foo)` will return `#f` and  `#t`:  the value associated with the name `foo` is different, even though the symbols are the same.

Well, of course Racket's semantics is fairly different than CL's, and it's much harder to see how this would work in CL with its multiple namespaces associated with symbols: modules would need to be able to control at least `symbol-value`, `symbol-function`& `symbol-plist`, as well as any class or type associated with a symbol and things like any symbol macros associated with a symbol and so on.  And they'd almost certainly want to be able to be able to be defined so that only *some* of these associations were different in different modules.  I may be misunderstanding things but it's really not clear to me how a module system like Racket's would work for CL.

### Invented problems
But what's the real problem with the package system as it is?  Well, the first answer is 'much less than people often think': if you're willing to think about package design, and in particular to think about it as a critical part of *language* design and realise that all programming is language design, then you pretty seldom run into problems that can't be solved fairly easily.  As an example of this [conduit packages](https://tfeb.github.io/conduit-packages/ "Conduit packages") lets you do a lot of things that are possible but painful in CL, as well as now providing a variant of `defpackage` which is user-extensible.

Of course people like to use packages in an absolutely terrible way for reasons I can't fathom.  Generations of Lisp programmers have somehow been unable to understand that calling their package `XML`, `HTTP` or `JSON` is not a good idea.  More generally they still seem not to understand both that the package namespace is a scarce resource which needs to be structured so that clashes do not happen and that there is an absolutely obvious way of doing that: domain-structured names for packages.  Then, rather than using the package system to construct the language in which they want to write, they give up and use explicit package prefixes all over the place, resulting in code which is annoyingly hard to read.  Because, even by very low standards, writing code like that is not practical with long, unique package names, we then get the whole package-local-nicknames horror: a workaround to a problem which simply never needed to exist.

This is all just amazing.  The *whole point* of Lisp is that *it is a language in which to invent languages*: it's a language in which, to solve a problem you first design a programming language in which to solve that problem.  But somehow people have C++ envy and feel that they need to not only avoid doing the one thing Lisp is *for* but make their programs as unreadable as they can.  Oh well, whatever.

### Real problems
So, *are* there real problems?   If there are, what are they?  Are they to do with the package system at all?

There are real problems.  And they're two sides of the same coin.

The first problem is that **symbols have many meanings which can't easily be teased apart**.  It's fashionable to talk about 'Lisp-1s', which have a single namespace for variables and functions, and 'Lisp-2s' where functions and variables have distinct namespaces.  And then people will be clever and point out that CL is in fact a Lisp-$n$ for some value of $n$ significantly greater than 2 and perhaps equal to 8: symbols in CL name at least functions, blocks, variables, classes and types, slots, catch tags, property lists and restarts and probably other things I have forgotten.

Except that's wrong: *all Lisps are Lisp-$\infty$s*.  That's true because it is always possible to add a new namespace to any Lisp, if you have a Lisp-$n$ for any $n \in \mathbb{N}$, you can always write a program which turns your Lisp-$n$ into a Lisp-$n+1$.

And it's pretty much in the nature of symbols that, if a symbol $s$ has meaning $m_1$ and meaning $m_2$, then you can't split those two: you can't build some namespace where symbol $s_1$ has meaning $m_1$ and symbol $s_2$ has meaning $m_2$ and $s_1$ is not `eq` to $s_2$.

So there is really nothing you can do about this problem, certainly not within CL.

The second problem is the inverse of the first: **two distinct names can't be made to refer to the same symbol**.  Again this seems like it's not anything that can easily be fixed.  Except, *why* can't it be fixed?  For instance, it's perfectly possible to imagine a Lisp rather like CL where symbols have a canonical name and package, but also have a number of aliases, which might exist in different packages.  When the aliases are mentioned they would then refer to the canonical symbol.

In a system like that, this would be possible:

```lisp
;;; I want EQL to be also known as EQUIVALENTP
;;;
(setf (nickname-symbol "EQUIVALENTP") 'eql)

(defgeneric dispatch (on arg)
  (:method (on arg)
   (if (equivalentp on arg)
       on
     nil)))

(defmethod dispatch ((on (equivalentp 'foo)) arg)
  (format t "~%~S ~S~%" on arg)
  t)
```

And now

```lisp
> (eql 'eql 'equivalentp)
t

> (dispatch 'bar 1)
nil

> (dispatch 'bar 'bar)
bar

> (dispatch 'foo 3)

foo 3
t
```

There really is nothing stopping something like this working.

Are these problems to do with the package system?  Not really, no.

### Notes and queries
**Would something like this help?**  I don't know.  It would not help *me* very much, but clearly some people would like it.  It would certainly make things more complicated and probably allow even worse code to be written than people already do.

**Is something like this compatible with CL?**  Not entirely.  At the moment you can assume that `(find-symbol s)` will either return `nil`and `nil` or a symbol whose name `string=` to `s`and a second value.  That would no longer be the case if `s` was a nickname.

**Can this be implemented portably?** No: you need to be able to intervene in the process which turns strings into symbols which can't be done portably.

However it is possible, with a little work it is possible to implement something fairly like this.  Symbol nicknames is such a system.

## Symbol nicknames
Symbol nicknames provides a system where a given symbol may have a number of nicknames.  These nicknames are translated, at read time, into the original symbol.

This means that for a symbol $s$, with name $n(s)$ and package $p(s)$ there may be zero or more other pairs $(n_i(s), p_i(s))$ which also denote $s$.

Implementationally this is done by a cheap hack: the pairs $(n_i(s), p_i(s))$ in fact denote symbols which the system knows are nicknames for $s$, and the reader is hooked so that, if it reads some symbol $\sigma$, it looks it up in the table of nicknames and, if it is a nickname for a symbol $s$, returns $s$ instead.  This is not how a system like this *should* work -- there should be only one symbol, not two -- but this was the easy way to make it work as a proof of concept.

### The system
The system is `org.tfeb.toys.symbol-nicknames`.  It should be possible to load this in any conforming CL, but it only knows how to infect SBCL, CMUCL and LispWorks.  If you load it in another implementation you'll get a warning: all the functions below will work but the reader will not know how to do anything.

### Symbols and strings
In order to make the interface be more like it should be, most of the interface functions accept two arguments:

- a  *nickname designator* which is either a string or a symbol;
- an optional *package designator* which is a string, a package or `nil`.

The semantics of the case when the nickname designator is a string or a symbol are slightly different to make things sane.

- When the nickname designator is a  string, the package designator defaults to the value of `*package^`, so nicknames are created in the current package by default.  If given explicitly it should either designate a package in which the symbol will be looked up or created or be `nil`, which will create an uninterned nickname.
- When the nickname dessignator is a symbol the package designator defaults to the home package of the symbol.  If given explicitly then it should designate a package in which the symbol is present.  If given as `nil` the symbol should be uninterned.

In the symbol case where the designated package is not compatible with the symbol an error of type `package-error` is signalled.  In the case where a nickname is being created there will be a `continue` restart which will fix things up:

- if the symbol is not in the package it will be imported into it;
- if there is another symbol present in the package with the same name as the symbol it will be shadowed;
- if the symbol has a home package but the package has been given as `nil` it will be uninterned from its home package.

Interactively, the restart description describes what will be done.  You can invoke the restart with an optional argument which, if true, says 'I fixed it already, just blunder on without fixing anything': you are responsible for your actions in this case.

A lot of the above complexity is to handle nicknames which are uninterned symbols.  It's hard to think of a use for these which makes all that hair worth it.

The descriptions below use the terms 'nickname designator' and 'package designator' to mean what is described here.

### The interface
**`*use-symbol-nicknames*`** defines whether the reader will translate symbol nicknames.  If true, it will.  The initial value is `nil`.  You can bind this locally if you know what you are doing (for instance to run tests without risking making the system uninhabitable).

**`nickname-symbol`** looks up a nickname and returns its symbol, if any.  It takes a nickname designator and optionally a package designator as above, and returns two values:

- the symbol nicknamed or `nil`;
- whether there is a nickname.

So in the case where there is no nicknamed symbol the return values are `nil` and `nil`.

`nickname-symbol` does not modify the package state or create symbols, and there are no restarts available to fix things up.

**`(setf nickname-symbol)`** sets the symbol that a nickname refers to.  It has nickname designator and optional package designator arguments as well as the target symbol.  The target symbol cannot be a nickname: an error is signalled if it is.

Here is an example: make `nothing` be a nickname for `nil`:

```lisp
> (nickname-symbol "NOTHING")
nil
nil

> (nickname-symbol "NOTHING")
nil
nil

> (nickname-symbol 'nothing)
nil
nil

> (setf (nickname-symbol "NOTHING") 'nil)
nil

> (nickname-symbol 'nothing)
nil
t
```

Note that if the nickname is a string and the package is given as `nil`, this will create a nickname which is an uninterned symbol and that, obviously, multiple such nicknames can exist:

```lisp
> (setf (nickname-symbol "FOO" nil) 'car)
car

> (setf (nickname-symbol "FOO" nil) 'car)
car

> (map-symbol-nicknames (lambda (n s) (format t "~&~S -> ~S~%" n s)))
#:foo -> car
#:foo -> car
nil
```

A nickname cannot refer to a symbol which is itself a nickname: this avoids the possibility of circularity.

`(setf nickname-symbol)` can modify the package state and create symbols.  The restarts described above are offered in case of errors.

**`delete-symbol-nickname`** will remove a nickname.  It also takes a nickname designator and optional package designator arguments.  If symbol nicknames are enabled (`*use-symbol-nicknames*` is true) then it is almost always necessary to give it a nickname designator which is a string, as it's very hard to find the nickname symbol in this case (but see `map-symbol-nicknames`).

It returns true if there was a nickname, `nil` otherwise.

**`map-symbol-nicknames`** maps a function over nicknames and their targets.  It takes two arguments:

- a function of two arguments, the nickname symbol and its target;
- an optional package designator, or `nil` meaning uninterned nicknames.

If the package designator is not provided all nicknames are iterated over.  Note that the package designator refers to the package of the nicknames, not their targets.

`map-symbol-nicknames` is written in such a way that the function can freely add or remove nicknames.  Any nicknames the function adds will not be mapped over.

`map-symbol-nicknames` returns `nil`.

Symbol nicknames tries hard to avoid creating chains or cycles of nicknames: this should not be able to happen.  It does this in part by keeping a count of the nicknames a symbol has: if you try to make a symbol with a non-zero count into a nickname this fails.  However there is then the problem that a symbol which is a nickname for another symbol might get garbage-collected.`repair-symbol-nicknames` deals with this: it will fix up the reference counts to correspond to current reality.

**`repair-symbol-nicknames`** will repair symbol nicknames.  It first looks for serious problems which need user intervention, and then fixes up reference counts.  It takes one optional argument, `report`: if given this should be a suitable stream designator for `format` on which a report will be printed.  It returns two values: the number of repairs and the number of nasty problems it found.

The nasty problem it can find is when a nickname points at another nickname.  This should never be able to happen unless you manually manipulate the system, but it is checked anyway.  In this case `repair-symbol-nicknames` signals an error with two possible restarts:

- `remove-nickname-source` will stop the source being a nickname and then try again;
- `remove-nickname-target` will stop the target being a nickname and then try again.

In both cases there will then be reference counts which need to be fixed, but this will happen in the next phase.

The restart names are exported from the package, so you can programmatically handle these cases if you want to.

### Incompatibilities with Common Lisp: `find-symbol`
In CL you can assume that `(find-symbol x)` will return either `nil` as its second argument or a symbol whose name is the same as the string `x` as its first argument.  That is no longer true when symbol nicknames exist.  This is inherently incompatible with CL.  Rather than just blindly ignoring the problem, the system changes the behaviour of `find-symbol` so that when it is returning the target of a nickname it returns `:nickname` as its second value:

```lisp
> (setf *use-symbol-nicknames* nil)
nil

> (find-symbol "FOO")
foo
:internal

> (setf (nickname-symbol 'foo) 'bar)
bar

> (find-symbol "FOO")
foo
:internal

> (setf *use-symbol-nicknames* t)
t

> (find-symbol "FOO")
bar
:nickname
```

Note this only happens when the system is enabled: the bahaviour when it's disabled is standard.

### Package, module, feature, dependencies
Symbol nicknames lives in `org.tfeb.toys.symbol-nicknames` and provides `:org.tfeb.toys.symbol-nicknames`.  There is an ASDF system definition for both it and its various tests: `(asdf:test-system "org.tfeb.toys.symbol-nicknames")` should work.  The system itself has no dependencies, the test systems depend on [Parachute](https://shinmera.github.io/parachute/ "Parachute").

The core of the system should be portable CL.  It knows how to fully infect SBCL, CMUCL and LispWorks.

## Notes
The function `nickname-symbol` is called that because its argument is a nickname and it is returning a symbol.   `delete-symbol-nickname` is called that because it is deleting a nickname.  `map-symbol-nicknames` should probably be called `map-nickname-symbols` but is not.

Finally, note that symbol nicknames is a *toy*: it's a proof of concept, but not something being proposed as any kind of standard or substandard extension to CL.  Something a bit *like* it might be a possibly useful extension, but it needs frther thought.

---

## Appendices
### Adding a new meaning for symbols to Racket
The Racket code below defines things called 'frobs' which are just a way of associating values with symbols.  You can say `(set (frob 'a) 1)` to make a new frob or update an existing one, query the presence of a frob with `frob?`, retrieve its value with `(frob 'a)`, remove a frob with `remove-frob!`, dynamically bind frobs with `call/frobs` and finally clear all frobs with `clear-frobs!`.

```lisp
#lang racket

(require srfi/17)

(set! (setter hash-ref) hash-set!)

(define-values (frob frob? remove-frob! call/frobs clear-frobs!)
  (let ([frobs (make-parameter (list (make-hasheqv)))])
    (define (find-frob-hash name)
      (unless (symbol? name)
        (error 'frob "~S isn't a symbol" name))
      (let fftl ([ft (frobs)])
        (match ft
          ['()
           #f]
          [(cons table more)
           (if (hash-has-key? table name)
               table
               (fftl more))])))
    (values
     (getter-with-setter
      (λ (name (default (thunk
                         (raise
                          (make-exn:fail:contract
                           (format "~S is not a frob" name)
                           (current-continuation-marks))))))
        (let ([table (find-frob-hash name)])
          (cond
            [table (hash-ref table name)]
            [(procedure? default) (default)]
            [else default])))
     (λ (name value)
       (set! (hash-ref (or (find-frob-hash name) (car (frobs))) name) value)))
     (λ (name)
       (if (find-frob-hash name) #t #f))
     (λ (name)
       (let ([table (find-frob-hash name)])
         (if table
             (begin
               (hash-remove! table name)
               #t)
             #f)))
     (λ (thunk)
       (parameterize ([frobs (cons (make-hasheqv) (frobs))])
         (thunk)))
     (thunk
      (let cfl ([ft (frobs)])
        (match ft
          ['()
           #t]
          [(cons table more)
           (hash-clear! table)
           (cfl more)]))))))
```
