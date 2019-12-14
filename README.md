# mksense

It just makes sense!


## Excuse me?

mksense's purpose is to help evaluating [Propositional
calculus](https://en.wikipedia.org/wiki/Propositional_calculus) expression

## Functionalities

- Parse expressions -> see [Syntax](#syntax)
- form negation normal form
- form [Disjunctive Normal Form](https://en.wikipedia.org/wiki/Disjunctive_normal_form)
- form [Conjunctive Normal Form](https://en.wikipedia.org/wiki/Conjunctive_normal_form)
- form "orand" Form (only &and; and &or;)
- form Negation Normal Form
- form sets of clauses for DNF and CNF
- rich&trade; syntax highlighting

## Usage

```sh
mksense [--human | --brief] [[[dnf|cnf] clauses] nnf | orand | show] [expression]
```

- ```--human``` fancier output
- ```--brief``` brief output (default)
- ```dnf [clauses]``` form dnf or show dnf clauses
- ```cnf [clauses]``` form dnf or show cnf clauses
- ```nnf``` form nnf
- ```orand``` form orand
- ```show``` only parse and syntax highlight expression (default)

If no expression is given, start in interactive mode with specified settings.

## Syntax

``` ebnf
<literal> ::= "\"" <string> "\"" | <alphanum>
<unary> ::= "not" | "!" | "-" | ""
<operator> ::= "or" | "||" | "and" | "&&" | "&" | "=>" | "->" | "implies" | "impl" | "<=>" | "==" | "=" | "<->" | "equiv" | "eq" | "equivalent" | "nand" | "!&" | "xor"
<po> ::= "(" | "[" | "{" | "<"
<pc> ::= ")" | "]" | "}" | ">"
<atom> ::= <unary> <po> <expr> <pc> | <literal>
<expr> ::= <unary> <atom> <operator> <atom>

<syntax> := <expr>
```

**NOTE:** chaining "or" and "and" without parens is allowed too!

## Building

``` sh
stack build
cp $(stack path --dist-dir)/build/mksense/mksense . # copy executable to our workdirectory
```


## Examples

Just show parsed expression of ```( a ∨ b ) ⇔ ¬( a ∧ b )```
``` sh
mksense '(a or b) = not (a and b)'
```

Get NNF of ```¬( ¬[ a ∧ b ] ∨ ¬[ c ∨ ¬d ] )```

``` sh
mksense nnf '!( !(a and b) or !(c or !d))'
 ```
 
 Get DNF of ```( a ⇔ b ) ⇒ ¬( a ⊻ b )```
``` sh
mksense dnf '(a eq b) implies not (a xor b)'
 ```
 
... if you only want to see the set of clauses you can run
``` sh
mksense dnf clauses '(a eq b) implies not (a xor b)'
 ```

... a clearer output 
``` sh
mksense --human dnf clauses '(a eq b) implies not (a xor b)'
 ```
 
 
## An application Example

Take a look a those five statements. One of them is true, the rest is false.
Which statement is true?

- Statement 1: "Statement two is true."
- Statement 2: "Statement five is false."
- Statement 3: "All five statements are true."
- Statement 4: "All five statements are false."
- Statement 5: "Statement one is false."

First of all we will model each statement:

``` haskell
( 1 <-> 2 )
( 2 <-> !5 )
( 3 <-> [1 and 2 and 3 and 4 and 5] )
( 4 <-> not [1 or 2 or 3 or 4 or 5] )
( 5 <-> !1 )
```

So far, so good! But this by itself is not sufficient to model the whole
problem.

We need to model "One of them is true, the rest is false.":
``` haskell
( 1 -> not [2 or 3 or 4 or 5] )
( 2 -> not [1 or 3 or 4 or 5] )
( 3 -> not [2 or 1 or 4 or 5] )
( 4 -> not [2 or 3 or 1 or 5] )
( 5 -> not [2 or 3 or 4 or 1] )
(1 or 2 or 3 or 4 or 5)
```

Allright, to finalize our proposition we want every sub proposition to be true:

``` haskell
( 1 <-> 2 )                           and
( 2 <-> !5 )                          and
( 3 <-> [1 and 2 and 3 and 4 and 5] ) and 
( 4 <-> not [1 or 2 or 3 or 4 or 5] ) and
( 5 <-> !1 )                          and
( 1 -> not [2 or 3 or 4 or 5] )       and
( 2 -> not [1 or 3 or 4 or 5] )       and
( 3 -> not [2 or 1 or 4 or 5] )       and
( 4 -> not [2 or 3 or 1 or 5] )       and
( 5 -> not [2 or 3 or 4 or 1] )       and
(1 or 2 or 3 or 4 or 5)
```

Now we want to start mksense in interactive dnf clauses mode to enter this big
expression:

``` sh
mksense --human dnf clauses
```

Now paste the expression and hit enter twice.
You will get about this output:

``` text
Expression:
  <Totally readable expression>
Is satisfied when either:
 - Each of: [¬4,¬3,5,¬2,¬1] evaluates to True
```

Okay this output hints us that statement five is true.

Nice!
