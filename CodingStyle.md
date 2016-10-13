# su4sml coding style
We here document coding-style guidelines that should be adhered to.
Much of the existing code is not yet following these guidelines,
patches are welcome. This style-guide was inspired by the SML Style
Guide from
[Cornell university](http://www.cs.cornell.edu/Courses/cs312/2007fa/handouts/style.htm).

    	   
##  Chapter 1: Indentation and breaking long lines
The limit on the length of lines is 80 columns and this is a hard
limit. Using more than 80 columns causes your code to wrap around to
the next line, which is devastating to readability.

No Tab Characters. Do not use the tab character (0x09).  Instead, use
spaces to control indenting. Indent by two spaces.

Long expressions can be broken up and the parts aligned, as in the second
example.  Either is acceptable.
```sml
val x = "Long line..."^
  "Another long line."

val x = "Long line..."^
        "Another long line."
```
Case expressions should be indented as follows:
```sml
case expr of
  pat1 => ...
| pat2 => ...
```
If expressions should be indented according to one of the following schemes:
```sml
if exp1 then exp2              if exp1 then
else if exp3 then exp4           exp2
else if exp5 then exp6         else exp3
     else exp8

if exp1 then exp2 else exp3    if exp1 then exp2
                               else exp3
```
Comments should be indented to the level of the line of code that follows the
comment.


## Chapter 2: Factoring

Avoid breaking expressions over multiple lines.  If a tuple consists of more
than two or three elements, you should consider using a record instead of a
tuple.  Records have the advantage of placing each name on a separate line and
still looking good.  Constructing a tuple over multiple lines makes for ugly
code.  Other expressions that take up multiple lines should be done with a lot
of thought.  The best way to transform code that constructs expressions over
multiple lines to something that has good style is to factor the code using a
let expression.  Consider the following:

* Bad
  ```sml
     fun euclid (m:int,n:int) : (int * int * int) =
       if n=0
         then (b 1, b 0, m)
       else (#2 (euclid (n, m mod n)), u - (m div n) *

               (euclid (n, m mod n)), #3 (euclid (n, m mod n)))
  ```

* Better
  ```sml
     fun euclid (m:int,n:int) : (int * int * int) =
       if n=0
         then (b 1, b 0, m)
       else (#2 (euclid (n, m mod n)),

             u - (m div n) * (euclid (n, m mod n)),

             #3 (euclid (n, m mod n)))
  ```
* Best
  ```
     fun euclid (m:int,n:int) : (int * int * int) =
       if n=0
         then (b 1, b 0, m)
       else let
         val q = m div n
         val r = n mod n
         val (u,v,g) = euclid (n,r)
       in
         (v, u - q*v, g)
       end
  ```

Do not factor unnecessarily.
* Bad
  ```sml
     let
       val x = TextIO.inputLine TextIO.stdIn
     in
       case x of
         ...
     end
  ```

* Good
  ```sml
     case TextIO.inputLine TextIO.stdIn of
       ...
  ```
* Bad (provided y is not a large expression):
  ```sml
    let val x = y*y in x+z end
  ```

* Good
  ```sml
    y*y + z
  ```

## Chapter 3: Comments
Comments should be written with
[SMLDoc](http://www.pllab.riec.tohoku.ac.jp/smlsharp/?SMLDoc) in mind;
a, nightly updated, API documentation is
[available](http://projects.brucker.ch/su4sml/smldoc/). For example:
```sml
(**
 * opens a file.
 * @params {fileName, mode}
 * @param fileName file name
 * @param mode mode flag
 * @return file stream *)
val openFile : {fileName : string, mode : openMode} -> stream
```
We mainly restrict ourselves to the following SMLDoc tags:
```
  @params  gives names to formal parameters of functions and value constructors
  @param   a description of a formal parameter
  @return  a description about return value of the function
  @see     related items (specified text is not analyzed in the current version)
  @throws  same as the @exception tag
```
Signatures must be commented using SMLDoc. 

Comments go above the code they reference, as in the following example:
```sml
(** Sums a list of integers. *)
val sum = foldl (op +) 0
```
Avoid Useless Comments. Avoid comments that merely repeat the code they
reference or state the obvious.  Comments should state the invariants, the
non-obvious, or any references that have more information about the code.

Avoid Over-commenting. Very many or very long comments in the code body are
more distracting than helpful.  Long comments may appear at the top of a file
if you wish to explain the overall design of the code or refer to any sources
that have more information about the algorithms or data structures.  All other
comments in the file should be as short as possible.  A good place for a
comment is just before a function declaration.  Judicious choice of variable
names can help minimize the need for comments.

Line Breaks. Empty lines should only be included between value declarations
within a struct block, especially between function declarations.  It is not
necessary to put empty lines between other declarations unless you are
separating the different types of declarations (such as structures, types,
exceptions and values).  Unless function declarations within a let block are
long, there should be no empty lines within a let block. There should never be
an empty line within an expression.

Multi-line Commenting. When comments are printed on paper, the reader lacks the
advantage of color highlighting performed by an editor such as Emacs. 
Multiline comments can be distinguished from code by preceding each line of the
comment with a * similar to the following:
```sml
(**
 * This is one of those rare but long comments
 * that need to span multiple lines because
 * the code is unusually complex and requires
 * extra explanation. *)
fun complicatedFunction () = ...
```

## Chapter 4: Parentheses

Over Parenthesizing.  Parentheses have many semantic purposes in ML, including
constructing tuples, grouping sequences of side-effect expressions, forcing a
non-default parse of an expression, and grouping structures for functor
arguments.  Their usage is very different from C or Java.  Avoid using
unnecessary parantheses when their presence makes your code harder to
understand.

Case expressions.  Wrap case expressions with parentheses.  This avoids a
common error involving nested case expressions. If the case expression is
already wrapped by a let...in...end block, you can drop the parentheses.

Alternative Block Styles. Blocks of code such as let...in...end, struct...end,
and sig...end should be indented as follows.  There are several alternative
styles to choose from.
```sml
fun foo bar =        fun foo bar =       fun foo bar = let
  let                  let val p = 4       val p = 4
    val p = 4              val q = 38      val q = 38
    val q = 38       in                  in
  in                   bar * (p + q)       bar * (p + q)
    bar * (p + q)    end                 end
  end
```


## Chapter 5: Pattern matching

No Incomplete Pattern Matches.  Incomplete pattern matches are flagged with
compiler warnings, which should be treated as errors. 

Pattern Match in the Function Arguments When Possible.  Tuples, records and
datatypes can be deconstructed using pattern matching.  If you simply
deconstruct the function argument before you do anything useful, it is better
to pattern match in the function argument. Consider these examples:
```sml
Bad                              Good
fun f arg1 arg2 = let            fun f (x,y) (z,_) = ...
  val x = #1 arg1
  val y = #2 arg1
  val z = #1 arg2
in                     
  ...
end


fun f arg1 = let                 fun f {foo=x, bar=y, baz} = ...
  val x = #foo arg1
  val y = #bar arg1
  val baz = #baz arg1  
in
  ...
end
```


Avoid Unnecessary Projections.  Prefer pattern matching to projections with
function arguments or a value declarations.  Using projections is okay as long
as it is infrequent and the meaning is clearly understood from the context. 
The above rule shows how to pattern-match in the function arguments.  Here is
an example for pattern matching with value declarations.

```sml
Bad                                 Good
let                                 let
  val v = someFunction()              val (x,y) = someFunction()
  val x = #1 v                      in
  val y = #2 v                        x+y
in                                  end
  x+y
end
```


Combine nested case Expressions.  Rather than nest case expressions, you can
combine them by pattern matching against a tuple, provided the tests in the
case expressions are independent.  Here is an example:

* Bad
  ```sml
     let
       val d = Date.fromTimeLocal(Time.now())
     in
       case Date.month d of
         Date.Jan => (case Date.day d of
                        1 => print "Happy New Year"
                      | _ => ())
       | Date.Jul => (case Date.day d of
                        4 => print "Happy Independence Day"
                      | _ => ())
       | Date.Oct => (case Date.day d of
                        10 => print "Happy Metric Day"
                      | _ => ())
     end
     ```
     
* Good
  ```sml
     let
       val d = Date.fromTimeLocal(Time.now())
     in
       case (Date.month d, Date.day d) of
         (Date.Jan, 1) => print "Happy New Year"
       | (Date.Jul, 4) => print "Happy Independence Day"
       | (Date.Oct, 10) => print "Happy Metric Day"
       | _ => ()
     end
  ```
     
Avoid the use `valOf`, `hd`, or `tl`.  The functions `valOf`, `hd`,
and `tl` are used to deconstruct option types and list types.
However, they raise exceptions on certain inputs.  You should avoid
these functions altogether.  It is usually easy to achieve the same
effect with pattern matching.  If you cannot manage to avoid them, you
should handle any exceptions that they might raise.


## Chapter 6: Naming and declarations
Naming Conventions. The best way to tell at a glance something about the type
of a variable is to use the standard SML naming conventions.  The following are
the preferred rules that are (more or less) followed by the SML basis and 
SML/NJ libraries:

Token          | SML Naming Convention
 ------------- |:-------------------------------------------------------------------------
Variables      | Symbolic or initial lower case. Use embedded caps for multiword names.         
               | *Example:* `getItem`
Functions      | Initial lower case.  Use embedded caps for multiword names.
               | *Example:* `nameOf`
Constructors   | Initial upper case.  Use embedded caps for multiword names.  Historic exceptions are `nil`, `true`, and `false`.  Rarely are symbolic names like `::` used.   
               | *Example:* `Node`, `EmptyQueue`
Types          | All lower case.  Use underscores for multiword names.           
               | *Example:* `priority_queue`
Signatures     | All upper case.  Use underscores for multiword names.                           
               | *Example:* `PRIORITY_QUEUE`
Structures     | Initial upper case.  Use embedded caps for multiword names.                     
               | *Example:* `PriorityQueue`
Functors       | Same as structure convention, except Fn completes the name.                  
               | *Example:* `PriorityQueueFn`

These conventions are not enforced by the compiler, though violations of the
variable/constructor conventions ought to cause warning messages because of the
danger of a constructor turning into a variable when it is misspelled.

Use Meaningful Names. Another way of conveying information is to use meaningful
variable names that reflect their intended use.  Choose words or combinations
of words describing the value.  Variable names may be one letter in short let
blocks.  Functions used in a fold, filter, or map are often bound to the name
f.  Here is an example for short variable names:
```sml
let
  val d = Date.fromTimeLocal(Time.now())
  val m = Date.minute d
  val s = Date.second d
  fun f n = (n mod 3) = 0
in
  List.filter f [m,s]
end
```

Avoid Global Mutable Variables. Mutable values should be local to closures and
almost never declared as a structure's value.  Global mutable values cause many
problems.  First, it is difficult to ensure that the mutable value is in the
proper state, since it might have been modified outside the function or by a
previous execution of the algorithm.  This is especially problematic with
concurrent threads.  Second, and more importantly, having global mutable values
makes it more likely that your code is nonreentrant.  Without proper knowledge
of the ramifications, declaring global mutable values can extend beyond bad
style to incorrect code.

When to Rename Variables. You should rarely need to rename values, in fact this
is a sure way to obfuscate code.  Renaming a value should be backed up with a
very good reason. One instance where renaming a variable is common and
encouraged is when aliasing structures. In these cases, other structures used
by functions within the current structure are aliased to one or two letter
variables at the top of the struct block. This serves two purposes: it shortens
the name of the structure and it documents the structures you use. Here is an
example:
```sml
struct
  structure H = HashTable
  structure T = TextIO
  structure A = Array
  ...
end
```
Order of Declarations in a Structure. When declaring elements in a structure,
you should first alias the structures you intend to use, followed by the types,
followed by exceptions, and lastly list all the value declarations for the
structure. Here is an example:
```sml
struct
  structure L = List
  type foo = unit
  exception InternalError
  fun first list = L.nth(list,0)
end
```

Every declaration within the structure should be indented the same amount.

Moreover, every top-level structure should be restricted by a (documented) 
signature. 

Functions should declared in their their curried form, e.g.,  
```sml
fun f x y = ... instead of fun f(x,y) = ...
```
Datatypes should be preferred to type synonyms in particular for
record types


## Chapter 7: Verbosity

Don't Rewrite Library Functions. The basis library and the SML/NJ library have
a great number of functions and data structures -- use them!  Often students
will recode List.filter, List.map, and similar functions.  A more subtle
situation for recoding is all the fold functions.  Writing a function that
recursively walks down the list should make vigorous use of List.foldl or
List.foldr.  Other data structures often have a folding function; use them
whenever they are available.

Misusing if Expressions.  Remember that the type of the condition in an if
expression is bool. In general, the type of an if expression is 'a, but in the
case that the type is bool, you should not be using if at all. Consider the
following:


```sml
Bad                                     Good
if e then true else false               e
if e then false else true               not e
if beta then beta else false            beta
if not e then x else y                  if e then y else x
if x then true else y                   x orelse y
if x then y else false                  x andalso y
if x then false else y                  not x andalso y
if x then y else true                   not x orelse y
```

Misusing case Expressions.  The case expression is misused in two common
situations.  First, case should never be used in place of an if expression
(that's why if exists).  Note the following:
```sml
case e of
  true => x
| false => y

if e then x else y
```
The latter is much better.  Another situation where if expressions are
preferred over case expressions is as follows:
```sml
case e of
  c => x   (* c is a constant value *)
| _ => y

if e=c then x else y
```
The latter is definitely better.  The other misuse is using case when pattern
matching with a val declaration is enough. Consider the following:
```sml
val x = case expr of (y,z) => y

val (x,_) = expr
```
The latter is better.

Other Common Misuses.  Here are some other common mistakes to watch out for:

```sml                              
Bad                                 Good
l::nil                              [l]
l::[]                               [l]
length + 0                          length
length * 1                          length
big exp * same big exp              let val x = big exp in x*x end
if x then f a b c1                  f a b if x then c1 else c2
else f a b c2
String.compare(x,y)=EQUAL           x=y
String.compare(x,y)=LESS            x<y
String.compare(x,y)=GREATER         x>y
Int.compare(x,y)=EQUAL              x=y
Int.compare(x,y)=LESS               x<y
Int.compare(x,y)=GREATER            x>y
Int.sign(x)=~1                      x<0
Int.sign(x)=0                       x=0
Int.sign(x)=1                       x>0
```

Do not re-wrap Functions.  When passing a function as an argument to another
function, don't re-wrap the function unnecessarily.  Here's an example:
```sml
List.map (fn x => Math.sqrt x) [1.0, 4.0, 9.0, 16.0]

List.map Math.sqrt [1.0, 4.0, 9.0, 16.0]
```
The latter is better. Another case for rewrapping a function is often
associated with infix binary operators. To prevent rewrapping the binary
operator, use the op keyword as in the following example:
```sml
foldl (fn (x,y) => x + y) 0

foldl (op +) 0
```
The latter is better.

Don't Needlessly Nest let Expressions. Multiple declarations may occur in the
first block of a let...in...end expression.  The bindings are performed
sequentially, so you may use a name bound earlier in the same block.  Consider
the following:
```sml
let
  val x = 42
in
  let
    val y = x + 101
  in
    x + y
  end
end

let
  val x = 42
  val y = x + 101
in
  x + y
end
```

The latter is better.

Avoid Computing Values Twice.  If you compute a value twice, you're wasting CPU
time and making your program ugly. The best way to avoid computing values twice
is to create a let expression and bind the computed value to a variable name.
This has the added benefit of letting you document the purpose of the value
with a name.

           
## Chapter 8: File names and encoding

In general, a source file should only contain one signature or structure. In more 
detail:
- if a signature is only implemented by one structure, the signature and 
  structure can be placed in one file,
- if a signature is implemented by several structures, the signature should 
  be placed into a separate file. 
The file name should reflect the signature name and it should be 
and separated by underscore, e.g., ocl_term.sig 

Source files should use the Unix line ending convention and be either encoding 
using ASCII (preferred) or UTF-8.

## Chapter 9: Compatibility

Any code developed must be portable among the supported SML systems
(currently: sml/NJ, mlton, polyml 5.x). Moreover, the code should
without errors or warnings.  In general, you should treat compiler
warnings as errors. Keep in mind that polyml does only provide a subset
of the SML standard library. 


## Appendix
###   Appendix I: Machine-support
The following elisp-snippet provides marginal support for this coding-style for 
the [sml-mode](http://www.smlnj.org/doc/Emacs/sml-mode.html) of
[Emacs](http://www.gnu.org/software/emacs/).

```emacs
  (setq sml-indent-level       2)
  (setq sml-pipe-indent        -2)
  (setq sml-case-indent        t)
  (setq sml-nested-if-indent   t)
  (setq sml-type-of-indent     nil)     
  (setq sml-electric-semi-mode nil)
```

