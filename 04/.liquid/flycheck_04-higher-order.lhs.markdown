<!--
{-# OPTIONS_GHC -Wall #-}
-->

Higher-order programming and type inference
===========================================

CIS 194 Week 4  
4 February 2013

Suggested reading:

  * *Learn You a Haskell for Great Good* chapter "Higher-Order Functions" (Chapter 5 in the printed book; [Chapter 6 online](http://learnyouahaskell.com/higher-order-functions))

Anonymous functions
-------------------

Suppose we want to write a function

~~~~ {.haskell}
greaterThan100 :: [Integer] -> [Integer]
~~~~

which keeps only those `Integers` from the input list which are
greater than 100.  For example, 

~~~~ {.haskell}
greaterThan100 [1,9,349,6,907,98,105] = [349,907,105].
~~~~

By now, we know a nice way to do this:

<pre><span class=hs-linenum>33: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>gt100</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Integer</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span>
<span class=hs-linenum>34: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer -&gt; GHC.Types.Bool</span><span class='hs-definition'>gt100</span></a> <a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer</span><span class='hs-varid'>x</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Integer.Type.Integer | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &gt; v}</span><span class='hs-varop'>&gt;</span></a> <span class='hs-num'>100</span>
<span class=hs-linenum>35: </span><span class='hs-varop'>&gt;</span>
<span class=hs-linenum>36: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>greaterThan100</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>37: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; [GHC.Integer.Type.Integer]</span><span class='hs-definition'>greaterThan100</span></a> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer]</span><span class='hs-varid'>xs</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(GHC.Integer.Type.Integer -&gt; GHC.Types.Bool)
-&gt; x3:[GHC.Integer.Type.Integer]
-&gt; {v : [GHC.Integer.Type.Integer] | len v &lt;= len x3}</span><span class='hs-varid'>filter</span></a> <a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer -&gt; GHC.Types.Bool</span><span class='hs-varid'>gt100</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Integer.Type.Integer] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>
But it's annoying to give `gt100` a name, since we are probably never
going to use it again.  Instead, we can use an *anonymous function*,
also known as a *lambda abstraction*:

<pre><span class=hs-linenum>43: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>greaterThan100_2</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>44: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; [GHC.Integer.Type.Integer]</span><span class='hs-definition'>greaterThan100_2</span></a> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer]</span><span class='hs-varid'>xs</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(GHC.Integer.Type.Integer -&gt; GHC.Types.Bool)
-&gt; x3:[GHC.Integer.Type.Integer]
-&gt; {v : [GHC.Integer.Type.Integer] | len v &lt;= len x3}</span><span class='hs-varid'>filter</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer -&gt; GHC.Types.Bool</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer</span><span class='hs-varid'>x</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>{v : GHC.Integer.Type.Integer | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &gt; v}</span><span class='hs-varop'>&gt;</span></a> <span class='hs-num'>100</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [GHC.Integer.Type.Integer] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>
`\x -> x > 100` (the backslash is supposed to look kind of like a
lambda with the short leg missing) is the function which takes a
single argument `x` and outputs whether `x` is greater than 100.

Lambda abstractions can also have multiple arguments. For example:

    Prelude> (\x y z -> [x,2*y,3*z]) 5 6 3
    [5,12,9]

However, in the particular case of `greaterThan100`, there's an even
better way to write it, without a lambda abstraction:

<pre><span class=hs-linenum>58: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>greaterThan100_3</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>59: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; [GHC.Integer.Type.Integer]</span><span class='hs-definition'>greaterThan100_3</span></a> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer]</span><span class='hs-varid'>xs</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(GHC.Integer.Type.Integer -&gt; GHC.Types.Bool)
-&gt; x3:[GHC.Integer.Type.Integer]
-&gt; {v : [GHC.Integer.Type.Integer] | len v &lt;= len x3}</span><span class='hs-varid'>filter</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &gt; v}</span><span class='hs-varop'>&gt;</span></a><span class='hs-num'>100</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>{v : [GHC.Integer.Type.Integer] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>
`(>100)` is an *operator section*: if `?` is an operator, then `(?y)`
is equivalent to the function `\x -> x ? y`, and `(y?)` is equivalent
to `\x -> y ? x`.  In other words, using an operator section allows us
to *partially apply* an operator to one of its two arguments.  What we
get is a function of a single argument.  Here are some examples:

    Prelude> (>100) 102
    True
    Prelude> (100>) 102
    False
    Prelude> map (*6) [1..5]
    [6,12,18,24,30]

Function composition
--------------------

Before reading on, can you write down a function whose type is

~~~~ {.haskell}
(b -> c) -> (a -> b) -> (a -> c)
~~~~

?

Let's try.  It has to take two arguments, both of which are functions,
and output a function.

~~~~ {.haskell}
foo f g = ...
~~~~

In the place of the `...` we need to write a function of type `a ->
c`.  Well, we can create a function using a lambda abstraction:

~~~~ {.haskell}
foo f g = \x -> ...
~~~~

`x` will have type `a`, and now in the `...` we need to write an
expression of type `c`.  Well, we have a function `g` which can turn
an `a` into a `b`, and a function `f` which can turn a `b` into a `c`,
so this ought to work:

<pre><span class=hs-linenum>104: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>foo</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span>
<span class=hs-linenum>105: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>forall a b c. (a -&gt; b) -&gt; (c -&gt; a) -&gt; c -&gt; b</span><span class='hs-definition'>foo</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>g</span></a> <span class='hs-keyglyph'>=</span> <span class='hs-keyglyph'>\</span><a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>f</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>g</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-layout'>)</span>
</pre>
(Quick quiz: why do we need the parentheses around `g x`?)

OK, so what was the point of that?  Does `foo` actually do anything
useful or was that just a silly exercise in working with types?

As it turns out, `foo` is really called `(.)`, and represents
*function composition*.  That is, if `f` and `g` are functions, then
`f . g` is the function which does first `g` and then `f`.

Function composition can be quite useful in writing concise, elegant
code.  It fits well in a "wholemeal" style where we think about
composing together successive high-level transformations of a data
structure.

As an example, consider the following function:

<pre><span class=hs-linenum>123: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myTest</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span>
<span class=hs-linenum>124: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Types.Bool</span><span class='hs-definition'>myTest</span></a> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer]</span><span class='hs-varid'>xs</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>GHC.Types.Int -&gt; GHC.Types.Bool</span><span class='hs-varid'>even</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:[GHC.Integer.Type.Integer] -&gt; {v : GHC.Types.Int | v == len x1}</span><span class='hs-varid'>length</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; [GHC.Integer.Type.Integer]</span><span class='hs-varid'>greaterThan100</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Integer.Type.Integer] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
</pre>
We can rewrite this as:

<pre><span class=hs-linenum>128: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myTest'</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span>
<span class=hs-linenum>129: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Types.Bool</span><span class='hs-definition'>myTest'</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>GHC.Types.Int -&gt; GHC.Types.Bool</span><span class='hs-varid'>even</span></a> <a class=annot href="#"><span class=annottext>(GHC.Types.Int -&gt; GHC.Types.Bool)
-&gt; ([GHC.Integer.Type.Integer] -&gt; GHC.Types.Int)
-&gt; [GHC.Integer.Type.Integer]
-&gt; exists [GHC.Types.Int].GHC.Types.Bool</span><span class='hs-varop'>.</span></a> <a class=annot href="#"><span class=annottext>x1:[GHC.Integer.Type.Integer] -&gt; {v : GHC.Types.Int | v == len x1}</span><span class='hs-varid'>length</span></a> <a class=annot href="#"><span class=annottext>([GHC.Integer.Type.Integer] -&gt; GHC.Types.Int)
-&gt; ([GHC.Integer.Type.Integer] -&gt; [GHC.Integer.Type.Integer])
-&gt; [GHC.Integer.Type.Integer]
-&gt; exists [[GHC.Integer.Type.Integer]].GHC.Types.Int</span><span class='hs-varop'>.</span></a> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; [GHC.Integer.Type.Integer]</span><span class='hs-varid'>greaterThan100</span></a>
</pre>
This version makes much clearer what is really going on: `myTest'` is
just a "pipeline" composed of three smaller functions.  This example
also demonstrates why function composition seems "backwards": it's
because function application is backwards!  Since we read from left to
right, it would make sense to think of values as also flowing from
left to right.  But in that case we should write \\( (x)f \\) to
denote giving the value \\(x\\) as an input to the function \\(f\\).
But no thanks to Alexis Claude Clairaut and Euler, we have been stuck
with the backwards notation since 1734.

Let's take a closer look at the type of `(.)`.  If we ask `ghci` for
its type, we get

    Prelude> :t (.)
    (.) :: (b -> c) -> (a -> b) -> a -> c

Wait a minute.  What's going on here?  What happened to the
parentheses around `(a -> c)`?

Currying and partial application
--------------------------------

Remember how the types of multi-argument functions look weird, like
they have "extra" arrows in them?  For example, consider the function

<pre><span class=hs-linenum>156: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>f</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>157: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>GHC.Types.Int -&gt; GHC.Types.Int -&gt; GHC.Types.Int</span><span class='hs-definition'>f</span></a> <a class=annot href="#"><span class=annottext>GHC.Types.Int</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>GHC.Types.Int</span><span class='hs-varid'>y</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int
-&gt; {v : GHC.Types.Int | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-varop'>*</span></a><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int -&gt; {v : GHC.Types.Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == y}</span><span class='hs-varid'>y</span></a>
</pre>
I promised before that there is a beautiful, deep reason for this, and
now it's finally time to reveal it: *all functions in Haskell take
only one argument*.  Say what?! But doesn't the function `f` shown
above take two arguments?  No, actually, it doesn't: it takes one
argument (an `Int`) and *outputs a function* (of type `Int -> Int`);
that function takes one argument and returns the final answer.  In
fact, we can equivalently write `f`'s type like this:

<pre><span class=hs-linenum>167: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>f'</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span><span class='hs-layout'>)</span>
<span class=hs-linenum>168: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>GHC.Types.Int -&gt; GHC.Types.Int -&gt; GHC.Types.Int</span><span class='hs-definition'>f'</span></a> <a class=annot href="#"><span class=annottext>GHC.Types.Int</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>GHC.Types.Int</span><span class='hs-varid'>y</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int
-&gt; {v : GHC.Types.Int | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-varop'>*</span></a><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int -&gt; {v : GHC.Types.Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == y}</span><span class='hs-varid'>y</span></a>
</pre>
In particular, note that function arrows *associate to the right*,
that is, `W -> X -> Y -> Z` is equivalent to `W -> (X -> (Y -> Z))`.
We can always add or remove parentheses around the rightmost top-level
arrow in a type.

Function application, in turn, is *left*-associative. That is, `f 3 2`
is really shorthand for `(f 3) 2`.  This makes sense given what we
said previously about `f` actually taking one argument and returning a
function: we apply `f` to an argument `3`, which returns a function of
type `Int -> Int`, namely, a function which takes an `Int` and adds 6
to it. We then apply that function to the argument `2` by writing `(f
3) 2`, which gives us an `Int`.  Since function application associates
to the left, however, we can abbreviate `(f 3) 2` as `f 3 2`, giving
us a nice notation for `f` as a "multi-argument" function.

The "multi-argument" lambda abstraction

~~~~ {.haskell}
\x y z -> ... 
~~~~

is really just syntax sugar for

~~~~ {.haskell}
\x -> (\y -> (\z -> ...)).  
~~~~

Likewise, the function definition

~~~~ {.haskell}
f x y z = ... 
~~~~

is syntax sugar for 

~~~~ {.haskell}
f = \x -> (\y -> (\z -> ...)).
~~~~

Note, for example, that we can rewrite our composition function from
above by moving the `\x -> ...` from the right-hand side of the `=` to
the left-hand side:

<pre><span class=hs-linenum>213: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>comp</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span>
<span class=hs-linenum>214: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>forall a b c. (a -&gt; b) -&gt; (c -&gt; a) -&gt; c -&gt; b</span><span class='hs-definition'>comp</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>g</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>f</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>a -&gt; b</span><span class='hs-varid'>g</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-layout'>)</span>
</pre>
This idea of representing multi-argument functions as one-argument
functions returning functions is known as *currying*, named for the
British mathematician and logician Haskell Curry.  (His first name
might sound familiar; yes, it's the same guy.) Curry lived from
1900-1982 and spent much of his life at Penn State---but he also
helped work on ENIAC at UPenn.  The idea of representing
multi-argument functions as one-argument functions returning functions
was actually first discovered by Moses Schönfinkel, so we probably
ought to call it *schönfinkeling*.  Curry himself attributed the idea
to Schönfinkel, but others had already started calling it "currying"
and it was too late.

If we want to actually represent a function of two arguments we
can use a single argument which is a tuple.  That is, the function

<pre><span class=hs-linenum>231: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>f''</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Int</span><span class='hs-layout'>,</span><span class='hs-conid'>Int</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>232: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>(GHC.Types.Int, GHC.Types.Int) -&gt; GHC.Types.Int</span><span class='hs-definition'>f''</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-layout'>,</span><span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (2  :  int)}</span><span class='hs-num'>2</span></a><a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int
-&gt; {v : GHC.Types.Int | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-varop'>*</span></a><a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int -&gt; {v : GHC.Types.Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == y}</span><span class='hs-varid'>y</span></a>
</pre>
can also be thought of as taking "two arguments", although in another
sense it really only takes one argument which happens to be a pair.
In order to convert between the two representations of a two-argument
function, the standard library defines functions called `curry` and
`uncurry`, defined like this (except with different names):

<pre><span class=hs-linenum>240: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>schönfinkel</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span><span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span>
<span class=hs-linenum>241: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>forall a b c. ((a, b) -&gt; c) -&gt; a -&gt; b -&gt; c</span><span class='hs-definition'>schönfinkel</span></a> <a class=annot href="#"><span class=annottext>(a, b) -&gt; c</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>y</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>(a, b) -&gt; c</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{v : (a, b) | fst v == x &amp;&amp; x_Tuple22 v == y &amp;&amp; snd v == y &amp;&amp; x_Tuple21 v == x}</span><span class='hs-layout'>(</span></a><a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a><span class='hs-layout'>,</span><a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a><span class='hs-layout'>)</span>
<span class=hs-linenum>242: </span><span class='hs-varop'>&gt;</span>
<span class=hs-linenum>243: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>unschönfinkel</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span><span class='hs-layout'>,</span><span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>c</span>
<span class=hs-linenum>244: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>forall a b c. (a -&gt; b -&gt; c) -&gt; (a, b) -&gt; c</span><span class='hs-definition'>unschönfinkel</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; c</span><span class='hs-varid'>f</span></a> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-layout'>,</span><span class='hs-varid'>y</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; c</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == y}</span><span class='hs-varid'>y</span></a>
</pre>
`uncurry` in particular can be useful when you have a pair and want to
apply a function to it.  For example:

    Prelude> uncurry (+) (2,3)
    5

**Partial application**

The fact that functions in Haskell are curried makes *partial
application* particularly easy.  The idea of partial application is
that we can take a function of multiple arguments and apply it to just
*some* of its arguments, and get out a function of the remaining
arguments.  But as we've just seen, in Haskell there *are no*
functions of multiple arguments!  Every function can be "partially
applied" to its first (and only) argument, resulting in a function of
the remaining arguments.

Note that Haskell doesn't make it easy to partially apply to an
argument other than the first. The one exception is infix operators,
which as we've seen, can be partially applied to either of their two
arguments using an operator section.  In practice this is not that big
of a restriction.  There is an art to deciding the order of arguments
to a function to make partial applications of it as useful as
possible: the arguments should be ordered from from "least to greatest
variation", that is, arguments which will often be the same should be
listed first, and arguments which will often be different should come
last.

**Wholemeal programming**

Let's put some of the things we've just learned together in an example
that also shows the power of a "wholemeal" style of programming.
Consider the function `foobar`, defined as follows:

<pre><span class=hs-linenum>280: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>foobar</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Integer</span>
<span class=hs-linenum>281: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-definition'>foobar</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <span class='hs-num'>0</span>
<span class=hs-linenum>282: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>foobar</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span>
<span class=hs-linenum>283: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <a class=annot href="#"><span class=annottext>{v : GHC.Integer.Type.Integer | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &gt; v}</span><span class='hs-varop'>&gt;</span></a> <span class='hs-num'>3</span>     <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer</span><span class='hs-num'>7</span></a><a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Integer.Type.Integer | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-varop'>*</span></a><a class=annot href="#"><span class=annottext>{v : GHC.Integer.Type.Integer | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Integer.Type.Integer | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <span class='hs-num'>2</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Integer.Type.Integer | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-varid'>foobar</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Integer.Type.Integer] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>284: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-varid'>foobar</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Integer.Type.Integer] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>
This seems straightforward enough, but it is not good Haskell
style. The problem is that it is

  * doing too much at once; and
  * working at too low of a level.

Instead of thinking about what we want to do with each element, we can
instead think about making incremental transformations to the entire
input, using the existing recursion patterns that we know of.  Here's
a much more idiomatic implementation of `foobar`:

<pre><span class=hs-linenum>297: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>foobar'</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Integer</span>
<span class=hs-linenum>298: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-definition'>foobar'</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-varid'>sum</span></a> <a class=annot href="#"><span class=annottext>([GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer)
-&gt; ([GHC.Integer.Type.Integer] -&gt; [GHC.Integer.Type.Integer])
-&gt; [GHC.Integer.Type.Integer]
-&gt; exists [[GHC.Integer.Type.Integer]].GHC.Integer.Type.Integer</span><span class='hs-varop'>.</span></a> <a class=annot href="#"><span class=annottext>(GHC.Integer.Type.Integer -&gt; GHC.Integer.Type.Integer)
-&gt; x3:[GHC.Integer.Type.Integer]
-&gt; {v : [GHC.Integer.Type.Integer] | len v == len x3}</span><span class='hs-varid'>map</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer -&gt; GHC.Integer.Type.Integer</span><span class='hs-keyglyph'>\</span></a><a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer</span><span class='hs-varid'>x</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer</span><span class='hs-num'>7</span></a><a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Integer.Type.Integer | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-varop'>*</span></a><a class=annot href="#"><span class=annottext>{v : GHC.Integer.Type.Integer | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Integer.Type.Integer | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <span class='hs-num'>2</span><span class='hs-layout'>)</span> <a class=annot href="#"><span class=annottext>([GHC.Integer.Type.Integer] -&gt; [GHC.Integer.Type.Integer])
-&gt; ([GHC.Integer.Type.Integer] -&gt; [GHC.Integer.Type.Integer])
-&gt; [GHC.Integer.Type.Integer]
-&gt; exists [[GHC.Integer.Type.Integer]].[GHC.Integer.Type.Integer]</span><span class='hs-varop'>.</span></a> <a class=annot href="#"><span class=annottext>(GHC.Integer.Type.Integer -&gt; GHC.Types.Bool)
-&gt; x3:[GHC.Integer.Type.Integer]
-&gt; {v : [GHC.Integer.Type.Integer] | len v &lt;= len x3}</span><span class='hs-varid'>filter</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Types.Bool | Prop v &lt;=&gt; x1 &gt; v}</span><span class='hs-varop'>&gt;</span></a><span class='hs-num'>3</span><span class='hs-layout'>)</span>
</pre>
This defines `foobar'` as a "pipeline" of three functions: first, we
throw away all elements from the list which are not greater than
three; next, we apply an arithmetic operation to every element of the
remaining list; finally, we sum the results.

Notice that in the above example, `map` and `filter` have been
partially applied.  For example, the type of `filter` is

~~~~ {.haskell}
(a -> Bool) -> [a] -> [a]
~~~~

Applying it to `(>3)` (which has type `Integer -> Bool`) results in a
function of type `[Integer] -> [Integer]`, which is exactly the right
sort of thing to compose with another function on `[Integer]`.

This style of coding in which we define a function without reference
to its arguments---in some sense saying what a function *is* rather
than what it *does*---is known as "point-free" style.  As we can see
from the above example, it can be quite beautiful.  Some people might
even go so far as to say that you should always strive to use
point-free style; but taken too far it can become extremely confusing.
`lambdabot` in the `#haskell` IRC channel has a command `@pl` for
turning functions into equivalent point-free expressions; here's an
example:

    @pl \f g x y -> f (x ++ g x) (g y)
    join . ((flip . ((.) .)) .) . (. ap (++)) . (.)

This is clearly *not* an improvement!

Folds
-----

We have one more recursion pattern on lists to talk about: folds.
Here are a few functions on lists that follow a similar pattern: all
of them somehow "combine" the elements of the list into a final
answer.

<pre><span class=hs-linenum>339: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>sum'</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Integer</span>
<span class=hs-linenum>340: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-definition'>sum'</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <span class='hs-num'>0</span>
<span class=hs-linenum>341: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>sum'</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Integer.Type.Integer | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Integer.Type.Integer | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-varid'>sum'</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Integer.Type.Integer] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>342: </span><span class='hs-varop'>&gt;</span>
<span class=hs-linenum>343: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>product'</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Integer</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Integer</span>
<span class=hs-linenum>344: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-definition'>product'</span></a> <span class='hs-conid'>[]</span> <span class='hs-keyglyph'>=</span> <span class='hs-num'>1</span>
<span class=hs-linenum>345: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>product'</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Integer.Type.Integer | v == x}</span><span class='hs-varid'>x</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Integer.Type.Integer | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-varop'>*</span></a> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-varid'>product'</span></a> <a class=annot href="#"><span class=annottext>{v : [GHC.Integer.Type.Integer] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
<span class=hs-linenum>346: </span><span class='hs-varop'>&gt;</span>
<span class=hs-linenum>347: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>length'</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>348: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>forall a. [a] -&gt; GHC.Types.Int</span><span class='hs-definition'>length'</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>x1:GHC.Prim.Int# -&gt; {v : GHC.Types.Int | v == (x1  :  int)}</span><span class='hs-num'>0</span></a>
<span class=hs-linenum>349: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>length'</span> <span class='hs-layout'>(</span><span class='hs-keyword'>_</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{v : GHC.Types.Int | v == (1  :  int)}</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Types.Int
-&gt; x2:GHC.Types.Int -&gt; {v : GHC.Types.Int | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>forall a. [a] -&gt; GHC.Types.Int</span><span class='hs-varid'>length'</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a>
</pre>
What do these three functions have in common, and what is different?
As usual, the idea will be to abstract out the parts that vary, aided
by the ability to define higher-order functions.

<pre><span class=hs-linenum>355: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>fold</span> <span class='hs-keyglyph'>::</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span>
<span class=hs-linenum>356: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>forall a b. a -&gt; (b -&gt; a -&gt; a) -&gt; [b] -&gt; a</span><span class='hs-definition'>fold</span></a> <a class=annot href="#"><span class=annottext>a</span><span class='hs-varid'>z</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; b</span><span class='hs-varid'>f</span></a> <span class='hs-conid'>[]</span>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>{VV : a | VV == z}</span><span class='hs-varid'>z</span></a>
<span class=hs-linenum>357: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>fold</span> <span class='hs-varid'>z</span> <span class='hs-varid'>f</span> <span class='hs-layout'>(</span><span class='hs-varid'>x</span><span class='hs-conop'>:</span><span class='hs-varid'>xs</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == x}</span><span class='hs-varid'>x</span></a> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>forall a b. a -&gt; (b -&gt; a -&gt; a) -&gt; [b] -&gt; a</span><span class='hs-varid'>fold</span></a> <a class=annot href="#"><span class=annottext>{VV : a | VV == z}</span><span class='hs-varid'>z</span></a> <a class=annot href="#"><span class=annottext>a -&gt; b -&gt; b</span><span class='hs-varid'>f</span></a> <a class=annot href="#"><span class=annottext>{v : [a] | v == xs &amp;&amp; len v &gt;= 0}</span><span class='hs-varid'>xs</span></a><span class='hs-layout'>)</span>
</pre>
Notice how `fold` essentially replaces `[]` with `z` and `(:)` with
`f`, that is,

    fold f z [a,b,c] == a `f` (b `f` (c `f` z))

(If you think about `fold` from this perspective, you may be able to
figure out how to generalize `fold` to data types other than lists...)

Now let's rewrite `sum'`, `product'`, and `length'` in terms of `fold`:

<pre><span class=hs-linenum>369: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-definition'>sum''</span></a>     <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer
-&gt; (GHC.Integer.Type.Integer
    -&gt; GHC.Integer.Type.Integer -&gt; GHC.Integer.Type.Integer)
-&gt; [GHC.Integer.Type.Integer]
-&gt; GHC.Integer.Type.Integer</span><span class='hs-varid'>fold</span></a> <span class='hs-num'>0</span> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Integer.Type.Integer | v == x1 + v}</span><span class='hs-layout'>(</span></a><span class='hs-varop'>+</span><span class='hs-layout'>)</span>
<span class=hs-linenum>370: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>[GHC.Integer.Type.Integer] -&gt; GHC.Integer.Type.Integer</span><span class='hs-definition'>product''</span></a> <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer
-&gt; (GHC.Integer.Type.Integer
    -&gt; GHC.Integer.Type.Integer -&gt; GHC.Integer.Type.Integer)
-&gt; [GHC.Integer.Type.Integer]
-&gt; GHC.Integer.Type.Integer</span><span class='hs-varid'>fold</span></a> <span class='hs-num'>1</span> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Integer.Type.Integer | x1 &gt; 0 &amp;&amp; x2 &gt; 0 =&gt; v &gt;= x1 &amp;&amp; v &gt;= x2 &amp;&amp; x1 &gt; 1 &amp;&amp; x2 &gt; 1 =&gt; v &gt; x1 &amp;&amp; v &gt; x2 &amp;&amp; x1 == 0 || x2 == 0 =&gt; v == 0}</span><span class='hs-layout'>(</span></a><span class='hs-varop'>*</span><span class='hs-layout'>)</span>
<span class=hs-linenum>371: </span><span class='hs-varop'>&gt;</span> <a class=annot href="#"><span class=annottext>forall a. [a] -&gt; GHC.Integer.Type.Integer</span><span class='hs-definition'>length''</span></a>  <span class='hs-keyglyph'>=</span> <a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer
-&gt; (a -&gt; GHC.Integer.Type.Integer -&gt; GHC.Integer.Type.Integer)
-&gt; [a]
-&gt; GHC.Integer.Type.Integer</span><span class='hs-varid'>fold</span></a> <span class='hs-num'>0</span> <span class='hs-layout'>(</span><a class=annot href="#"><span class=annottext>a -&gt; GHC.Integer.Type.Integer -&gt; GHC.Integer.Type.Integer</span><span class='hs-keyglyph'>\</span></a><span class='hs-keyword'>_</span> <a class=annot href="#"><span class=annottext>GHC.Integer.Type.Integer</span><span class='hs-varid'>s</span></a> <span class='hs-keyglyph'>-&gt;</span> <a class=annot href="#"><span class=annottext>{v : GHC.Integer.Type.Integer | v == 1}</span><span class='hs-num'>1</span></a> <a class=annot href="#"><span class=annottext>x1:GHC.Integer.Type.Integer
-&gt; x2:GHC.Integer.Type.Integer
-&gt; {v : GHC.Integer.Type.Integer | v == x1 + x2}</span><span class='hs-varop'>+</span></a> <a class=annot href="#"><span class=annottext>{v : GHC.Integer.Type.Integer | v == s}</span><span class='hs-varid'>s</span></a><span class='hs-layout'>)</span>
</pre>
(Instead of `(\_ s -> 1 + s)` we could also write `(\_ -> (1+))` or
even `(const (1+))`.)

Of course, `fold` is already provided in the standard Prelude, under
the name [`foldr`](http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:foldr). The arguments to `foldr` are in a slightly
different order but it's the exact same function.  Here are some
Prelude functions which are defined in terms of `foldr`:

  * `length`  `::          [a] -> Int`
  * `sum`     `:: Num a => [a] -> a`
  * `product` `:: Num a => [a] -> a`
  * `and`     `::          [Bool] -> Bool`
  * `or`      `::          [Bool] -> Bool`
  * `any`     `:: (a -> Bool) -> [a] -> Bool`
  * `all`     `:: (a -> Bool) -> [a] -> Bool`

There is also [`foldl`](http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:foldl), which folds "from the left".  That is,

    foldr f z [a,b,c] == a `f` (b `f` (c `f` z))
    foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c

In general, however, you should use [`foldl'` from
`Data.List`](http://haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#v:foldl)
instead, which does the same thing as `foldl` but is more efficient.
