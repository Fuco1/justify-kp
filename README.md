# justify-kp

Paragraph justification for emacs using Knuth/Plass algorithm.

# Usage

The lowest level interactive command is `pj-justify`.  It justifies
current line *after* the point.  You must place the point at the
beginning of line and the line must have no line breaks (newlines)
inside it.

More convenient set of commands is `pj-justify-paragraph` and
`pj-unjustify-paragraph`.  They can be called from anywhere inside
current paragraph to justify or unjustify it.  Calling
`pj-justify-paragraph` on an already justified paragraph will
re-justify it.

If you want to automatically re-justify paragraphs as you type, you
can use `pj-auto-justify-mode`.  This is however a bit rough around
the edges, so your mileage might vary.

# What it supports so far?

* [X] hanging punctuation (on the right margin)
* [ ] hanging punctuation (on the left margin)
* [ ] hyphenation
* [ ] character protrusion, i.e. extending W, Q, V, Y etc. out of the margins a bit to get better optical whitespace. It is very similar to hanging punctuation in effect.
* [ ] kerning around special characters (useful for quotes, parens etc.)
* [X] mixed fonts in the same paragraph

See also: http://www.khirevich.com/latex/microtype/

# How it looks?

Here's a youtube video of the algorithm in action: http://www.youtube.com/watch?v=10FbsTP_IRg (note that the dynamic reindentation is very inefficient at the present moment.)

Comparsions of `M-q` (top) and the present implementation (bottom). Right now there's no support for hyphenation but it is a very high priority.

![](http://i.imgur.com/ibZGDLZ.png)

![](http://i.imgur.com/tJviUxp.png)

We can also do mixed-fonts!

![](http://i.imgur.com/B1Z3awR.png)
