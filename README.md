# justify-kp

Paragraph justification for emacs using Knuth/Plass algorithm

# What it supports so far?

* [X] hanging punctuation (on the right margin)
* [ ] hanging punctuation (on the left margin)
* [ ] hyphenation
* [ ] character protrusion, i.e. extending W, Q, V, Y etc. out of the margins a bit to get better optical whitespace. It is very similar to hanging punctuation in effect.
* [ ] kerning around special characters (useful for quotes, parens etc.)

See also: http://www.khirevich.com/latex/microtype/

# How it looks?

Here's a youtube video of the algorithm in action: http://www.youtube.com/watch?v=10FbsTP_IRg (note that the dynamic reindentation is very inefficient at the present moment.)

Comparsions of `M-q` (top) and the present implementation (bottom). Right now there's no support for hyphenation but it is a very high priority.

![](http://i.imgur.com/ibZGDLZ.png)

![](http://i.imgur.com/tJviUxp.png)
