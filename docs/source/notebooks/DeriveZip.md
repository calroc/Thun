# Zip

Let's derive `zip`.

       [a b c ...] [e f g ...] zip
    ---------------------------------
         [[a e] [b f] [c g] ...]

It's a `genrec`:

    zip == [null] [popop []] [R0] [R1] genrec

If the top list is empty, pop both lists and put a new empty list...

Hmm...

    zip == [null] [pop] [R0] [R1] genrec

We will assume that both lists are the same size, so if the top list is empty the second list shall be too, and we can reuse it to store our pairs.

Now then, we have two non-empty lists:

    [a b c ...] [e f g ...] R0 [zip] R1

Let's imagine a function `uncons-pair`:

       [a ...] [e ...] uncons-pair
    --------------------------------
           [a e] [...] [...]

I'm going to defer derivation of that for now.

    [a b c ...] [e f g ...] uncons-pair [zip] R1

    [a e] [b c ...] [f g ...] [zip] R1

And so `R1` is `i cons` (it's a list builder.)

    zip == [null] [pop] [uncons-pair] [i cons] genrec

And now:

    uncons-pair == uncons-two [quote-two] dipd

w/

    uncons-two == [uncons] ii swapd
    quote-two == unit cons

    [zip [null] [pop] [uncons-pair] [i cons] genrec] inscribe
    [uncons-pair uncons-two [quote-two] dipd] inscribe
    [uncons-two [uncons] ii swapd] inscribe
    [quote-two unit cons] inscribe

