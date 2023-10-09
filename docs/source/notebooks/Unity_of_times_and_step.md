
[... [...] [... x] branch] x


step [_step0] x
_step0 _step1 [popopop] [_stept] branch
_step1 [?] dipd roll<
_stept [uncons] dipd [dupdipd] dip x


times [_times0] x
_times0 _times1 [popopop] [_timest] branch
_times1 [dup 0 >] dipd roll<
_timest [--] dipd [dupdipd] dip x


[[A] dipd roll< [popopop] [[B] dipd [dupdipd] dip x] branch] x


H == dipd [dupdipd] dip
J == dipd roll<

[[A] J [popopop] [[B] H x] branch] x

[A]         [B]
[?]         [uncons]   step
[dup 0 >]   [--]       times


