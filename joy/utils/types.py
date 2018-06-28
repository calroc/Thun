from collections import Counter
from itertools import imap


class AnyJoyType(object):

    prefix = 'a'

    def __init__(self, number):
        self.number = number

    def __repr__(self):
        return self.prefix + str(self.number)

    def __eq__(self, other):
        return (
            isinstance(other, self.__class__)
            and other.prefix == self.prefix
            and other.number == self.number
        )

    def __ge__(self, other):
        return issubclass(other.__class__, self.__class__)

    def __add__(self, other):
        return self.__class__(self.number + other)
    __radd__ = __add__

    def __hash__(self):
        return hash(repr(self))


class BooleanJoyType(AnyJoyType): prefix = 'b'
class NumberJoyType(AnyJoyType): prefix = 'n'
class FloatJoyType(NumberJoyType): prefix = 'f'
class IntJoyType(FloatJoyType): prefix = 'i'

class StackJoyType(AnyJoyType):
    prefix = 's'
    def __nonzero__(self):
        # Imitate () at the end of cons list.
        return False


class JoyTypeError(Exception): pass


def update(s, term):
    if not isinstance(term, tuple):
        return s.get(term, term)
    return tuple(update(s, inner) for inner in term)


def relabel(left, right):
    return left, _1000(right)


def _1000(right):
    if not isinstance(right, tuple):
        return 1000 + right
    return tuple(_1000(n) for n in right)


def delabel(f, seen=None, c=None):
    if seen is None:
        assert c is None
        seen, c = {}, Counter()

    try:
        return seen[f]
    except KeyError:
        pass

    if not isinstance(f, tuple):
        try:
            seen[f] = f.__class__(c[f.prefix] + 1)
        except TypeError:  # FunctionJoyTypes break this.
            seen[f] = f
        else:
            c[f.prefix] += 1
        return seen[f]

    return tuple(delabel(inner, seen, c) for inner in f)


def stack_concat(q, e):
  return (q[0], stack_concat(q[1], e)) if q else e


def unify(u, v, s=None):
    if s is None:
        s = {}
    elif s:
        u = update(s, u)
        v = update(s, v)

    if u == v:
        return s

    if isinstance(u, AnyJoyType) and isinstance(v, AnyJoyType):
        if u >= v:
            s[u] = v
            return s
        if v >= u:
            s[v] = u
            return s
        raise JoyTypeError('Cannot unify %r and %r.' % (u, v))

    if isinstance(u, tuple) and isinstance(v, tuple):
        if len(u) != len(v) != 2:
            raise ValueError(repr((u, v)))
        for uu, vv in zip(u, v):
            s = unify(uu, vv, s)
            if s == False: # (instead of a substitution dict.)
                break
        return s

    if isinstance(v, tuple):
        if not stacky(u):
            raise JoyTypeError('Cannot unify %r and %r.' % (u, v))
        s[u] = v
        return s

    if isinstance(u, tuple):
        if not stacky(v):
            raise JoyTypeError('Cannot unify %r and %r.' % (v, u))
        s[v] = u
        return s

    return False


def stacky(thing):
    return thing.__class__ in {AnyJoyType, StackJoyType}


def compose(f, g):
    (f_in, f_out), (g_in, g_out) = f, g

    if not g_in:
        fg_in, fg_out = f_in, stack_concat(g_out, f_out)
    elif not f_out:
        fg_in, fg_out = stack_concat(f_in, g_in), g_out
    else:
        s = unify(g_in, f_out)
        if s == False:  # s can also be the empty dict, which is ok.
            raise JoyTypeError('Cannot unify %r and %r.' % (fo, gi))
        fg_in, fg_out = update(s, (f_in, g_out))

    return fg_in, fg_out


def _C(f, g):
    f, g = relabel(f, g)
    fg = compose(f, g)
    return delabel(fg)


def C(*functions):
    return reduce(_C, functions)


def compilable(f):
    return isinstance(f, tuple) and all(imap(compilable, f)) or stacky(f)


def doc_from_stack_effect(inputs, outputs):
    switch = [False]  # Do we need to display the '...' for the rest of the main stack?
    i, o = _f(inputs, switch), _f(outputs, switch)
    if switch[0]:
        i.append('...')
        o.append('...')
    return '(%s--%s)' % (
        ' '.join(reversed([''] + i)),
        ' '.join(reversed(o + [''])),
    )


def _f(term, switch):
    a = []
    while term and isinstance(term, tuple):
        item, term = term
        a.append(item)
    assert isinstance(term, (tuple, StackJoyType)), repr(term)
    a = [_to_str(i, term, switch) for i in a]
    return a


def _to_str(term, stack, switch):
    if not isinstance(term, tuple):
        if term == stack:
            switch[0] = True
            return '[...]'
        return (
            '[...%i]' % term.number
            if isinstance(term, StackJoyType)
            else str(term)
        )

    a = []
    while term and isinstance(term, tuple):
        item, term = term
        a.append(_to_str(item, stack, switch))
    assert isinstance(term, (tuple, StackJoyType)), repr(term)
    if term == stack:
        switch[0] = True
        end = '' if term == () else '...'
        #end = '...'
    else:
        end = '' if term == () else '...%i' % term.number
    a.append(end)
    return '[%s]' % ' '.join(a)


def compile_(name, f, doc=None):
    i, o = f
    if doc is None:
        doc = doc_from_stack_effect(i, o)
    return '''def %s(stack):
  """
  ::

    %s

  """
  %s = stack
  return %s''' % (name, doc, i, o)


def __(*seq):
    stack = StackJoyType(23)
    for item in seq: stack = item, stack
    return stack


_R = range(10)
A = a0, a1, a2, a3, a4, a5, a6, a7, a8, a9 = map(AnyJoyType, _R)
B = b0, b1, b2, b3, b4, b5, b6, b7, b8, b9 = map(BooleanJoyType, _R)
N = n0, n1, n2, n3, n4, n5, n6, n7, n8, n9 = map(NumberJoyType, _R)
S = s0, s1, s2, s3, s4, s5, s6, s7, s8, s9 = map(StackJoyType, _R)
F = f0, f1, f2, f3, f4, f5, f6, f7, f8, f9 = map(FloatJoyType, _R)
I = i0, i1, i2, i3, i4, i5, i6, i7, i8, i9 = map(IntJoyType, _R)


def defs():
    cons = __(a1, s0), __((a1, s0),)
    ccons = C(cons, cons)
    dup = __(a1,), __(a1, a1)
    dupd = __(a2, a1), __(a2, a2, a1)
    dupdd = __(a3, a2, a1), __(a3, a3, a2, a1)
    first = __((a1, s1),), __(a1,)
    over = __(a2, a1), __(a2, a1, a2)
    pop = __(a1), __()
    popd = __(a2, a1,), __(a1)
    popdd = __(a3, a2, a1,), __(a2, a1,)
    popop = __(a2, a1,), __()
    popopd = __(a3, a2, a1,), __(a1)
    popopdd = __(a4, a3, a2, a1,), __(a2, a1)
    rest = __((a1, s0),), __(s0,)
    rolldown = __(a1, a2, a3), __(a2, a3, a1)
    rollup = __(a1, a2, a3), __(a3, a1, a2)
    rrest = C(rest, rest)
    second = C(rest, first)
    stack = s0, (s0, s0)
    swaack = (s1, s0), (s0, s1)
    swap = __(a1, a2), __(a2, a1)
    swons = C(swap, cons)
    third = C(rest, second)
    tuck = __(a2, a1), __(a1, a2, a1)
    uncons = __((a1, s0),), __(a1, s0)
    unswons = C(uncons, swap)
    stuncons = C(stack, uncons)
    stununcons = C(stack, uncons, uncons)
    unit = __(a1), __((a1, ()))

    eq = ge = gt = le = lt = ne = __(n1, n2), __(b1)

    and_ = __(b1, b2), __(b3)
    bool_ = not_ = __(a1), __(b1)

    add = div = floordiv = modulus = mul = pow_ = sub = truediv = \
          lshift = rshift = __(n1, n2), __(n3,)
    sqrt = C(dup, mul)
    succ = pred = neg = __(n1,), __(n2,)
    divmod_ = pm = __(n2, n1), __(n4, n3)

    first_two = C(uncons, uncons, pop)
    fourth = C(rest, third)

    _Tree_add_Ee = C(pop, swap, rolldown, rrest, ccons)
    _Tree_get_E = C(popop, second)
    _Tree_delete_clear_stuff = C(rollup, popop, rest)
    _Tree_delete_R0 = C(over, first, swap, dup)

    return locals()


DEFS = defs()

#globals().update(DEFS)


def show():
    for name, stack_effect_comment in sorted(DEFS.iteritems()):
        t = ' *'[compilable(stack_effect_comment)]
        print name, '=', doc_from_stack_effect(*stack_effect_comment), t


def generate_library_code(f=None):
    if f is None:
        import sys
        f = sys.stdout
    print >> f, '# GENERATED FILE. DO NOT EDIT.\n'
    for name, stack_effect_comment in sorted(DEFS.iteritems()):
        if not compilable(stack_effect_comment):
            continue
        print >> f
        print >> f, compile_(name, stack_effect_comment)
        print >> f


if __name__ == '__main__':
    show()
