from collections import Counter
from itertools import imap
from joy.utils.stack import concat
from joy.parser import Symbol


class AnyJoyType(object):
    '''
    Joy type variable.  Represents any Joy value.
    '''

    accept = tuple, int, float, long, str, unicode, bool, Symbol
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
        return (
            issubclass(other.__class__, self.__class__)
            or isinstance(other, self.accept)
            )

    def __le__(self, other):
        # 'a string' >= AnyJoyType() should be False.
        return issubclass(self.__class__, other.__class__)

    def __add__(self, other):
        return self.__class__(self.number + other)
    __radd__ = __add__

    def __hash__(self):
        return hash(repr(self))


class BooleanJoyType(AnyJoyType):
    accept = bool
    prefix = 'b'


class NumberJoyType(AnyJoyType):
    accept = int, float, long, complex
    prefix = 'n'


class FloatJoyType(NumberJoyType):
    accept = float
    prefix = 'f'


class IntJoyType(FloatJoyType):
    accept = int
    prefix = 'i'


class StackJoyType(AnyJoyType):

    accept = tuple
    prefix = 's'

    def __nonzero__(self):
        # Imitate () at the end of cons list.
        return False


class JoyTypeError(Exception): pass


def reify(meaning, name, seen=None):
  '''
  Apply substitution dict to term, returning new term.
  '''
  if isinstance(name, tuple):
    return tuple(reify(meaning, inner) for inner in name)
  safety = 101
  while name in meaning and safety:
    safety -= 1
    name = meaning[name]
  if not safety:
      raise ValueError('Cycle in substitution dict: %s' % (meaning,))
  return name


def relabel(left, right):
    '''
    Re-number type variables to avoid collisions between stack effects.
    '''
    return left, _1000(right)


def _1000(right):
    if not isinstance(right, tuple):
        return 1000 + right
    return tuple(_1000(n) for n in right)


def delabel(f, seen=None, c=None):
    '''
    Fix up type variable numbers after relabel().
    '''
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


def unify(u, v, s=None):
    '''
    Return a substitution dict representing a unifier for u and v.
    '''
    if s is None:
        s = {}
    elif s:
        u = reify(s, u)
        v = reify(s, v)

    if isinstance(u, AnyJoyType) and isinstance(v, AnyJoyType):
        if u >= v:
            s[u] = v
        elif v >= u:
            s[v] = u
        else:
            raise JoyTypeError('Cannot unify %r and %r.' % (u, v))

    elif isinstance(u, tuple) and isinstance(v, tuple):
        if len(u) != len(v) != 2:
            raise ValueError(repr((u, v)))  # Bad input.
        (a, b), (c, d) = u, v
        s = unify(b, d, unify(a, c, s))

    elif isinstance(v, tuple):
        if not _stacky(u):
            raise JoyTypeError('Cannot unify %r and %r.' % (u, v))
        s[u] = v

    elif isinstance(u, tuple):
        if not _stacky(v):
            raise JoyTypeError('Cannot unify %r and %r.' % (v, u))
        s[v] = u

    else:
        raise JoyTypeError('Cannot unify %r and %r.' % (u, v))

    return s


def _stacky(thing):
    return thing.__class__ in {AnyJoyType, StackJoyType}


def _compose(f, g):
    '''
    Return the stack effect of the composition of two stack effects.
    '''
    # Relabel, unify, update, delabel.
    (f_in, f_out), (g_in, g_out) = relabel(f, g)
    fg = reify(unify(g_in, f_out), (f_in, g_out))
    return delabel(fg)


def compose(*functions):
    '''
    Return the stack effect of the composition of some of stack effects.
    '''
    return reduce(_compose, functions)


def compilable(f):
    '''
    Return True if a stack effect represents a function that can be
    automatically compiled (to Python), False otherwise.
    '''
    return isinstance(f, tuple) and all(imap(compilable, f)) or _stacky(f)


def doc_from_stack_effect(inputs, outputs):
    '''
    Return a crude string representation of a stack effect.
    '''
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
    '''
    Return a string of Python code implementing the function described
    by the stack effect.  If no doc string is passed doc_from_stack_effect()
    is used to generate one.
    '''
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
    '''
    Return a dict of named stack effects.
    '''
    at = __(s0, i1), __(a1)
    drop = take = __(s0, i1), __(s1)
    cons = __(a1, s0), __((a1, s0),)
    ccons = compose(cons, cons)
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
    rrest = compose(rest, rest)
    second = compose(rest, first)
    stack = s0, (s0, s0)
    swaack = (s1, s0), (s0, s1)
    swap = __(a1, a2), __(a2, a1)
    swons = compose(swap, cons)
    third = compose(rest, second)
    tuck = __(a2, a1), __(a1, a2, a1)
    uncons = __((a1, s0),), __(a1, s0)
    unswons = compose(uncons, swap)
    stuncons = compose(stack, uncons)
    stununcons = compose(stack, uncons, uncons)
    unit = __(a1), __((a1, ()))

    eq = ge = gt = le = lt = ne = __(n1, n2), __(b1)

    and_ = __(b1, b2), __(b3)
    bool_ = not_ = __(a1), __(b1)

    add = div = floordiv = modulus = mul = pow_ = sub = truediv = \
          lshift = rshift = __(n1, n2), __(n3,)
    sqrt = compose(dup, mul)
    succ = pred = neg = __(n1,), __(n2,)
    divmod_ = pm = __(n2, n1), __(n4, n3)

    first_two = compose(uncons, uncons, pop)
    fourth = compose(rest, third)
    of = compose(swap, at)

    _Tree_add_Ee = compose(pop, swap, rolldown, rrest, ccons)
    _Tree_get_E = compose(popop, second)
    _Tree_delete_clear_stuff = compose(rollup, popop, rest)
    _Tree_delete_R0 = compose(over, first, swap, dup)

    return {
        name.rstrip('_'): stack_effect
        for name, stack_effect in locals().iteritems()
        }


DEFS = defs()


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
