# -*- coding: utf_8
'''

Multiple Stack Effects

By adjusting the machinery in types.py to handles lists of stack effect comments
we can capture more information about the type signatures of some functions,
and we can introduce a kind of Kleene Star or sequence type that can stand for
an unbounded sequence of other types.

'''
from inspect import stack as inspect_stack
from itertools import chain, product
from logging import getLogger

_log = getLogger(__name__)

import joy.library
from joy.parser import Symbol
from joy.utils.stack import (
    concat as CONCAT,
    expression_to_string,
    list_to_stack,
    )
from joy.utils.types import (
  AnyJoyType, A,
  BooleanJoyType, B,
  DEFS,
  doc_from_stack_effect,
  FloatJoyType, F,
  JoyTypeError,
  NumberJoyType, N,
  StackJoyType, S,
  _stacky,
  _R,
  relabel, delabel,
  update,
  )


# We no longer want FloatJoyType to accept IntJoyType.
class IntJoyType(NumberJoyType): prefix = 'i'


class KleeneStar(object):
    '''
    A sequence of zero or more `AnyJoyType` variables would be:

       A*

    The `A*` works by splitting the universe into two alternate histories:

       A* -> 0

       A* -> A A*

    The Kleene star variable disappears in one universe, and in the other
    it turns into an `AnyJoyType` variable followed by itself again.

    We have to return all universes (represented by their substitution
    dicts, the "unifiers") that don't lead to type conflicts.
    '''

    kind = AnyJoyType

    def __init__(self, number):
        assert number
        self.number = number
        self.count = 0
        self.prefix = repr(self)

    def __repr__(self):
        return '%s%i*' % (self.kind.prefix, self.number)

    def another(self):
        self.count += 1
        return self.kind(10000 * self.number + self.count)

    def __eq__(self, other):
        return (
            isinstance(other, self.__class__)
            and other.number == self.number
        )

    def __ge__(self, other):
        return self.kind >= other.kind

    def __add__(self, other):
        return self.__class__(self.number + other)
    __radd__ = __add__
    
    def __hash__(self):
        return hash(repr(self))


class AnyStarJoyType(KleeneStar): kind = AnyJoyType
class NumberStarJoyType(KleeneStar): kind = NumberJoyType
#class FloatStarJoyType(KleeneStar): kind = FloatJoyType
#class IntStarJoyType(KleeneStar): kind = IntJoyType
class StackStarJoyType(KleeneStar): kind = StackJoyType


class FunctionJoyType(AnyJoyType):

    def __init__(self, name, sec, number):
        self.name = name
        self.stack_effects = sec
        self.number = number

    def __add__(self, other):
        return self
    __radd__ = __add__

    def __repr__(self):
        return self.name


class SymbolJoyType(FunctionJoyType):
    '''
    Represent non-combinator functions.

    These type variables carry the stack effect comments and can
    appear in expressions (as in quoted programs.)
    '''
    prefix = 'F'


class CombinatorJoyType(FunctionJoyType):
    '''
    Represent combinators.
    
    These type variables carry Joy functions that implement the
    behaviour of Joy combinators and they can appear in expressions.
    For simple combinators the implementation functions can be the
    combinators themselves.

    These types can also specify a stack effect (input side only) to
    guard against being used on invalid types.
    '''

    prefix = 'C'

    def __init__(self, name, sec, number, expect=None):
        super(CombinatorJoyType, self).__init__(name, sec, number)
        self.expect = expect

    def enter_guard(self, f):
        if self.expect is None:
            return f
        g = self.expect, self.expect
        new_f = list(compose(f, g, ()))
        assert len(new_f) == 1, repr(new_f)
        return new_f[0][1]


def unify(u, v, s=None):
    '''
    Return a tuple of substitution dicts representing unifiers for u and v.
    '''
    if s is None:
        s = {}
    elif s:
        u = update(s, u)
        v = update(s, v)

    if u == v:
        res = s,

    elif isinstance(u, tuple) and isinstance(v, tuple):
        if len(u) != 2 or len(v) != 2:
            raise ValueError(repr((u, v)))  # Bad input.

        (a, b), (c, d) = v, u
        if isinstance(a, KleeneStar):
            if isinstance(c, KleeneStar):
                s = _lil_uni(a, c, s)  # Attempt to unify the two K-stars.
                res = unify(d, b, s[0])

            else:
                # Two universes, in one the Kleene star disappears and
                # unification continues without it...
                s0 = unify(u, b)
                
                # In the other it spawns a new variable.
                s1 = unify(u, (a.another(), v))
                
                res = s0 + s1
                for sn in res:
                    sn.update(s)

        elif isinstance(c, KleeneStar):
            res = unify(v, d) + unify(v, (c.another(), u))
            for sn in res:
              sn.update(s)

        else:
            res = tuple(flatten(unify(d, b, sn) for sn in unify(c, a, s)))
 
    elif isinstance(v, tuple):
        if not _stacky(u):
            raise JoyTypeError('Cannot unify %r and %r.' % (u, v))
        s[u] = v
        res = s,

    elif isinstance(u, tuple):
        if not _stacky(v):
            raise JoyTypeError('Cannot unify %r and %r.' % (v, u))
        s[v] = u
        res = s,

    else:
        res = _lil_uni(u, v, s)

    return res


def _lil_uni(u, v, s):
    if u >= v:
        s[u] = v
        return s,
    if v >= u:
        s[v] = u
        return s,
    raise JoyTypeError('Cannot unify %r and %r.' % (u, v))


def _compose(f, g, e):
    (f_in, f_out), (g_in, g_out) = f, g
    for s in unify(g_in, f_out):
        yield update(s, (e, (f_in, g_out)))


def compose(f, g, e):
    '''
    Yield the stack effects of the composition of two stack effects.  An
    expression is carried along and updated and yielded.
    '''
    f, g = relabel(f, g)
    for fg in _compose(f, g, e):
        yield delabel(fg)


def _meta_compose(F, G, e):
    for f, g in product(F, G):
        try:
            for result in compose(f, g, e): yield result
        except JoyTypeError:
            pass


def meta_compose(F, G, e):
    '''
    Yield the stack effects of the composition of two lists of stack
    effects.  An expression is carried along and updated and yielded.
    '''
    res = sorted(set(_meta_compose(F, G, e)))
    if not res:
        raise JoyTypeError('Cannot unify %r and %r.' % (F, G))
    return res


def flatten(g):
  return list(chain.from_iterable(g))


ID = S[0], S[0]  # Identity function.


def _infer(e, F=ID):
    _log_it(e, F)
    if not e:
        return [F]

    n, e = e

    if isinstance(n, SymbolJoyType):
        eFG = meta_compose([F], n.stack_effects, e)
        res = flatten(_infer(e, Fn) for e, Fn in eFG)

    elif isinstance(n, CombinatorJoyType):
        fi, fo = n.enter_guard(F)
        res = flatten(_interpret(f, fi, fo, e) for f in n.stack_effects)

    elif isinstance(n, Symbol):
        assert n not in FUNCTIONS, repr(n)
        func = joy.library._dictionary[n]
        res = _interpret(func, F[0], F[1], e)

    else:
        fi, fo = F
        res = _infer(e, (fi, (n, fo)))

    return res


def _interpret(f, fi, fo, e):
  new_fo, ee, _ = f(fo, e, {})
  ee = update(FUNCTIONS, ee)  # Fix Symbols.
  new_F = fi, new_fo
  return _infer(ee, new_F)


def _log_it(e, F):
    _log.info(
        u'%3i %s ∘ %s',
        len(inspect_stack()),
        doc_from_stack_effect(*F),
        expression_to_string(e),
        )


def infer(*expression):
    '''
    Return a list of stack effects for a Joy expression.

    For example::

        h = infer(pop, swap, rolldown, rest, rest, cons, cons)
        for fi, fo in h:
            print doc_from_stack_effect(fi, fo)

    Prints::

        ([a4 a5 ...1] a3 a2 a1 -- [a2 a3 ...1])

    '''
    return sorted(set(_infer(list_to_stack(expression))))


a0, a1, a2, a3, a4, a5, a6, a7, a8, a9 = A
b0, b1, b2, b3, b4, b5, b6, b7, b8, b9 = B
n0, n1, n2, n3, n4, n5, n6, n7, n8, n9 = N
f0, f1, f2, f3, f4, f5, f6, f7, f8, f9 = F
i0, i1, i2, i3, i4, i5, i6, i7, i8, i9 = I = map(IntJoyType, _R)
s0, s1, s2, s3, s4, s5, s6, s7, s8, s9 = S

_R = range(1, 11)
As = map(AnyStarJoyType, _R)
Ns = map(NumberStarJoyType, _R)
Ss = map(StackStarJoyType, _R)


'''Docstring for functions in Sphinx?'''
FUNCTIONS = {
    name: SymbolJoyType(name, [DEFS[name]], i)
    for i, name in enumerate('''
        ccons cons divmod dup dupd dupdd first first_two fourth over pop
        popd popdd popop popopd popopdd rest rrest rolldown rollup second
        stack swaack swap swons third tuck uncons unswons stuncons
        stununcons unit eq ge gt le lt ne and bool not
        _Tree_add_Ee _Tree_delete_R0 _Tree_delete_clear_stuff _Tree_get_E
        '''.strip().split())
    }


def defs():
    '''
    Return a dict of FunctionJoyType instances to be used with ``infer()``.
    '''

    sum_ = product = [(((Ns[1], s1), s0), (n0, s0))]

    add = mul = sub = floordiv = modulus = [
        ((i2, (i1, s0)), (i3, s0)),
        ((f2, (i1, s0)), (f3, s0)),
        ((i2, (f1, s0)), (f3, s0)),
        ((f2, (f1, s0)), (f3, s0)),
        ]

    div = truediv = pow_ = [
        ((i2, (i1, s0)), (f3, s0)),
        ((f2, (i1, s0)), (f3, s0)),
        ((i2, (f1, s0)), (f3, s0)),
        ((f2, (f1, s0)), (f3, s0)),
        ]

    lshift = rshift = [((i2, (i1, s0)), (i3, s0))]

    neg = pred = succ = [((n1, s0), (n2, s0))]

    sqrt = [((n1, s0), (f2, s0))]

    pm = divmod_ = [
        ((i2, (i1, s0)), (i3, (i4, s0))),
        ((f2, (i1, s0)), (f3, (f4, s0))),
        ((i2, (f1, s0)), (f3, (f4, s0))),
        ((f2, (f1, s0)), (f3, (f4, s0))),
        ]

    return {
        name.rstrip('_'): stack_effect
        for name, stack_effect in locals().iteritems()
        }


FUNCTIONS.update({
    name: SymbolJoyType(name, stack_effect, i)
    for i, (name, stack_effect) in enumerate(defs().iteritems())
    })
FUNCTIONS.update({
    combo.__name__: CombinatorJoyType(combo.__name__, [combo], i)
    for i, combo in enumerate((
        joy.library.b,
        joy.library.concat_,
        joy.library.dip,
        joy.library.dipd,
        joy.library.dipdd,
        joy.library.dupdip,
        joy.library.i,
        joy.library.infra,
        joy.library._dictionary['nullary'],
        joy.library.x,
        ))
    })

def branch_true(stack, expression, dictionary):
  (then, (else_, (flag, stack))) = stack
  return stack, CONCAT(then, expression), dictionary

def branch_false(stack, expression, dictionary):
  (then, (else_, (flag, stack))) = stack
  return stack, CONCAT(else_, expression), dictionary

FUNCTIONS['branch'] = CombinatorJoyType('branch', [branch_true, branch_false], 100)
pop = FUNCTIONS['pop']

def loop_true(stack, expression, dictionary):
    quote, (flag, stack) = stack
    return stack, CONCAT(quote, (pop, expression)), dictionary

def loop_two_true(stack, expression, dictionary):
    quote, (flag, stack) = stack
    return stack, CONCAT(quote, (pop, CONCAT(quote, (pop, expression)))), dictionary

def loop_false(stack, expression, dictionary):
    quote, (flag, stack) = stack
    return stack, expression, dictionary

FUNCTIONS['loop'] = CombinatorJoyType('loop', [loop_two_true, loop_true, loop_false], 101)


def set_expectations():
    branch.expect = s7, (s6, (b1, s5))
    loop.expect = s6, (b1, s5)
    i.expect = nullary.expect = x.expect = s7, s6
    dip.expect = dupdip.expect = s8, (a8, s7)
    dipd.expect = s8, (a8, (a7, s7))
    dipdd.expect = s8, (a8, (a7, (a6, s7)))
    b.expect = concat_.expect = infra.expect = s8, (s7, s6)
scope = globals().copy()
scope.update(FUNCTIONS)
eval(set_expectations.func_code, scope)


# Type Checking...

def _ge(self, other):
    return (issubclass(other.__class__, self.__class__)
            or hasattr(self, 'accept')
            and isinstance(other, self.accept))

AnyJoyType.__ge__ = _ge
AnyJoyType.accept = tuple, int, float, long, str, unicode, bool, Symbol
StackJoyType.accept = tuple
FloatJoyType.accept = float
