'''

Multiple Stack Effects

By adjusting the machinery in types.py to handles lists of stack effect comments
we can capture more information about the type signatures of some functions,
and we can introduce a kind of Kleene Star or sequence type that can stand for
an unbounded sequence of other types.

'''
from itertools import chain, product

import joy.library
from joy.parser import Symbol
from joy.utils.stack import concat as CONCAT
from joy.utils.types import (
  AnyJoyType, A,
  C,
  DEFS,
  doc_from_stack_effect,
  FloatJoyType,
  JoyTypeError,
  NumberJoyType, N,
  StackJoyType, S,
  stacky,
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


class SymbolJoyType(FunctionJoyType): prefix = 'F'


class CombinatorJoyType(FunctionJoyType):

    prefix = 'C'

    def __init__(self, name, sec, number, expect=None):
        super(CombinatorJoyType, self).__init__(name, sec, number)
        self.expect = expect

    def enter_guard(self, f):
        if self.expect is None:
            return f
        g = self.expect, self.expect
        new_f = list(C(f, g))
        assert len(new_f) == 1, repr(new_f)
        return new_f[0]



def unify(u, v, s=None):
    if s is None:
        s = {}
    elif s:
        u = update(s, u)
        v = update(s, v)

    if u == v:
        return s,

    if isinstance(u, AnyJoyType) and isinstance(v, AnyJoyType):
        return _lil_uni(u, v, s)

    if isinstance(u, tuple) and isinstance(v, tuple):
        if len(u) != 2 or len(v) != 2:
            raise JoyTypeError('Cannot unify %r and %r.' % (u, v))

        (a, b), (c, d) = v, u
        if isinstance(a, KleeneStar):
            if isinstance(c, KleeneStar):
                s = _lil_uni(a, c, s)  # Attempt to unify the two K-stars.
                return unify(d, b, s[0])

            # Two universes, in one the Kleene star disappears and unification
            # continues without it...
            s0 = unify(u, b)
            
            # In the other it spawns a new variable.
            s1 = unify(u, (a.another(), v))
            
            t = s0 + s1
            for sn in t:
                sn.update(s)
            return t

        if isinstance(c, KleeneStar):
            t = unify(v, d) + unify(v, (c.another(), u))
            for sn in t:
              sn.update(s)
            return t

        return tuple(flatten(unify(d, b, sn) for sn in unify(c, a, s)))
 
    if isinstance(v, tuple):
        if not stacky(u):
            raise JoyTypeError('Cannot unify %r and %r.' % (u, v))
        s[u] = v
        return s,

    if isinstance(u, tuple):
        if not stacky(v):
            raise JoyTypeError('Cannot unify %r and %r.' % (v, u))
        s[v] = u
        return s,

    return _lil_uni(u, v, s)


def _lil_uni(u, v, s):
    if u >= v:
        s[u] = v
        return s,
    if v >= u:
        s[v] = u
        return s,
    raise JoyTypeError('Cannot unify %r and %r.' % (u, v))


def compose(f, g):
    (f_in, f_out), (g_in, g_out) = f, g
    for s in unify(g_in, f_out):
        yield update(s, (f_in, g_out))


def C(f, g):
    f, g = relabel(f, g)
    for fg in compose(f, g):
        yield delabel(fg)


def meta_compose(F, G):
    for f, g in product(F, G):
        try:
            for result in C(f, g): yield result
        except JoyTypeError:
            pass


def MC(F, G):
    res = sorted(set(meta_compose(F, G)))
    if not res:
        raise JoyTypeError('Cannot unify %r and %r.' % (F, G))
    return res


def flatten(g):
  return list(chain.from_iterable(g))


ID = S[0], S[0]  # Identity function.


def infer(e, F=ID):
    if not e:
        return [F]

    n, e = e

    if isinstance(n, SymbolJoyType):
        res = flatten(infer(e, Fn) for Fn in MC([F], n.stack_effects))

    elif isinstance(n, CombinatorJoyType):
        fi, fo = n.enter_guard(F)
        res = []
        for combinator in n.stack_effects:
            print fo
            print e
            new_fo, ee, _ = combinator(fo, e, {})
            print new_fo
            print ee
            ee = update(FUNCTIONS, ee)  # Fix Symbols.
            new_F = fi, new_fo
            res.extend(infer(ee, new_F))
    else:
        lit = s9, (n, s9)
        res = flatten(infer(e, Fn) for Fn in MC([F], [lit]))

    return res


a0, a1, a2, a3, a4, a5, a6, a7, a8, a9 = A
n0, n1, n2, n3, n4, n5, n6, n7, n8, n9 = N
s0, s1, s2, s3, s4, s5, s6, s7, s8, s9 = S
f0, f1, f2, f3, f4, f5, f6, f7, f8, f9 = F = map(FloatJoyType, _R)
i0, i1, i2, i3, i4, i5, i6, i7, i8, i9 = I = map(IntJoyType, _R)

_R = range(1, 11)
As = map(AnyStarJoyType, _R)
Ns = map(NumberStarJoyType, _R)
Ss = map(StackStarJoyType, _R)


FUNCTIONS = {
    name: SymbolJoyType(name, [DEFS[name]], i)
    for i, name in enumerate('''
        ccons cons divmod_ dup dupd first
        over pm pop popd popdd popop pred
        rest rolldown rollup rrest second
        sqrt stack succ swaack swap swons
        third tuck uncons
        '''.strip().split())
    }
FUNCTIONS['sum'] = SymbolJoyType('sum', [(((Ns[1], s1), s0), (n0, s0))], 100)
FUNCTIONS['mul'] = SymbolJoyType('mul', [
     ((i2, (i1, s0)), (i3, s0)),
     ((f2, (i1, s0)), (f3, s0)),
     ((i2, (f1, s0)), (f3, s0)),
     ((f2, (f1, s0)), (f3, s0)),
], 101)
FUNCTIONS.update({
    combo.__name__: CombinatorJoyType(combo.__name__, [combo], i)
    for i, combo in enumerate((
        joy.library.i,
        joy.library.dip,
        joy.library.dipd,
        joy.library.dipdd,
        joy.library.dupdip,
        joy.library.b,
        joy.library.x,
        joy.library.infra,
        ))
    })

def branch_true(stack, expression, dictionary):
  (then, (else_, (flag, stack))) = stack
  return stack, CONCAT(then, expression), dictionary

def branch_false(stack, expression, dictionary):
  (then, (else_, (flag, stack))) = stack
  return stack, CONCAT(else_, expression), dictionary

FUNCTIONS['branch'] = CombinatorJoyType('branch', [branch_true, branch_false], 100)


globals().update(FUNCTIONS)


dip.expect = s8, (a8, s7)


def _ge(self, other):
    return (issubclass(other.__class__, self.__class__)
            or hasattr(self, 'accept')
            and isinstance(other, self.accept))

AnyJoyType.__ge__ = _ge
AnyJoyType.accept = tuple, int, float, long, str, unicode, bool, Symbol
StackJoyType.accept = tuple


##if __name__ == '__main__':
##
##  from joy.parser import text_to_expression
##  from joy.utils.stack import list_to_stack as l2s
##
##
##  F = infer(l2s((pop, pop, pop)))
##  for f in F:
##      print doc_from_stack_effect(*f)
##  s = text_to_expression('0 1 2')
##  L = unify(s, F[0][0])
##  print L
##
##  print
##
##  F = infer(l2s((pop, swap, rolldown, rest, rest, cons, cons)))
##  for f in F:
##      print doc_from_stack_effect(*f)
##  s = text_to_expression('0 1 2 [3 4]')
##  L = unify(s, F[0][0])
##  print L
##  print
##
##  g = update(L[0], F[0])
##  print doc_from_stack_effect(*g)
##  print g
##
##
##  print '- - - - - -'
##  s = text_to_expression('[3 4]')
##  L = unify(s, F[0][1])
##  print L
##  print
##
##  g = update(L[0], F[0])
##  print doc_from_stack_effect(*g)
##  print g
##
