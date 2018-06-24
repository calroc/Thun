'''

Multiple Stack Effects

By adjusting the machinery in types.py to handles lists of stackeffect comments
we can capture more information about the type signatures of some functions,
and we can introduce a kind of Kleene Star or sequence type that can stand for
an unbounded sequence of other types.

'''
import sys
sys.path.append('/home/sforman/Desktop/Joypy-hg')
from itertools import product
from joy.utils.types import (
  AnyJoyType, A,
  C,
  DEFS,
  doc_from_stack_effect,
  FloatJoyType,
  NumberJoyType, N,
  StackJoyType, S,
  stacky,
  _R,
  relabel, delabel,
  update,
  )


# We no longer want FloatJoyType to accept IntJoyType.
class IntJoyType(NumberJoyType): prefix = 'i'


a0, a1, a2, a3, a4, a5, a6, a7, a8, a9 = A
n0, n1, n2, n3, n4, n5, n6, n7, n8, n9 = N
s0, s1, s2, s3, s4, s5, s6, s7, s8, s9 = S
f0, f1, f2, f3, f4, f5, f6, f7, f8, f9 = F = map(FloatJoyType, _R)
i0, i1, i2, i3, i4, i5, i6, i7, i8, i9 = I = map(IntJoyType, _R)


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


As = map(AnyStarJoyType, _R)
Ns = map(NumberStarJoyType, _R)
Ss = map(StackStarJoyType, _R)


def _lil_uni(u, v, s):
    if u >= v:
        s[u] = v
        return s,
    if v >= u:
        s[v] = u
        return s,
    raise TypeError('Cannot unify %r and %r.' % (u, v))



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
        if len(u) != len(v) != 2:
            raise TypeError(repr((u, v)))
            
        a, b = v
        if isinstance(a, KleeneStar):
            if isinstance(b, KleeneStar):
                return _lil_uni(a, b, s)

            # Two universes, in one the Kleene star disappears and unification
            # continues without it...
            s0 = unify(u, b)
            
            # In the other it spawns a new variable.
            s1 = unify(u, (a.another(), v))
            
            t = s0 + s1
            for sn in t:
                sn.update(s)
            return t

        a, b = u
        if isinstance(a, KleeneStar):
            s0 = unify(v, b)
            s1 = unify(v, (a.another(), u))
            t = s0 + s1
            for sn in t:
                sn.update(s)
            return t

        ses = unify(u[0], v[0], s)
        results = ()
        for sn in ses:
            results += unify(u[1], v[1], sn)
        return results
 
    if isinstance(v, tuple):
        if not stacky(u):
            raise TypeError('Cannot unify %r and %r.' % (u, v))
        s[u] = v
        return s,

    if isinstance(u, tuple):
        if not stacky(v):
            raise TypeError('Cannot unify %r and %r.' % (v, u))
        s[v] = u
        return s,

    return ()


def compose(f, g):
    (f_in, f_out), (g_in, g_out) = f, g
    s = unify(g_in, f_out)
    if not s:
        raise TypeError('Cannot unify %r and %r.' % (fo, gi))
    for result in s:
        yield update(result, (f_in, g_out))


def C(f, g):
    f, g = relabel(f, g)
    for fg in compose(f, g):
        yield delabel(fg)


def meta_compose(F, G):
    for f, g in product(F, G):
        try:
            for result in C(f, g):
                yield result
        except TypeError:
            pass


def MC(F, G):
    res = sorted(set(meta_compose(F, G)))
    if not res:
        raise TypeError('Cannot unify %r and %r.' % (F, G))
    return res



mul = [
     ((i2, (i1, s0)), (i3, s0)),
     ((f2, (i1, s0)), (f3, s0)),
     ((i2, (f1, s0)), (f3, s0)),
     ((f2, (f1, s0)), (f3, s0)),
]


dup = [DEFS['dup']]
cons = [DEFS['cons']]


sum_ = [(((Ns[1], s1), s0), (n0, s0))]
f = [(s0, ((n1, (n2, (n3, s1))), s0))]


print doc_from_stack_effect(*f[0]),  doc_from_stack_effect(*sum_[0])
print '.......................'
##for result in unify(sum_[0][0], f[0][1]):
##    print result, '->', update(result, sum_[0][1])
for g in MC(f, sum_):
    print doc_from_stack_effect(*g)

print
print '.......................'
print

for g in MC(dup, mul):
    print doc_from_stack_effect(*g)



##stack_concat = lambda q, e: (q[0], stack_concat(q[1], e)) if isinstance(q, tuple) else e


class FunctionJoyType(AnyJoyType):
    def __init__(self, name, sec, number):
        self.name = name
        self.stack_effects = sec
        self.number = number
    def __add__(self, other):
        return self
    __radd__ = __add__


class SymbolJoyType(FunctionJoyType): prefix = 'F'
class CombinatorJoyType(FunctionJoyType): prefix = 'C'

from joy.library import dip, dipd

##def dip_t(stack, expression):
##    (quote, (a1, stack)) = stack
##    expression = stack_concat(quote, (a1, expression))
##    return stack, expression

CONS = SymbolJoyType('cons', cons, 23)
DIP = CombinatorJoyType('dip', [dip], 44)
DIPD = CombinatorJoyType('dipd', [dipd], 45)

from itertools import chain

flatten = lambda g: list(chain.from_iterable(g))

def kav(F, e):
    if not e:
        return [F]

    n, e = e

    if isinstance(n, SymbolJoyType):
        Fs = MC([F], n.stack_effects)
        res = flatten(kav(Fn, e) for Fn in Fs)

    elif isinstance(n, CombinatorJoyType):
        res = []
        for combinator in n.stack_effects:
            fi, fo = F
            new_fo, ee, _ = combinator(fo, e, {})
            new_F = fi, new_fo
            res.extend(kav(new_F, ee))
    else:
        lit = s9, (n, s9)
        res = flatten(kav(Fn, e) for Fn in MC([F], [lit]))

    return res


##l = [(s0, ((CONS, s2), (A[1], s0)))]
##
##e = (DIP, ())
##
##h = kav(l[0], e)
##
##for z in h:
##  print doc_from_stack_effect(*z)


ID = s0, s0
expression = (a1, ((CONS, s0), (DIP, ())))

for sec in kav(ID, expression):
  print doc_from_stack_effect(*sec)




expression = (a1, (a3, ((CONS, s0), (DIPD, ()))))

for sec in kav(ID, expression):
  print doc_from_stack_effect(*sec)













