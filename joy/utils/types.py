# -*- coding: utf_8
#
#    Copyright © 2018 Simon Forman
#
#    This file is part of Thun
#
#    Thun is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    Thun is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Thun.  If not see <http://www.gnu.org/licenses/>. 
#
from __future__ import print_function
from logging import getLogger, addLevelName
from functools import reduce

_log = getLogger(__name__)
addLevelName(15, 'hmm')

from collections import Counter
from itertools import imap, chain, product
from inspect import stack as inspect_stack
from joy.utils.stack import (
  concat,
  expression_to_string,
  list_to_stack,
  stack_to_string,
  )
from joy.parser import Symbol, text_to_expression


class AnyJoyType(object):
  '''
  Joy type variable.  Represents any Joy value.
  '''

  accept = tuple, int, float, long, complex, str, unicode, bool, Symbol
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
  accept = bool, int, float, long, complex
  prefix = 'n'


class FloatJoyType(NumberJoyType):
  accept = float
  prefix = 'f'


class IntJoyType(FloatJoyType):
  accept = int
  prefix = 'i'


class TextJoyType(AnyJoyType):
  accept = basestring
  prefix = 't'


class StackJoyType(AnyJoyType):

  accept = tuple
  prefix = 's'

  def __nonzero__(self):
    # Imitate () at the end of cons list.
    return False


class KleeneStar(object):
    u'''
    A sequence of zero or more `AnyJoyType` variables would be:

       A*

    The `A*` works by splitting the universe into two alternate histories:

       A* → ∅

       A* → A A*

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
class FloatStarJoyType(KleeneStar): kind = FloatJoyType
class IntStarJoyType(KleeneStar): kind = IntJoyType
class StackStarJoyType(KleeneStar): kind = StackJoyType
class TextStarJoyType(KleeneStar): kind = TextJoyType


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
        new_f = list(poly_compose(f, g, ()))
        assert len(new_f) == 1, repr(new_f)
        return new_f[0][1]


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
  if isinstance(right, Symbol):
    return right
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
    except (TypeError,  # FunctionJoyTypes break this.
        AttributeError):  # Symbol
      seen[f] = f
    else:
      c[f.prefix] += 1
    return seen[f]

  return tuple(delabel(inner, seen, c) for inner in f)


def uni_unify(u, v, s=None):
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
    s = uni_unify(b, d, uni_unify(a, c, s))

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


def _log_uni(U):
  def inner(u, v, s=None):
    _log.debug(
      '%3i %s U %s   w/ %s',
      len(inspect_stack()), u, v, s,
      )
    res = U(u, v, s)
    _log.debug(
      '%3i %s U %s   w/ %s => %s',
      len(inspect_stack()), u, v, s, res,
      )
    return res
  return inner


@_log_uni
def unify(u, v, s=None):
    '''
    Return a tuple of substitution dicts representing unifiers for u and v.
    '''
    if s is None:
        s = {}
    elif s:
        u = reify(s, u)
        v = reify(s, v)

    if u == v:
        res = s,

    elif isinstance(u, tuple) and isinstance(v, tuple):
        if len(u) != 2 or len(v) != 2:
            if _that_one_special_case(u, v):
                return s,
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


def _that_one_special_case(u, v):
    '''
    Handle e.g. ((), (n1*, s1)) when type-checking sum, product, etc...
    '''
    return (
        u == ()
        and len(v) == 2
        and isinstance(v[0], KleeneStar)
        and isinstance(v[1], StackJoyType)
        )


def flatten(g):
  return list(chain.from_iterable(g))


def _lil_uni(u, v, s):
    if u >= v:
        s[u] = v
        return s,
    if v >= u:
        s[v] = u
        return s,
    raise JoyTypeError('Cannot unify %r and %r.' % (u, v))


def _stacky(thing):
  return thing.__class__ in {AnyJoyType, StackJoyType}


def _compose(f, g):
  '''
  Return the stack effect of the composition of two stack effects.
  '''
  # Relabel, unify, update, delabel.
  (f_in, f_out), (g_in, g_out) = relabel(f, g)
  fg = reify(uni_unify(g_in, f_out), (f_in, g_out))
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


def doc_from_stack_effect(inputs, outputs=('??', ())):
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


def _poly_compose(f, g, e):
    (f_in, f_out), (g_in, g_out) = f, g
    for s in unify(g_in, f_out):
        yield reify(s, (e, (f_in, g_out)))


def poly_compose(f, g, e):
    '''
    Yield the stack effects of the composition of two stack effects.  An
    expression is carried along and updated and yielded.
    '''
    f, g = relabel(f, g)
    for fg in _poly_compose(f, g, e):
        yield delabel(fg)


def _meta_compose(F, G, e):
    for f, g in product(F, G):
        try:
            for result in poly_compose(f, g, e): yield result
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


_S0 = StackJoyType(0)
ID = _S0, _S0  # Identity function.


def _infer(e, F=ID):
    if __debug__:
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
        if n in FUNCTIONS:
          res =_infer((FUNCTIONS[n], e), F)
        else:
          raise JoyTypeError(n)
        #   print n
        #   func = joy.library._dictionary[n]
        #   res = _interpret(func, F[0], F[1], e)

    else:
        fi, fo = F
        res = _infer(e, (fi, (n, fo)))

    return res


def _interpret(f, fi, fo, e):
  new_fo, ee, _ = f(fo, e, {})
  ee = reify(FUNCTIONS, ee)  # Fix Symbols.
  new_F = fi, new_fo
  return _infer(ee, new_F)


def _log_it(e, F):
    _log.log(
      15,
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


def infer_string(string):
    e = reify(FUNCTIONS, text_to_expression(string))  # Fix Symbols.
    return sorted(set(_infer(e)))


def infer_expression(expression):
    e = reify(FUNCTIONS, expression)  # Fix Symbols.
    return sorted(set(_infer(e)))


def type_check(name, stack):
  '''
  Trinary predicate.  True if named function type-checks, False if it
  fails, None if it's indeterminate (because I haven't entered it into
  the FUNCTIONS dict yet.)
  '''
  try:
    func = FUNCTIONS[name]
  except KeyError:
    return # None, indicating unknown
  if isinstance(func, SymbolJoyType):
    secs = func.stack_effects
  elif isinstance(func, CombinatorJoyType):
    if func.expect is None:
      return # None, indicating unknown
    secs = [(func.expect, ())]
  else:
    raise TypeError(repr(func))  # wtf?
  for fi, fo in secs:
    try:
      unify(fi, stack)
    except (JoyTypeError, ValueError):
      continue
    except:
      _log.exception(
        'Type-checking %s %s against %s',
        name,
        doc_from_stack_effect(fi, fo), 
        stack_to_string(stack),
        )
      continue
    return True
  return False


FUNCTIONS = {}  # Polytypes (lists of stack effects.)
_functions = {}  # plain ol' stack effects.


def __(*seq):
  stack = StackJoyType(23)
  for item in seq: stack = item, stack
  return stack


def stack_effect(*inputs):
  def _stack_effect(*outputs):
    def _apply_to(function):
      i, o = _functions[function.name] = __(*inputs), __(*outputs)
      d = doc_from_stack_effect(i, o)
      function.__doc__ += (
        '\nStack effect::\n\n    '  # '::' for Sphinx docs.
        + d
        )
      _log.info('Setting stack effect for %s := %s', function.name, d)
      return function
    return _apply_to
  return _stack_effect


def ef(*inputs):
  def _ef(*outputs):
    return __(*inputs), __(*outputs)
  return _ef


def combinator_effect(number, *expect):
  def _combinator_effect(c):
    e = __(*expect) if expect else None
    FUNCTIONS[c.name] = CombinatorJoyType(c.name, [c], number, e)
    if e:
      sec = doc_from_stack_effect(e)
      _log.info('Setting stack EXPECT for combinator %s := %s', c.name, sec)
    return c
  return _combinator_effect


def show(DEFS):
  for name, stack_effect_comment in sorted(DEFS.iteritems()):
    t = ' *'[compilable(stack_effect_comment)]
    print(name, '=', doc_from_stack_effect(*stack_effect_comment), t)


def generate_library_code(DEFS, f=None):
  if f is None:
    import sys
    f = sys.stdout
  print('# GENERATED FILE. DO NOT EDIT.\n', file=f)
  for name, stack_effect_comment in sorted(DEFS.iteritems()):
    if not compilable(stack_effect_comment):
      continue
    print(file=f)
    print(compile_(name, stack_effect_comment), file=f)
    print(file=f)


def poly_combinator_effect(number, effect_funcs, *expect):
  def _poly_combinator_effect(c):
    e = __(*expect) if expect else None
    FUNCTIONS[c.name] = CombinatorJoyType(c.name, effect_funcs, number, e)
    if e:
      _log.info('Setting stack EXPECT for combinator %s := %s', c.name, e)
    return c
  return _poly_combinator_effect

#FUNCTIONS['branch'].expect = s7, (s6, (b1, s5))

