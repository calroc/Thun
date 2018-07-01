#!/usr/bin/env python
import unittest

from joy.utils.polytypes import *
from joy.utils.stack import list_to_stack as __


globals().update(FUNCTIONS)


class TestMixin(object):

  def assertEqualTypeStructure(self, a, b):
    # Check shape and types match.
    self.assert_(a >= b)
    self.assert_(b >= a)
    # Check type variables match expected pattern.
    self._compare_structures(b, a)

  def _compare_structures(self, a, b, seen=None):
    # Sometimes we change ONLY the "number" attr of our type vars.
    # We need to make sure the patterns still match our expected, uh,
    # patterns.
    if seen is None:
      seen = {}
    self.assertEqual(type(a), type(b))
    if isinstance(a, (tuple, list)):
      self.assertEqual(len(a), len(b))
      for aa, bb in zip(a, b):
        self._compare_structures(aa, bb, seen)
    else:
      if a in seen:
        self.assertEqual(b, seen[a])
      seen[a] = b


class TestCombinators(TestMixin, unittest.TestCase):

#  def setUp(self):
#  def tearDown(self):

  def test_branch(self):
    '''
    a1 [dup] [cons] branch
    '''
    expression = a1, (dup, s0), (cons, s0), branch
    f = [
      ((a0, s0),        (a0, (a0, s0))), #        (a0 -- a0 a0)
      ((s0, (a0, s1)), ((a0, s0), s1)),  # (a0 [...0] -- [a0 ...0])
      ]
    self.assertEqualTypeStructure(infer(*expression), f)

  def test_concat(self):
    expression = (swons, s3), (a4, s0), concat_
    f = (s1, ((swons, (a1, s1)), s1))  # (... -- ... [swons a1 ...])
    self.assertEqualTypeStructure(infer(*expression), [f])

  def test_dip(self):
    expression = dip,
    f = ((s1, (a1, s2)), (a2, s2))  # (a1 [...1] -- a2)
    self.assertEqualTypeStructure(infer(*expression), [f])

  def test_cons_dip(self):
    expression = (cons, s3), dip  # [cons] dip
    # (a2 [...1] a1 -- [a2 ...1] a1)
    f = (a1, (s1, (a2, s2))), (a1, ((a2, s1), s2))
    self.assertEqualTypeStructure(infer(*expression), [f])

  def test_cons_dipd(self):
    expression = a1, a3, (cons, s0), dipd
    f = ((s0, (a0, s1)), (a1, (a2, ((a0, s0), s1))))
    # (a0 [...0] -- [a0 ...0] a2 a1)
    self.assertEqualTypeStructure(infer(*expression), [f])

  def test_i(self):
    # [cons] i == cons
    expression = (cons, s0), i
    self.assertEqualTypeStructure(infer(*expression), infer(cons))

  def test_infra(self):
    expression = [
      __((n1, n2, n3), s1),  # Three numbers in a stack.
      (mul, s2),
      infra
      ]
    f = [
      (s1, ((f1, (n1, s2)), s3)),  # (-- [f1 n1 ...2])
      (s1, ((i1, (n1, s2)), s3)),  # (-- [i1 n1 ...2])
      ]
    self.assertEqualTypeStructure(infer(*expression), f)

  def test_nullary(self):
    expression = n1, n2, (mul, s2), (stack, s3), dip, infra, first
    f = [
      (s1, (f1, (f2, (f3, s1)))),  # (-- f3 f2 f1)
      (s1, (f1, (f2, (i1, s1)))),  # (-- i1 f2 f1)
      (s1, (f1, (i1, (f2, s1)))),  # (-- f2 i1 f1)
      (s1, (i1, (i2, (i3, s1)))),  # (-- i3 i2 i1)
      ]
    self.assertEqualTypeStructure(infer(*expression), f)

    expression = n1, n2, (mul, s2), nullary
    self.assertEqualTypeStructure(infer(*expression), f)

  def test_nullary_too(self):
    expression = (stack, s3), dip, infra, first
    f = ((s1, (a1, s2)), (a1, (a1, s2)))  # (a1 [...1] -- a1 a1)
    self.assertEqualTypeStructure(infer(*expression), [f])

    expression = nullary,
    f = ((s1, (a1, s2)), (a1, (a1, s2)))  # (a1 [...1] -- a1 a1)
    # Something's not quite right here...
    # e = infer(*expression)
    self.assertEqualTypeStructure(infer(*expression), [f])

  def test_x(self):
    expression = (a1, (swap, ((dup, s2), (dip, s1)))), x
    f = (s0, ((a0, (swap, ((dup, s1), (dip, s2)))), (a1, (a1, s0))))
    # (-- a1 a1 [a0 swap [dup ...1] dip ...2])
    self.assertEqualTypeStructure(infer(*expression), [f])


class TestKleeneStar(TestMixin, unittest.TestCase):

  def test_Astar(self):
    expression = a1, As[2], a2, cons
    f = [  # Vanish in one, spawn a new variable in the other...
      (s1, ((a1, s2), s1)),  # (-- [a1 ...2])
      (s1, ((a1, s2), (As[1], (a2, s1)))),  # (-- a2 a1* [a1 ...2])
      ]
    self.assertEqualTypeStructure(infer(*expression), f)

  def test_sum(self):
    expression = [
      __((n1, n2, n3), s1),  # Three numbers in a stack.
      sum,  # builtin shadowed by SymbolJoyType
      ]
    # A function that puts a single number on the stack.
    f = s0, (n0, s0)
    self.assertEqualTypeStructure(infer(*expression), [f])

  def test_no_infinite_loop(self):
    expression = [
      (Ns[2], s1),  # A stack of numbers.
      sum,  # builtin shadowed by SymbolJoyType.
      ]
    # A function that puts a single number on the stack.
    f = s0, (n0, s0)
    self.assertEqualTypeStructure(infer(*expression), [f])


class TestYin(TestMixin, unittest.TestCase):

  def test_MiscYin(self):
    expression = pop, swap, rolldown, rest, rest, cons, cons
    # ([a3 a4 ...0] a2 a1 a0 -- [a1 a2 ...0])
    f = (a0, (a1, (a2, ((a3, (a4, s0)), s1)))), ((a1, (a2, s0)), s1)
    self.assertEqualTypeStructure(infer(*expression), [f])

  def test_swaack(self):
    expression = a0, (a1, s0), swaack
    f = (s0, ((a0, s0), (a1, s1)))  # (-- a1 [a0 ...0])
    self.assertEqualTypeStructure(infer(*expression), [f])
  
  def test_z_down(self):
    expression = s2, swap, uncons, swap
    f = (((a1, s1), s2), (a1, (s1, (s3, s2))))
    # ([a1 ...1] -- [...3] [...1] a1)
    self.assertEqualTypeStructure(infer(*expression), [f])

  def test_z_right(self):
    expression = a1, a2, (swons, s3), cons, dip, uncons, swap
    f = ((s1, s2), (a1, (s3, ((a2, s1), s2))))
    # ([...1] -- [a2 ...1] [...3] a1)
    self.assertEqualTypeStructure(infer(*expression), [f])

  def test_stack_dup_ccons(self):
    expression = stack, dup, ccons
    f = ((a1, s1), ((a1, ((a1, s1), (a1, s1))), s1))
    # (... a1 -- ... [a1 [a1 ...] a1 ...])
    self.assertEqualTypeStructure(infer(*expression), [f])

##  def test_(self):
##    expression = pop, swap, rolldown, rest, rest, cons, cons
##    f = 
##    for sec in infer(*expression):
##      print sec, doc_from_stack_effect(*sec)
##    self.assertEqualTypeStructure(infer(*expression), [f])

##      for g in MC(dup, mul):
##        print doc_from_stack_effect(*g)


if __name__ == '__main__':
    unittest.main() #defaultTest='TestCombinators.test_cons_dip')
