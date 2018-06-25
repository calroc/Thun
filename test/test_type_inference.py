#!/usr/bin/env python
import unittest

from joy.utils.polytypes import *
from joy.utils.stack import list_to_stack as __


infr = lambda e: infer(__(e))


globals().update(FUNCTIONS)


class TestKleeneStar(unittest.TestCase):
    
#  def setUp(self):
#  def tearDown(self):

  def test_infra(self):
    expression = [
      __((n1, n2, n3), s1),  # Three numbers in a stack.
      (mul, s2),
      infra
      ]
    f = (s0, ((n0, (n1, s1)), s2))
    # (-- [n0 n1 ...1]) Two numbers in a stack.
    self.assertEqual(infr(expression), [f])

  def test_sum(self):
    expression = [
      __((n1, n2, n3), s1),  # Three numbers in a stack.
      sum,  # builtin shadowed by SymbolJoyType
      ]
    # A function that puts a single number on the stack.
    f = s0, (n0, s0)
    self.assertEqual(infr(expression), [f])

  def test_Yin(self):
    expression = pop, swap, rolldown, rest, rest, cons, cons
    # ([a3 a4 ...0] a2 a1 a0 -- [a1 a2 ...0])
    f = (a0, (a1, (a2, ((a3, (a4, s0)), s1)))), ((a1, (a2, s0)), s1)
    self.assertEqual(infr(expression), [f])

  def test_cons_dip(self):
    expression = a1, (cons, s0), dip  # a1 [cons] dip
    # (a0 [...0] -- [a0 ...0] a1)
    f = ((s0, (a0, s1)), (a1, ((a0, s0), s1)))
    self.assertEqual(infr(expression), [f])

  def test_cons_dipd(self):
    expression = a1, a3, (cons, s0), dipd
    f = ((s0, (a0, s1)), (a1, (a2, ((a0, s0), s1))))
    # (a0 [...0] -- [a0 ...0] a2 a1)
    self.assertEqual(infr(expression), [f])

  def test_i(self):
    # [cons] i == cons
    expression = (cons, s0), i
    self.assertEqual(infr(expression), infr([cons]))

  def test_branch(self):
    '''
    a1 [dup] [cons] branch
    '''
    expression = a1, (dup, s0), (cons, s0), branch
    f = [
      ((s0, (a0, s1)), ((a0, s0), s1)),  # (a0 [...0] -- [a0 ...0])
      ((a0, s0),        (a0, (a0, s0))), #        (a0 -- a0 a0)
      ]
    self.assertEqual(infr(expression), f)

  def test_swaack(self):
    expression = a0, (a1, s0), swaack
    f = (s0, ((a0, s0), (a1, s1)))  # (-- a1 [a0 ...0])
    self.assertEqual(infr(expression), [f])

  def test_x(self):
    expression = (a1, (swap, ((dup, s2), (dip, s0)))), x
    f = (s0, ((a0, (swap, ((dup, s1), (dip, s2)))), (a1, (a1, s0))))
    # (-- a1 a1 [a0 swap [dup ...1] dip ...2])
    self.assertEqual(infr(expression), [f])

##    for sec in infr(expression):
##      print sec, doc_from_stack_effect(*sec)


##for g in MC(dup, mul):
##    print doc_from_stack_effect(*g)


if __name__ == '__main__':
    unittest.main()
