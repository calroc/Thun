#!/usr/bin/env python
import unittest

from joy.utils.polytypes import *
from joy.utils.stack import list_to_stack as __


infr = lambda e: infer(__(e))


globals().update(SYMBOLS)
globals().update(COMBINATORS)


class TestKleeneStar(unittest.TestCase):
    
  def test_sum(self):
    expression = [
      (n1, (n2, (n3, s1))),  # Three numbers in a stack.
      sum,  # builtin shadowed by SymbolJoyType
      ]
    # A function that puts a single number on the stack.
    f = s0, (n0, s0)
    self.assertEqual(infr(expression), [f])

  def test_BS(self):
    expression = pop, swap, rolldown, rest, rest, cons, cons
    # ([a3 a4 ...0] a2 a1 a0 -- [a1 a2 ...0])
    f = (a0, (a1, (a2, ((a3, (a4, s0)), s1)))), ((a1, (a2, s0)), s1)
    self.assertEqual(infr(expression), [f])

  def test_enter(self):
    expression = a1, (cons, s0), dip  # a1 [cons] dip
    # (a0 [...0] -- [a0 ...0] a1)
    f = ((s0, (a0, s1)), (a1, ((a0, s0), s1)))
    self.assertEqual(infr(expression), [f])

  def test_2(self):
    # [cons] i == cons
    expression = (cons, s0), i
    self.assertEqual(infr(expression), infr([cons]))


##
##for g in MC(dup, mul):
##    print doc_from_stack_effect(*g)


##    print doc_from_stack_effect(*f)
##    print 
##    for sec in infer(e):
##      print sec, doc_from_stack_effect(*sec)
##        self.assertRaises(KeyError, lambda: {}[23])
##    def test_leave(self):
##    def setUp(self):
##    def tearDown(self):



##DIP = CombinatorJoyType('dip', [dip], 44)
##DIPD = CombinatorJoyType('dipd', [dipd], 45)

##l = [(s0, ((CONS, s2), (A[1], s0)))]
##
##e = (DIP, ())
##
##h = infer(e, l[0])
##
##for z in h:
##  print doc_from_stack_effect(*z)

##
##
##
##
##
##expression = (a1, (a3, ((CONS, s0), (DIPD, ()))))
##
##for sec in infer(expression):
##  print doc_from_stack_effect(*sec)
##

if __name__ == '__main__':
    unittest.main()
