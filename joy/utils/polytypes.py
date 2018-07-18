# -*- coding: utf_8
'''

Multiple Stack Effects

By adjusting the machinery in types.py to handles lists of stack effect comments
we can capture more information about the type signatures of some functions,
and we can introduce a kind of Kleene Star or sequence type that can stand for
an unbounded sequence of other types.

'''
import sys
import joy.library
from joy.parser import Symbol, text_to_expression
from joy.utils.stack import (
    concat as CONCAT,
    expression_to_string,
    list_to_stack,
    )

    average = 
    # sum_ = 
    # product = 
    # min_ = max_ = [(((Ns[1], s1), s0), (n0, s0))]
 #   flatten = [(((Ss[1], s1), s0), (s2, s0))]

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
        joy.library.concat_,
        joy.library._dictionary['disenstacken'],
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


joy.library.add_aliases(FUNCTIONS, joy.library.ALIASES)


def set_expectations_of_definition(cjt):
    if len(cjt.stack_effects) != 1:
        raise ValueError
    defi = cjt.stack_effects[0]
    if not isinstance(defi, joy.library.DefinitionWrapper):
        raise ValueError
    F = infer_expression(defi.body)
    assert len(F) == 1, repr(F)
    fi, fo = F[0]
    cjt.expect = fi



def set_expectations():
    
    loop.expect = s6, (b1, s5)
#    i.expect = nullary.expect = x.expect = s7, s6
#    dip.expect = dupdip.expect = s8, (a8, s7)
#    dipd.expect = s8, (a8, (a7, s7))
#    dipdd.expect = s8, (a8, (a7, (a6, s7)))
    concat_.expect = s8, (s7, s6)
#    b.expect = infra.expect = s8, (s7, s6)
    # set_expectations_of_definition(unary)
    # set_expectations_of_definition(binary)
    # set_expectations_of_definition(ternary)
    # set_expectations_of_definition(quoted)
    # set_expectations_of_definition(unquoted)
    # set_expectations_of_definition(enstacken)
    disenstacken.expect = (As[1], s1), s0
scope = globals().copy()
scope.update(FUNCTIONS)
eval(set_expectations.func_code, scope)
del scope


