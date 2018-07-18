# -*- coding: utf_8











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












# joy.library.add_aliases(FUNCTIONS, joy.library.ALIASES)


# def set_expectations_of_definition(cjt):
#     if len(cjt.stack_effects) != 1:
#         raise ValueError
#     defi = cjt.stack_effects[0]
#     if not isinstance(defi, joy.library.DefinitionWrapper):
#         raise ValueError
#     F = infer_expression(defi.body)
#     assert len(F) == 1, repr(F)
#     fi, fo = F[0]
#     cjt.expect = fi



    
    average = 
    loop.expect = s6, (b1, s5)
    disenstacken.expect = (As[1], s1), s0
    joy.library._dictionary['disenstacken'],
  

# scope = globals().copy()
# scope.update(FUNCTIONS)
# eval(set_expectations.func_code, scope)
# del scope


