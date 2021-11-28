'''
In order to debug the problem I'm having with definitions breaking a
zipper expression I need to load a system, load the defs into a dictionary,
and then run the expression and see if it raises an exception, substituting
definitions one-at-a-time until I find the one that breaks it.
'''

from joy.library import default_defs, initialize, inscribe, Def
from joy.joy import joy
from joy.parser import text_to_expression
from joy.utils.pretty_print import trace
from joy.utils.stack import stack_to_string


inscribe(trace)

dictionary = initialize()

defs = {}
default_defs(defs)


expression = text_to_expression(
    '[1 [2 [3 4 25 6] 7] 8]'
    '[dup mul]'
    '[dip dip infra dip infra dip infra]'
    '[[] ccons] step i'

    # to trace replace last line above with:
    # '[[[] ccons] step i]'
    # 'trace'
    )

expected_result = '[1 [2 [3 4 625 6] 7] 8]'
expected_result_as_stack = text_to_expression(expected_result)


def test_expr(ds):
    '''
    Run the test expression with the defs in ds.
    Return the resulting stack as a string or the
    exception raised if any.
    '''
    D = dictionary.copy()
    D.update(ds)
    try:
        stack, _, _ = joy((), expression, D)
    except Exception as err:
        return err
    return stack_to_string(stack)


# The problem is that it works with the built-ins:

print(test_expr({}))

# Results:
#   [1 [2 [3 4 625 6] 7] 8]
#
# But not with the definitions:

print(test_expr(defs))

# Results:
#   not enough values to unpack (expected 2, got 0)
#
# This obviously sucks and is bad. :(

# First, because it's easy, let's try adding single defs
# one-at-a-time to the dictionary and see if any one of
# them breaks it.

for def_name in defs:
    stack_str = test_expr({def_name: defs[def_name]})
    if stack_str != expected_result:
        print(def_name, 'failed!')
        print(stack_str)

# Results:
#   step failed!
#   _step0

# Ah yes, step's definition has parts (and dependencies).
step_defs = {
    d: defs[d]
    for d in defs
    if 'step' in d
    }
for name in ('?', 'dupdipd', 'popopop'):
    step_defs[name] = defs[name]
print(sorted(step_defs))
print(test_expr(step_defs))

# Results:
#   [1 [2 [3 4 625 6] 7] 8]
#
# So it's not step by itself, it's some combination of defintions
# that is causing the bug.
