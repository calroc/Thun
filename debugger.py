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
##    '[[[] ccons] step i]'
##    'trace'
    )

step_d = {d:defs[d] for d in defs if 'step' in d}
for name in ('?', 'dupdipd', 'popopop'):
    step_d[name] = defs[name]

def test_expr(ds):
    D = dictionary.copy()
    D.update(ds)
    try:
        stack, _, _ = joy((), expression, D)
    except Exception as err:
        return err
    return stack_to_string(stack)

res = test_expr(step_d)
if res:
    print(res)

##for def_name in defs:
##    D = dictionary.copy()
##    D[def_name] = defs[def_name]
##    try:
##        stack, _, d = joy((), expression, D)
##    except:
##        print(def_name, 'failed!')
##    else:
##        print(stack_to_string(stack), def_name, 'pass')
##
