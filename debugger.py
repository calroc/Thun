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
##    '[[] ccons] step i'

    # to trace replace last line above with:
    '[[[] ccons] step i]'
    'trace'
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

##print(test_expr({}))

# Results:
#   [1 [2 [3 4 625 6] 7] 8]
#
# But not with the definitions:

##print(test_expr(defs))

# Results:
#   not enough values to unpack (expected 2, got 0)
#
# This obviously sucks and is bad. :(

# First, because it's easy, let's try adding single defs
# one-at-a-time to the dictionary and see if any one of
# them breaks it.

# Only the defs that shadow the built-ins could be the problem:
candidates = set(dictionary) & set(defs)

##for def_name in candidates:
##    stack_str = test_expr({def_name: defs[def_name]})
##    if stack_str != expected_result:
##        print(def_name, 'failed!')
##        print(stack_str)

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
##print(sorted(step_defs))
##print(test_expr(step_defs))

# Results:
#   [1 [2 [3 4 625 6] 7] 8]
#
# So it's not step by itself, it's some combination of defintions
# that is causing the bug.

its_is_probably_not = set('''
    dipd roll< uncons
'''.split())

sus_defs = {
    def_name: defs[def_name]
    for def_name in defs
    if (def_name in candidates
    or def_name in step_defs)
    and def_name not in its_is_probably_not
    }
##print()
##print(test_expr(sus_defs))

d = step_defs.copy()
d['uncons'] = defs['uncons']
d['cleave'] = defs['cleave']
d['fork'] = defs['fork']

##print(test_expr(d))

CD = {
    name: defs[name]
    for name in candidates
    }
CD.update(step_defs)
CD['codi'] = defs['codi']
CD['swapd'] = defs['swapd']
CD['cleave'] = defs['cleave']
CD['fork'] = defs['fork']
CD['grba'] = defs['grba']
CD['infrst'] = defs['infrst']

##print(test_expr(CD))

##print(sorted(CD))
# [++, --, '?', _step0, _step1, _stept, abs, app1, app2, app3, at, b, ccons, clear, 'cleave', 'codi', dipd, disenstacken, drop, dupd, dupdd, dupdip, 'dupdipd', 'fork', fourth, genrec, 'grba', ii, infra, 'infrst', map, mod, neg, not, pm, popd, popdd, popop, popopd, popopdd, 'popopop', rest, reverse, roll<, roll>, rolldown, rollup, rrest, second, shunt, step, step_zero, sum, 'swapd', swons, take, third, times, tuck, uncons, unit, unswons, x]

del CD['++']
del CD['--']
##del CD['?']
##del CD['_step0']
##del CD['_step1']
##del CD['_stept']
del CD['abs']
del CD['app1']
del CD['app2']
del CD['app3']
del CD['at']
del CD['b']
del CD['ccons']
del CD['clear']
##del CD['cleave']  # <-- dep
del CD['codi']
del CD['dipd']
del CD['disenstacken']
del CD['drop']
del CD['dupd']
del CD['dupdd']
del CD['dupdip']
del CD['dupdipd']
##del CD['fork']  # <-- dep
del CD['fourth']
del CD['genrec']
##del CD['grba']  # <-- dep
del CD['ii']
del CD['infra']
##del CD['infrst']  # <-- dep
del CD['map']
del CD['mod']
del CD['neg']
del CD['not']
del CD['pm']
del CD['popd']
##del CD['popdd']  # <-- !!!!!
del CD['popop']
del CD['popopd']
del CD['popopdd']
del CD['popopop']
del CD['rest']
del CD['reverse']
del CD['roll<']
del CD['roll>']
del CD['rolldown']
del CD['rollup']
del CD['rrest']
del CD['second']
del CD['shunt']
##del CD['step']  # <-- !!!!!
del CD['step_zero']
del CD['sum']
del CD['swapd']
del CD['swons']
del CD['take']
del CD['third']
del CD['times']
del CD['tuck']
##del CD['uncons']  #  <-- popopop !?
del CD['unit']
del CD['unswons']
del CD['x']

print(test_expr(CD))
for n in sorted(CD):
	print(n)
##    ?
##    _step0
##    _step1
##    _stept
##    cleave
##    fork
##    grba
##    infrst
##    popdd
##    step
##    uncons





##print()
##print(set(dictionary) & set(defs))
