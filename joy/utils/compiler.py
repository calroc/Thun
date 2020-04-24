'''
A crude compiler for a subset of Joy functions.

I think I'm going about this the wrong way.

The inference algorithm can "collapse" Yin function sequences into
single stack effects which can then be written out as Python functions.
Why not keep track of the new variables introduced as results of Yang
functions, during inference?  Could I write out better code that way?

In any event, I am proceeding with this sort of ad hoc way for now.
'''
from __future__ import print_function
from builtins import next
from builtins import str
from builtins import object
from joy.parser import text_to_expression, Symbol
from joy.utils.stack import concat, iter_stack, list_to_stack
from joy.library import SimpleFunctionWrapper, YIN_STACK_EFFECTS
from functools import reduce


def import_yin():
    from joy.utils.generated_library import *
    return locals()


class InfiniteStack(tuple):

    def _names():
        n = 0
        while True:
            m = yield Symbol('a' + str(n))
            n = n + 1 if m is None else m

    _NAMES = _names()
    next(_NAMES)

    names = _NAMES.__next__
    reset = lambda _, _n=_NAMES: _n.send(-1)

    def __init__(self, code):
        self.reset()
        self.code = code

    def __iter__(self):
        if not self:
            new_var = self.names()
            self.code.append(('pop', new_var))
            return iter((new_var, self))


def I(expression):
    code = []
    stack = InfiniteStack(code)

    while expression:
        term, expression = expression
        if isinstance(term, Symbol):
            func = D[term]
            stack, expression, _ = func(stack, expression, code)
        else:
            stack = term, stack

    code.append(tuple(['ret'] + list(iter_stack(stack))))
    return code


strtup = lambda a, b: '(%s, %s)' % (b, a)
strstk = lambda rest: reduce(strtup, rest, 'stack')


def code_gen(code):
    #for p in code: print p
    coalesce_pops(code)
    lines = []
    emit = lines.append
    for t in code:
        tag, rest = t[0], t[1:]
        if tag == 'pop': emit(strstk(rest) + ' = stack')
        elif tag == 'call': emit('%s = %s%s' % rest)
        elif tag == 'ret': emit('return ' + strstk(rest[::-1]))
        else:
            raise ValueError(tag)
    return '\n'.join('    ' + line for line in lines)


def coalesce_pops(code):
    code.sort(key=lambda p: p[0] != 'pop')  # All pops to the front.
    try: index = next((i for i, t in enumerate(code) if t[0] != 'pop'))
    except StopIteration: return
    code[:index] = [tuple(['pop'] + [t for _, t in code[:index][::-1]])]


def compile_yinyang(name, text):
    return '''
def %s(stack):
%s
''' % (name, code_gen(I(text_to_expression(text))))


def q():
    memo = {}
    def bar(type_var):
        try:
            res = memo[type_var]
        except KeyError:
            res = memo[type_var] = InfiniteStack.names()
        return res
    return bar


def type_vars_to_labels(thing, map_):
    if not thing:
        return thing
    if not isinstance(thing, tuple):
        return map_(thing)
    return tuple(type_vars_to_labels(inner, map_) for inner in thing)


def remap_inputs(in_, stack, code):
    map_ = q()
    while in_:
        term, in_ = in_
        arg0, stack = stack
        term = type_vars_to_labels(term, map_)
        code.append(('call', term, '', arg0))
    return stack, map_


class BinaryBuiltin(object):

    def __init__(self, name):
        self.name = name

    def __call__(self, stack, expression, code):
        in1, (in0, stack) = stack
        out = InfiniteStack.names()
        code.append(('call', out, self.name, (in0, in1)))
        return (out, stack), expression, code


YIN = import_yin()


D = {
    name: SimpleFunctionWrapper(YIN[name])
    for name in '''
        ccons
        cons
        dup
        dupd
        dupdd
        over
        pop
        popd
        popdd
        popop
        popopd
        popopdd
        rolldown
        rollup
        swap
        swons
        tuck
        unit
        '''.split()
    }


for name in '''
    first
    first_two
    fourth
    rest
    rrest
    second
    third
    uncons
    unswons
    '''.split():

    def foo(stack, expression, code, name=name):
        in_, out = YIN_STACK_EFFECTS[name]
        stack, map_ = remap_inputs(in_, stack, code)
        out = type_vars_to_labels(out, map_)
        return concat(out, stack), expression, code

    foo.__name__ = name
    D[name] = foo


for name in '''
  eq
  ge
  gt
  le
  lt
  ne
  xor
  lshift
  rshift
  and_
  or_
  add
  floordiv
  mod
  mul
  pow
  sub
  truediv
  '''.split():
    D[name.rstrip('-')] = BinaryBuiltin(name)


'''
        stack
        stuncons
        stununcons
        swaack
'''

for name in sorted(D):
    print(name, end=' ')
##    print compile_yinyang(name, name)
print('-' * 100)


print(compile_yinyang('mul_', 'mul'))
print(compile_yinyang('pop', 'pop'))
print(compile_yinyang('ppm', 'popop mul'))
print(compile_yinyang('sqr', 'dup mul'))
print(compile_yinyang('foo', 'dup 23 sub mul'))
print(compile_yinyang('four_mul', 'mul mul mul mul'))
print(compile_yinyang('baz', 'mul dup sub dup'))
print(compile_yinyang('to_the_fifth_power', 'dup dup mul dup mul mul'))
print(compile_yinyang('dup3', 'dup dup dup'))
print(compile_yinyang('df2m', 'dup first_two mul'))
print(compile_yinyang('sqr_first', 'uncons swap dup mul swons'))
print(compile_yinyang('0BAD',      'uncons      dup mul'))
print(compile_yinyang('uncons', 'uncons'))
