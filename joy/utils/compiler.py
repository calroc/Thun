from joy.parser import text_to_expression, Symbol
from joy.utils.stack import concat, iter_stack, list_to_stack
from joy.library import SimpleFunctionWrapper, YIN_STACK_EFFECTS


def import_yin():
    from joy.utils.generated_library import *
    return locals()


def _names():
    n = 0
    while True:
        yield Symbol('a' + str(n))
        n += 1


class InfiniteStack(tuple):

    names = _names().next

    def __init__(self, code):
        self.code = code

    def __iter__(self):
        if not self:
            new_var = self.names()
            self.code.append(('pop', new_var))
            return iter((new_var, self))


class Foo(object):

    def __init__(self, name):
        self.name = name

    def __call__(self, stack, expression, code):
        in1, (in0, stack) = stack
        out = InfiniteStack.names()
        code.append(('call', out, self.name, (in0, in1)))
        return (out, stack), expression, code


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

    s = list(iter_stack(stack))
    if s: code.append(tuple(['ret'] + s))
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
    index = (i for i, t in enumerate(code) if t[0] != 'pop').next()
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


def first_two(stack, expression, code):
    in_, out = YIN_STACK_EFFECTS['first_two']
    stack, map_ = remap_inputs(in_, stack, code)
    out = type_vars_to_labels(out, map_)
    return concat(out, stack), expression, code


YIN = import_yin()


D = {
    name: SimpleFunctionWrapper(func)
    for name, func in YIN.iteritems()
    }


D['mul'] = Foo('mul')
D['sub'] = Foo('sub')
D['first_two'] = first_two

print compile_yinyang('mul_', 'mul')
print compile_yinyang('sqr', 'dup mul')
print compile_yinyang('foo', 'dup 23 sub mul')
print compile_yinyang('bar', 'mul mul mul mul')
print compile_yinyang('baz', 'mul dup sub dup')
print compile_yinyang('to_the_fifth_power', 'dup dup mul dup mul mul')
print compile_yinyang('hey', 'dup dup dup')
print compile_yinyang('hey', 'dup first_two mul')

