from joy.parser import text_to_expression, Symbol
from joy.utils.stack import iter_stack, list_to_stack
from joy.library import SimpleFunctionWrapper


def import_yin():
    from joy.utils.generated_library import *
    return locals()

D = {
    name: SimpleFunctionWrapper(func)
    for name, func in import_yin().iteritems()
    }


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
    coalesce_pops(code)
    lines = []
    emit = lines.append
    for t in code:
        tag, rest = t[0], t[1:]
        if tag == 'pop': emit(strstk(rest) + ' = stack')
        elif tag == 'call': emit('%s = %s%s' % rest)
        elif tag == 'ret': emit('return ' + strstk(rest))
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


D['mul'] = Foo('mul')
D['sub'] = Foo('sub')

##print compile_yinyang('mul_', 'mul')
##print compile_yinyang('sqr', 'dup mul')
##print compile_yinyang('foo', 'dup 23 sub mul')
##print compile_yinyang('bar', 'mul mul mul mul')
##print compile_yinyang('baz', 'mul dup sub dup')
##print compile_yinyang('to_the_fifth_power', 'dup dup mul dup mul mul')
##print compile_yinyang('hey', 'dup dup dup')
print compile_yinyang('hey', 'first_two')
