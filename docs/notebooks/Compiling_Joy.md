```python
from notebook_preamble import D, J, V, define
```

# Compiling Joy

Given a Joy program like:

    sqr == dup mul


```python
V('23 sqr')
```

          • 23 sqr
       23 • sqr
       23 • dup mul
    23 23 • mul
      529 • 


How would we go about compiling this code (to Python for now)?

## Naive Call Chaining
The simplest thing would be to compose the functions from the library:


```python
dup, mul = D['dup'], D['mul']
```


```python
def sqr(stack, expression, dictionary):
    return mul(*dup(stack, expression, dictionary))
```


```python
old_sqr = D['sqr']
D['sqr'] = sqr
```


```python
V('23 sqr')
```

        • 23 sqr
     23 • sqr
    529 • 


It's simple to write a function to emit this kind of crude "compiled" code.


```python
def compile_joy(name, expression):
    term, expression = expression
    code = term +'(stack, expression, dictionary)'
    format_ = '%s(*%s)'
    while expression:
        term, expression = expression
        code = format_ % (term, code)
    return '''\
def %s(stack, expression, dictionary):
    return %s
''' % (name, code)


def compile_joy_definition(defi):
    return compile_joy(defi.name, defi.body)

```


```python
print(compile_joy_definition(old_sqr))
```

    def sqr(stack, expression, dictionary):
        return mul(*dup(stack, expression, dictionary))
    


But what about literals?

    quoted == [unit] dip


```python
unit, dip = D['unit'], D['dip']
```


```python
# print compile_joy_definition(D['quoted'])
# raises
# TypeError: can only concatenate tuple (not "str") to tuple
```

For a program like `foo == bar baz 23 99 baq lerp barp` we would want something like:


```python
def foo(stack, expression, dictionary):
    stack, expression, dictionary = baz(*bar(stack, expression, dictionary))
    return barp(*lerp(*baq((99, (23, stack)), expression, dictionary)))
```

You have to have a little discontinuity when going from a symbol to a literal, because you have to pick out the stack from the arguments to push the literal(s) onto it before you continue chaining function calls.

## Compiling Yin Functions
Call-chaining results in code that does too much work.  For functions that operate on stacks and only rearrange values, what I like to call "Yin Functions", we can do better.

We can infer the stack effects of these functions (or "expressions" or "programs") automatically, and the stack effects completely define the semantics of the functions, so we can directly write out a two-line Python function for them.  This is already implemented in the `joy.utils.types.compile_()` function.


```python
from joy.utils.types import compile_, doc_from_stack_effect, infer_string
from joy.library import SimpleFunctionWrapper
```


    ---------------------------------------------------------------------------

    ModuleNotFoundError                       Traceback (most recent call last)

    <ipython-input-14-d5ef3c7560be> in <module>
    ----> 1 from joy.utils.types import compile_, doc_from_stack_effect, infer_string
          2 from joy.library import SimpleFunctionWrapper


    ModuleNotFoundError: No module named 'joy.utils.types'



```python
stack_effects = infer_string('tuck over dup')
```

Yin functions have only a single stack effect, they do not branch or loop.


```python
for fi, fo in stack_effects:
    print doc_from_stack_effect(fi, fo)
```


```python
source = compile_('foo', stack_effects[0])
```

All Yin functions can be described in Python as a tuple-unpacking (or "-destructuring") of the stack datastructure followed by building up the new stack structure.


```python
print source
```


```python
exec compile(source, '__main__', 'single')

D['foo'] = SimpleFunctionWrapper(foo)
```


      File "<ipython-input-9-1a7e90bf2d7b>", line 1
        exec compile(source, '__main__', 'single')
             ^
    SyntaxError: invalid syntax




```python
V('23 18 foo')
```

## Compiling from Stack Effects

There are times when you're deriving a Joy program when you have a stack effect for a Yin function and you need to define it.  For example, in the Ordered Binary Trees notebook there is a point where we must derive a function `Ee`:

       [key old_value left right] new_value key [Tree-add] Ee
    ------------------------------------------------------------
       [key new_value left right]

While it is not hard to come up with this function manually, there is no necessity.  This function can be defined (in Python) directly from its stack effect:

       [a b c d] e a [f] Ee
    --------------------------
       [a e c d]

(I haven't yet implemented a simple interface for this yet.  What follow is an exploration of how to do it.)


```python
from joy.parser import text_to_expression
```


```python
Ein = '[a b c d] e a [f]'  # The terms should be reversed here but I don't realize that until later.
Eout = '[a e c d]'
E = '[%s] [%s]' % (Ein, Eout)

print E
```


```python
(fi, (fo, _)) = text_to_expression(E)
```


```python
fi, fo
```


```python
Ein = '[a1 a2 a3 a4] a5 a6 a7'
Eout = '[a1 a5 a3 a4]'
E = '[%s] [%s]' % (Ein, Eout)

print E
```


```python
(fi, (fo, _)) = text_to_expression(E)
```


```python
fi, fo
```


```python
def type_vars():
    from joy.library import a1, a2, a3, a4, a5, a6, a7, s0, s1
    return locals()

tv = type_vars()
tv
```


```python
from joy.utils.types import reify
```


```python
stack_effect = reify(tv, (fi, fo))
print doc_from_stack_effect(*stack_effect)
```


```python
print stack_effect
```

Almost, but what we really want is something like this:


```python
stack_effect = eval('(((a1, (a2, (a3, (a4, s1)))), (a5, (a6, (a7, s0)))), ((a1, (a5, (a3, (a4, s1)))), s0))', tv)
```

Note the change of `()` to `JoyStackType` type variables.


```python
print doc_from_stack_effect(*stack_effect)
```

Now we can omit `a3` and `a4` if we like:


```python
stack_effect = eval('(((a1, (a2, s1)), (a5, (a6, (a7, s0)))), ((a1, (a5, s1)), s0))', tv)
```

The `right` and `left` parts of the ordered binary tree node are subsumed in the tail of the node's stack/list.


```python
print doc_from_stack_effect(*stack_effect)
```


```python
source = compile_('Ee', stack_effect)
print source
```

Oops!  The input stack is backwards...


```python
stack_effect = eval('((a7, (a6, (a5, ((a1, (a2, s1)), s0)))), ((a1, (a5, s1)), s0))', tv)
```


```python
print doc_from_stack_effect(*stack_effect)
```


```python
source = compile_('Ee', stack_effect)
print source
```

Compare:

       [key old_value left right] new_value key [Tree-add] Ee
    ------------------------------------------------------------
       [key new_value left right]



```python
eval(compile(source, '__main__', 'single'))
D['Ee'] = SimpleFunctionWrapper(Ee)
```


```python
V('[a b c d] 1 2 [f] Ee')
```


```python

```

## Working with Yang Functions

Consider the compiled code of `dup`:


```python

def dup(stack):
    (a1, s23) = stack
    return (a1, (a1, s23))


```

To compile `sqr == dup mul` we can compute the stack effect:


```python
stack_effects = infer_string('dup mul')
for fi, fo in stack_effects:
    print doc_from_stack_effect(fi, fo)
```

Then we would want something like this:


```python

def sqr(stack):
    (n1, s23) = stack
    n2 = mul(n1, n1)
    return (n2, s23)


```


```python

```


```python

```

How about...


```python
stack_effects = infer_string('mul mul sub')
for fi, fo in stack_effects:
    print doc_from_stack_effect(fi, fo)
```


```python

def foo(stack):
    (n1, (n2, (n3, (n4, s23)))) = stack
    n5 = mul(n1, n2)
    n6 = mul(n5, n3)
    n7 = sub(n6, n4)
    return (n7, s23)


# or

def foo(stack):
    (n1, (n2, (n3, (n4, s23)))) = stack
    n5 = sub(mul(mul(n1, n2), n3), n4)
    return (n5, s23)


```


```python

```


```python
stack_effects = infer_string('tuck')
for fi, fo in stack_effects:
    print doc_from_stack_effect(fi, fo)
```


```python

```

## Compiling Yin~Yang Functions

First, we need a source of Python identifiers.  I'm going to reuse `Symbol` class for this.


```python
from joy.parser import Symbol
```


```python
def _names():
    n = 0
    while True:
        yield Symbol('a' + str(n))
        n += 1

names = _names().next
```

Now we need an object that represents a Yang function that accepts two args and return one result (we'll implement other kinds a little later.)


```python
class Foo(object):

    def __init__(self, name):
        self.name = name

    def __call__(self, stack, expression, code):
        in1, (in0, stack) = stack
        out = names()
        code.append(('call', out, self.name, (in0, in1)))
        return (out, stack), expression, code
```

A crude "interpreter" that translates expressions of args and Yin and Yang functions into a kind of simple dataflow graph.


```python
def I(stack, expression, code):
    while expression:
        term, expression = expression
        if callable(term):
            stack, expression, _ = term(stack, expression, code)
        else:
            stack = term, stack
            code.append(('pop', term))

    s = []
    while stack:
        term, stack = stack
        s.insert(0, term)
    if s:
        code.append(('push',) + tuple(s))
    return code
```

Something to convert the graph into Python code.


```python
strtup = lambda a, b: '(%s, %s)' % (b, a)
strstk = lambda rest: reduce(strtup, rest, 'stack')


def code_gen(code):
    coalesce_pops(code)
    lines = []
    for t in code:
        tag, rest = t[0], t[1:]

        if tag == 'pop':
            lines.append(strstk(rest) + ' = stack')

        elif tag == 'push':
            lines.append('stack = ' + strstk(rest))

        elif tag == 'call':
            #out, name, in_ = rest
            lines.append('%s = %s%s' % rest)

        else:
            raise ValueError(tag)

    return '\n'.join('    ' + line for line in lines)


def coalesce_pops(code):
    index = [i for i, t in enumerate(code) if t[0] == 'pop']
    for start, end in yield_groups(index):
        code[start:end] = \
            [tuple(['pop'] + [t for _, t in code[start:end][::-1]])]


def yield_groups(index):
    '''
    Yield slice indices for each group of contiguous ints in the
    index list.
    '''
    k = 0
    for i, (a, b) in enumerate(zip(index, index[1:])):
        if b - a > 1:
            if k != i:
                yield index[k], index[i] + 1
            k = i + 1
    if k < len(index):
        yield index[k], index[-1] + 1


def compile_yinyang(name, expression):
    return '''\
def %s(stack):
%s
    return stack
''' % (name, code_gen(I((), expression, [])))

```

A few functions to try it with...


```python
mul = Foo('mul')
sub = Foo('sub')
```


```python
def import_yin():
    from joy.utils.generated_library import *
    return locals()

yin_dict = {name: SimpleFunctionWrapper(func) for name, func in import_yin().iteritems()}

yin_dict

dup = yin_dict['dup']

#def dup(stack, expression, code):
#    n, stack = stack
#    return (n, (n, stack)), expression
```

... and there we are.


```python
print compile_yinyang('mul_', (names(), (names(), (mul, ()))))
```


```python
e = (names(), (dup, (mul, ())))
print compile_yinyang('sqr', e)
```


```python
e = (names(), (dup, (names(), (sub, (mul, ())))))
print compile_yinyang('foo', e)
```


```python
e = (names(), (names(), (mul, (dup, (sub, (dup, ()))))))
print compile_yinyang('bar', e)
```


```python
e = (names(), (dup, (dup, (mul, (dup, (mul, (mul, ())))))))
print compile_yinyang('to_the_fifth_power', e)
```


```python

```


```python

```


```python

```


```python

```
