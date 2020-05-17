.. code:: ipython2

    from notebook_preamble import D, J, V, define

Compiling Joy
=============

Given a Joy program like:

::

   sqr == dup mul

.. code:: ipython2

    V('23 sqr')


.. parsed-literal::

          . 23 sqr
       23 . sqr
       23 . dup mul
    23 23 . mul
      529 . 


How would we go about compiling this code (to Python for now)?

Naive Call Chaining
-------------------

The simplest thing would be to compose the functions from the library:

.. code:: ipython2

    dup, mul = D['dup'], D['mul']

.. code:: ipython2

    def sqr(stack, expression, dictionary):
        return mul(*dup(stack, expression, dictionary))

.. code:: ipython2

    old_sqr = D['sqr']
    D['sqr'] = sqr

.. code:: ipython2

    V('23 sqr')


.. parsed-literal::

        . 23 sqr
     23 . sqr
    529 . 


It’s simple to write a function to emit this kind of crude “compiled”
code.

.. code:: ipython2

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


.. code:: ipython2

    print compile_joy_definition(old_sqr)


.. parsed-literal::

    def sqr(stack, expression, dictionary):
        return mul(*dup(stack, expression, dictionary))
    


But what about literals?

::

   quoted == [unit] dip

.. code:: ipython2

    unit, dip = D['unit'], D['dip']

.. code:: ipython2

    # print compile_joy_definition(D['quoted'])
    # raises
    # TypeError: can only concatenate tuple (not "str") to tuple

For a program like ``foo == bar baz 23 99 baq lerp barp`` we would want
something like:

.. code:: ipython2

    def foo(stack, expression, dictionary):
        stack, expression, dictionary = baz(*bar(stack, expression, dictionary))
        return barp(*lerp(*baq((99, (23, stack)), expression, dictionary)))

You have to have a little discontinuity when going from a symbol to a
literal, because you have to pick out the stack from the arguments to
push the literal(s) onto it before you continue chaining function calls.

Compiling Yin Functions
-----------------------

Call-chaining results in code that does too much work. For functions
that operate on stacks and only rearrange values, what I like to call
“Yin Functions”, we can do better.

We can infer the stack effects of these functions (or “expressions” or
“programs”) automatically, and the stack effects completely define the
semantics of the functions, so we can directly write out a two-line
Python function for them. This is already implemented in the
``joy.utils.types.compile_()`` function.

.. code:: ipython2

    from joy.utils.types import compile_, doc_from_stack_effect, infer_string
    from joy.library import SimpleFunctionWrapper

.. code:: ipython2

    stack_effects = infer_string('tuck over dup')

Yin functions have only a single stack effect, they do not branch or
loop.

.. code:: ipython2

    for fi, fo in stack_effects:
        print doc_from_stack_effect(fi, fo)


.. parsed-literal::

    (a2 a1 -- a1 a2 a1 a2 a2)


.. code:: ipython2

    source = compile_('foo', stack_effects[0])

All Yin functions can be described in Python as a tuple-unpacking (or
“-destructuring”) of the stack datastructure followed by building up the
new stack structure.

.. code:: ipython2

    print source


.. parsed-literal::

    def foo(stack):
      """
      ::
    
      (a2 a1 -- a1 a2 a1 a2 a2)
    
      """
      (a1, (a2, s1)) = stack
      return (a2, (a2, (a1, (a2, (a1, s1)))))


.. code:: ipython2

    exec compile(source, '__main__', 'single')
    
    D['foo'] = SimpleFunctionWrapper(foo)

.. code:: ipython2

    V('23 18 foo')


.. parsed-literal::

                   . 23 18 foo
                23 . 18 foo
             23 18 . foo
    18 23 18 23 23 . 


Compiling from Stack Effects
----------------------------

There are times when you’re deriving a Joy program when you have a stack
effect for a Yin function and you need to define it. For example, in the
Ordered Binary Trees notebook there is a point where we must derive a
function ``Ee``:

::

      [key old_value left right] new_value key [Tree-add] Ee
   ------------------------------------------------------------
      [key new_value left right]

While it is not hard to come up with this function manually, there is no
necessity. This function can be defined (in Python) directly from its
stack effect:

::

      [a b c d] e a [f] Ee
   --------------------------
      [a e c d]

(I haven’t yet implemented a simple interface for this yet. What follow
is an exploration of how to do it.)

.. code:: ipython2

    from joy.parser import text_to_expression

.. code:: ipython2

    Ein = '[a b c d] e a [f]'  # The terms should be reversed here but I don't realize that until later.
    Eout = '[a e c d]'
    E = '[%s] [%s]' % (Ein, Eout)
    
    print E


.. parsed-literal::

    [[a b c d] e a [f]] [[a e c d]]


.. code:: ipython2

    (fi, (fo, _)) = text_to_expression(E)

.. code:: ipython2

    fi, fo




.. parsed-literal::

    (((a, (b, (c, (d, ())))), (e, (a, ((f, ()), ())))),
     ((a, (e, (c, (d, ())))), ()))



.. code:: ipython2

    Ein = '[a1 a2 a3 a4] a5 a6 a7'
    Eout = '[a1 a5 a3 a4]'
    E = '[%s] [%s]' % (Ein, Eout)
    
    print E


.. parsed-literal::

    [[a1 a2 a3 a4] a5 a6 a7] [[a1 a5 a3 a4]]


.. code:: ipython2

    (fi, (fo, _)) = text_to_expression(E)

.. code:: ipython2

    fi, fo




.. parsed-literal::

    (((a1, (a2, (a3, (a4, ())))), (a5, (a6, (a7, ())))),
     ((a1, (a5, (a3, (a4, ())))), ()))



.. code:: ipython2

    def type_vars():
        from joy.library import a1, a2, a3, a4, a5, a6, a7, s0, s1
        return locals()
    
    tv = type_vars()
    tv




.. parsed-literal::

    {'a1': a1,
     'a2': a2,
     'a3': a3,
     'a4': a4,
     'a5': a5,
     'a6': a6,
     'a7': a7,
     's0': s0,
     's1': s1}



.. code:: ipython2

    from joy.utils.types import reify

.. code:: ipython2

    stack_effect = reify(tv, (fi, fo))
    print doc_from_stack_effect(*stack_effect)


.. parsed-literal::

    (... a7 a6 a5 [a1 a2 a3 a4 ] -- ... [a1 a5 a3 a4 ])


.. code:: ipython2

    print stack_effect


.. parsed-literal::

    (((a1, (a2, (a3, (a4, ())))), (a5, (a6, (a7, ())))), ((a1, (a5, (a3, (a4, ())))), ()))


Almost, but what we really want is something like this:

.. code:: ipython2

    stack_effect = eval('(((a1, (a2, (a3, (a4, s1)))), (a5, (a6, (a7, s0)))), ((a1, (a5, (a3, (a4, s1)))), s0))', tv)

Note the change of ``()`` to ``JoyStackType`` type variables.

.. code:: ipython2

    print doc_from_stack_effect(*stack_effect)


.. parsed-literal::

    (a7 a6 a5 [a1 a2 a3 a4 ...1] -- [a1 a5 a3 a4 ...1])


Now we can omit ``a3`` and ``a4`` if we like:

.. code:: ipython2

    stack_effect = eval('(((a1, (a2, s1)), (a5, (a6, (a7, s0)))), ((a1, (a5, s1)), s0))', tv)

The ``right`` and ``left`` parts of the ordered binary tree node are
subsumed in the tail of the node’s stack/list.

.. code:: ipython2

    print doc_from_stack_effect(*stack_effect)


.. parsed-literal::

    (a7 a6 a5 [a1 a2 ...1] -- [a1 a5 ...1])


.. code:: ipython2

    source = compile_('Ee', stack_effect)
    print source


.. parsed-literal::

    def Ee(stack):
      """
      ::
    
      (a7 a6 a5 [a1 a2 ...1] -- [a1 a5 ...1])
    
      """
      ((a1, (a2, s1)), (a5, (a6, (a7, s0)))) = stack
      return ((a1, (a5, s1)), s0)


Oops! The input stack is backwards…

.. code:: ipython2

    stack_effect = eval('((a7, (a6, (a5, ((a1, (a2, s1)), s0)))), ((a1, (a5, s1)), s0))', tv)

.. code:: ipython2

    print doc_from_stack_effect(*stack_effect)


.. parsed-literal::

    ([a1 a2 ...1] a5 a6 a7 -- [a1 a5 ...1])


.. code:: ipython2

    source = compile_('Ee', stack_effect)
    print source


.. parsed-literal::

    def Ee(stack):
      """
      ::
    
      ([a1 a2 ...1] a5 a6 a7 -- [a1 a5 ...1])
    
      """
      (a7, (a6, (a5, ((a1, (a2, s1)), s0)))) = stack
      return ((a1, (a5, s1)), s0)


Compare:

::

      [key old_value left right] new_value key [Tree-add] Ee
   ------------------------------------------------------------
      [key new_value left right]

.. code:: ipython2

    eval(compile(source, '__main__', 'single'))
    D['Ee'] = SimpleFunctionWrapper(Ee)

.. code:: ipython2

    V('[a b c d] 1 2 [f] Ee')


.. parsed-literal::

                      . [a b c d] 1 2 [f] Ee
            [a b c d] . 1 2 [f] Ee
          [a b c d] 1 . 2 [f] Ee
        [a b c d] 1 2 . [f] Ee
    [a b c d] 1 2 [f] . Ee
            [a 1 c d] . 



Working with Yang Functions
---------------------------

Consider the compiled code of ``dup``:

.. code:: ipython2

    
    def dup(stack):
        (a1, s23) = stack
        return (a1, (a1, s23))
    


To compile ``sqr == dup mul`` we can compute the stack effect:

.. code:: ipython2

    stack_effects = infer_string('dup mul')
    for fi, fo in stack_effects:
        print doc_from_stack_effect(fi, fo)


.. parsed-literal::

    (n1 -- n2)


Then we would want something like this:

.. code:: ipython2

    
    def sqr(stack):
        (n1, s23) = stack
        n2 = mul(n1, n1)
        return (n2, s23)
    




How about…

.. code:: ipython2

    stack_effects = infer_string('mul mul sub')
    for fi, fo in stack_effects:
        print doc_from_stack_effect(fi, fo)


.. parsed-literal::

    (n4 n3 n2 n1 -- n5)


.. code:: ipython2

    
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
    



.. code:: ipython2

    stack_effects = infer_string('tuck')
    for fi, fo in stack_effects:
        print doc_from_stack_effect(fi, fo)


.. parsed-literal::

    (a2 a1 -- a1 a2 a1)



Compiling Yin~Yang Functions
----------------------------

First, we need a source of Python identifiers. I’m going to reuse
``Symbol`` class for this.

.. code:: ipython2

    from joy.parser import Symbol

.. code:: ipython2

    def _names():
        n = 0
        while True:
            yield Symbol('a' + str(n))
            n += 1
    
    names = _names().next

Now we need an object that represents a Yang function that accepts two
args and return one result (we’ll implement other kinds a little later.)

.. code:: ipython2

    class Foo(object):
    
        def __init__(self, name):
            self.name = name
    
        def __call__(self, stack, expression, code):
            in1, (in0, stack) = stack
            out = names()
            code.append(('call', out, self.name, (in0, in1)))
            return (out, stack), expression, code

A crude “interpreter” that translates expressions of args and Yin and
Yang functions into a kind of simple dataflow graph.

.. code:: ipython2

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

Something to convert the graph into Python code.

.. code:: ipython2

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


A few functions to try it with…

.. code:: ipython2

    mul = Foo('mul')
    sub = Foo('sub')

.. code:: ipython2

    def import_yin():
        from joy.utils.generated_library import *
        return locals()
    
    yin_dict = {name: SimpleFunctionWrapper(func) for name, func in import_yin().iteritems()}
    
    yin_dict
    
    dup = yin_dict['dup']
    
    #def dup(stack, expression, code):
    #    n, stack = stack
    #    return (n, (n, stack)), expression


.. parsed-literal::

    <ipython-input-74-a6ea700b09d9>:1: SyntaxWarning: import * only allowed at module level
      def import_yin():


… and there we are.

.. code:: ipython2

    print compile_yinyang('mul_', (names(), (names(), (mul, ()))))


.. parsed-literal::

    def mul_(stack):
        (a31, (a32, stack)) = stack
        a33 = mul(a32, a31)
        stack = (a33, stack)
        return stack
    


.. code:: ipython2

    e = (names(), (dup, (mul, ())))
    print compile_yinyang('sqr', e)


.. parsed-literal::

    def sqr(stack):
        (a34, stack) = stack
        a35 = mul(a34, a34)
        stack = (a35, stack)
        return stack
    


.. code:: ipython2

    e = (names(), (dup, (names(), (sub, (mul, ())))))
    print compile_yinyang('foo', e)


.. parsed-literal::

    def foo(stack):
        (a36, (a37, stack)) = stack
        a38 = sub(a37, a36)
        a39 = mul(a38, a36)
        stack = (a39, stack)
        return stack
    


.. code:: ipython2

    e = (names(), (names(), (mul, (dup, (sub, (dup, ()))))))
    print compile_yinyang('bar', e)


.. parsed-literal::

    def bar(stack):
        (a40, (a41, stack)) = stack
        a42 = mul(a41, a40)
        a43 = sub(a42, a42)
        stack = (a43, (a43, stack))
        return stack
    


.. code:: ipython2

    e = (names(), (dup, (dup, (mul, (dup, (mul, (mul, ())))))))
    print compile_yinyang('to_the_fifth_power', e)


.. parsed-literal::

    def to_the_fifth_power(stack):
        (a44, stack) = stack
        a45 = mul(a44, a44)
        a46 = mul(a45, a45)
        a47 = mul(a46, a44)
        stack = (a47, stack)
        return stack
    





