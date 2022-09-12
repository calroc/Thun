ALIASES = (
    ('bool', ['truthy']),
    ('mod', ['%', 'rem', 'remainder', 'modulus']),
    ('getitem', ['pick', 'at']),
    ('xor', ['^']),
    ('eh', ['?']),
    ('id', [u'•']),
    )

def floor(n):
    return int(math.floor(n))

floor.__doc__ = math.floor.__doc__


@inscribe
@SimpleFunctionWrapper
def divmod_(S):
    '''
    divmod(x, y) -> (quotient, remainder)

    Return the tuple (x//y, x%y).  Invariant: q * y + r == x.
    '''
    y, (x, stack) = S
    q, r = divmod(x, y)
    return r, (q, stack)


@inscribe
@SimpleFunctionWrapper
def id_(stack):
    '''The identity function.'''
    return stack



#
# § Combinators
#


# Several combinators depend on other words in their definitions,
# we use symbols to prevent hard-coding these, so in theory, you
# could change the word in the dictionary to use different semantics.
S_choice = Symbol('choice')
S_first = Symbol('first')
S_genrec = Symbol('genrec')
S_getitem = Symbol('getitem')
S_i = Symbol('i')
S_ifte = Symbol('ifte')
S_infra = Symbol('infra')
S_loop = Symbol('loop')
S_pop = Symbol('pop')
S_primrec = Symbol('primrec')
S_step = Symbol('step')
S_swaack = Symbol('swaack')
S_times = Symbol('times')






#def cleave(S, expression, dictionary):
#  '''
#  The cleave combinator expects two quotations, and below that an item X.
#  It first executes [P], with X on top, and saves the top result element.
#  Then it executes [Q], again with X, and saves the top result.
#  Finally it restores the stack to what it was below X and pushes the two
#  results P(X) and Q(X).
#  '''
#  (Q, (P, (x, stack))) = S
#  p = joy((x, stack), P, dictionary)[0][0]
#  q = joy((x, stack), Q, dictionary)[0][0]
#  return (q, (p, stack)), expression, dictionary


@inscribe
@FunctionWrapper
def app1(S, expression, dictionary):
    '''
    Given a quoted program on TOS and anything as the second stack item run
    the program and replace the two args with the first result of the
    program.
    ::

             ... x [Q] . app1
        -----------------------------------
           ... [x ...] [Q] . infra first

    '''
    (quote, (x, stack)) = S
    stack = (quote, ((x, stack), stack))
    expression = (S_infra, (S_first, expression))
    return stack, expression, dictionary


@inscribe
@FunctionWrapper
def app2(S, expression, dictionary):
    '''Like app1 with two items.
    ::

               ... y x [Q] . app2
        -----------------------------------
           ... [y ...] [Q] . infra first
               [x ...] [Q]   infra first

    '''
    (quote, (x, (y, stack))) = S
    expression = (S_infra, (S_first,
        ((x, stack), (quote, (S_infra, (S_first,
            expression))))))
    stack = (quote, ((y, stack), stack))
    return stack, expression, dictionary


@inscribe
@FunctionWrapper
def app3(S, expression, dictionary):
    '''Like app1 with three items.
    ::

             ... z y x [Q] . app3
        -----------------------------------
           ... [z ...] [Q] . infra first
               [y ...] [Q]   infra first
               [x ...] [Q]   infra first

    '''
    (quote, (x, (y, (z, stack)))) = S
    expression = (S_infra, (S_first,
        ((y, stack), (quote, (S_infra, (S_first,
        ((x, stack), (quote, (S_infra, (S_first,
            expression))))))))))
    stack = (quote, ((z, stack), stack))
    return stack, expression, dictionary


@inscribe
@FunctionWrapper
def step(S, expression, dictionary):
    '''
    Run a quoted program on each item in a sequence.
    ::

           ... [] [Q] . step
        -----------------------
              ... .


           ... [a] [Q] . step
        ------------------------
             ... a . Q


           ... [a b c] [Q] . step
        ----------------------------------------
                 ... a . Q [b c] [Q] step

    The step combinator executes the quotation on each member of the list
    on top of the stack.
    '''
    (quote, (aggregate, stack)) = S
    if not aggregate:
        return stack, expression, dictionary
    head, tail = aggregate
    stack = quote, (head, stack)
    if tail:
        expression = tail, (quote, (S_step, expression))
    expression = S_i, expression
    return stack, expression, dictionary


@inscribe
@FunctionWrapper
def times(stack, expression, dictionary):
    '''
    times == [-- dip] cons [swap] infra [0 >] swap while pop
    ::

           ... n [Q] . times
        ---------------------  w/ n <= 0
             ... .


           ... 1 [Q] . times
        -----------------------
             ... . Q


           ... n [Q] . times
        -------------------------------------  w/ n > 1
             ... . Q (n - 1) [Q] times

    '''
    # times == [-- dip] cons [swap] infra [0 >] swap while pop
    (quote, (n, stack)) = stack
    if n <= 0:
        return stack, expression, dictionary
    n -= 1
    if n:
        expression = n, (quote, (S_times, expression))
    expression = concat(quote, expression)
    return stack, expression, dictionary


# The current definition above works like this:

#             [P] [Q] while
# --------------------------------------
#    [P] nullary [Q [P] nullary] loop

#   while == [pop i not] [popop] [dudipd] tailrec

#def while_(S, expression, dictionary):
#  '''[if] [body] while'''
#  (body, (if_, stack)) = S
#  while joy(stack, if_, dictionary)[0][0]:
#    stack = joy(stack, body, dictionary)[0]
#  return stack, expression, dictionary


