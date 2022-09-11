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







@inscribe
@FunctionWrapper
def primrec(stack, expression, dictionary):
    '''
    From the "Overview of the language JOY":

    > The primrec combinator expects two quoted programs in addition to a
    data parameter. For an integer data parameter it works like this: If
    the data parameter is zero, then the first quotation has to produce
    the value to be returned. If the data parameter is positive then the
    second has to combine the data parameter with the result of applying
    the function to its predecessor.::

        5  [1]  [*]  primrec

    > Then primrec tests whether the top element on the stack (initially
    the 5) is equal to zero. If it is, it pops it off and executes one of
    the quotations, the [1] which leaves 1 on the stack as the result.
    Otherwise it pushes a decremented copy of the top element and
    recurses. On the way back from the recursion it uses the other
    quotation, [*], to multiply what is now a factorial on top of the
    stack by the second element on the stack.::

        n [Base] [Recur] primrec

           0 [Base] [Recur] primrec
        ------------------------------
              Base

             n [Base] [Recur] primrec
        ------------------------------------------ n > 0
           n (n-1) [Base] [Recur] primrec Recur

    '''
    recur, (base, (n, stack)) = stack
    if n <= 0:
        expression = concat(base, expression)
    else:
        expression = S_primrec, concat(recur, expression)
        stack = recur, (base, (n - 1, (n, stack)))
    return stack, expression, dictionary


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
def branch(stack, expression, dictionary):
    '''
    Use a Boolean value to select one of two quoted programs to run.

    ::

        branch == roll< choice i

    ::

           False [F] [T] branch
        --------------------------
              F

           True [F] [T] branch
        -------------------------
                 T

    '''
    (then, (else_, (flag, stack))) = stack
    return stack, concat(then if flag else else_, expression), dictionary


@inscribe
@FunctionWrapper
def ifte(stack, expression, dictionary):
  '''
  If-Then-Else Combinator
  ::

                  ... [if] [then] [else] ifte
       ---------------------------------------------------
          ... [[else] [then]] [...] [if] infra select i




                ... [if] [then] [else] ifte
       -------------------------------------------------------
          ... [else] [then] [...] [if] infra first choice i


  Has the effect of grabbing a copy of the stack on which to run the
  if-part using infra.
  '''
  (else_, (then, (if_, stack))) = stack
  expression = (S_infra, (S_first, (S_choice, (S_i, expression))))
  stack = (if_, (stack, (then, (else_, stack))))
  return stack, expression, dictionary


@inscribe
@FunctionWrapper
def cond(stack, expression, dictionary):
    '''
    This combinator works like a case statement.  It expects a single quote
    on the stack that must contain zero or more condition quotes and a 
    default quote.  Each condition clause should contain a quoted predicate
    followed by the function expression to run if that predicate returns
    true.  If no predicates return true the default function runs.

    It works by rewriting into a chain of nested `ifte` expressions, e.g.::

              [[[B0] T0] [[B1] T1] [D]] cond
        -----------------------------------------
           [B0] [T0] [[B1] [T1] [D] ifte] ifte

    '''
    conditions, stack = stack
    if conditions:
        expression = _cond(conditions, expression)
        try:
            # Attempt to preload the args to first ifte.
            (P, (T, (E, expression))) = expression
        except ValueError:
            # If, for any reason, the argument to cond should happen to contain
            # only the default clause then this optimization will fail.
            pass
        else:
            stack = (E, (T, (P, stack)))
    return stack, expression, dictionary


def _cond(conditions, expression):
    (clause, rest) = conditions
    if not rest:  # clause is [D]
        return clause
    P, T = clause
    return (P, (T, (_cond(rest, ()), (S_ifte, expression))))


@inscribe
@FunctionWrapper
def dip(stack, expression, dictionary):
    '''
    The dip combinator expects a quoted program on the stack and below it
    some item, it hoists the item into the expression and runs the program
    on the rest of the stack.
    ::

           ... x [Q] dip
        -------------------
             ... Q x

    '''
    try:
        (quote, (x, stack)) = stack
    except ValueError:
        raise StackUnderflowError('Not enough values on stack.')
    expression = (x, expression)
    return stack, concat(quote, expression), dictionary


@inscribe
@FunctionWrapper
def dipd(S, expression, dictionary):
    '''
    Like dip but expects two items.
    ::

           ... y x [Q] dip
        ---------------------
             ... Q y x

    '''
    (quote, (x, (y, stack))) = S
    expression = (y, (x, expression))
    return stack, concat(quote, expression), dictionary


@inscribe
@FunctionWrapper
def dipdd(S, expression, dictionary):
    '''
    Like dip but expects three items.
    ::

           ... z y x [Q] dip
        -----------------------
             ... Q z y x

    '''
    (quote, (x, (y, (z, stack)))) = S
    expression = (z, (y, (x, expression)))
    return stack, concat(quote, expression), dictionary


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


@inscribe
@FunctionWrapper
def loop(stack, expression, dictionary):
    '''
    Basic loop combinator.
    ::

           ... True [Q] loop
        -----------------------
              ... Q [Q] loop

           ... False [Q] loop
        ------------------------
              ...

    '''
    try:
        quote, stack = stack
    except ValueError:
        raise StackUnderflowError('Not enough values on stack.')
    if not isinstance(quote, tuple):
        raise NotAListError('Loop body not a list.')
    try:
        (flag, stack) = stack
    except ValueError:
        raise StackUnderflowError('Not enough values on stack.')
    if flag:
        expression = concat(quote, (quote, (S_loop, expression)))
    return stack, expression, dictionary


@inscribe
@FunctionWrapper
def cmp_(stack, expression, dictionary):
    '''
    cmp takes two values and three quoted programs on the stack and runs
    one of the three depending on the results of comparing the two values:
    ::

           a b [G] [E] [L] cmp
        ------------------------- a > b
            G

           a b [G] [E] [L] cmp
        ------------------------- a = b
                E

           a b [G] [E] [L] cmp
        ------------------------- a < b
                L
    '''
    L, (E, (G, (b, (a, stack)))) = stack
    expression = concat(G if a > b else L if a < b else E, expression)
    return stack, expression, dictionary


#  FunctionWrapper(cleave),
#  FunctionWrapper(while_),



for name, primitive in getmembers(genlib, isfunction):
    inscribe(SimpleFunctionWrapper(primitive))


