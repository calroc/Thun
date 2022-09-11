ALIASES = (
    ('bool', ['truthy']),
    ('mod', ['%', 'rem', 'remainder', 'modulus']),
    ('getitem', ['pick', 'at']),
    ('xor', ['^']),
    ('eh', ['?']),
    ('id', [u'•']),
    )

@inscribe
@FunctionWrapper
def inscribe_(stack, expression, dictionary):
    '''
    Create a new Joy function definition in the Joy dictionary.  A
    definition is given as a quote with a name followed by a Joy
    expression. for example:

        [sqr dup mul] inscribe

    '''
    (name, body), stack = stack
    inscribe(Def(name, body), dictionary)
    return stack, expression, dictionary


@inscribe
@SimpleFunctionWrapper
def getitem(stack):
    '''
    ::

        getitem == drop first

    Expects an integer and a quote on the stack and returns the item at the
    nth position in the quote counting from 0.
    ::

           [a b c d] 0 getitem
        -------------------------
            a

    '''
    n, (Q, stack) = stack
    return pick(Q, n), stack


@inscribe
@SimpleFunctionWrapper
def drop(stack):
    '''
    ::

        drop == [rest] times

    Expects an integer and a quote on the stack and returns the quote with
    n items removed off the top.
    ::

           [a b c d] 2 drop
        ----------------------
               [c d]

    '''
    n, (Q, stack) = stack
    while n > 0:
        try:
            _, Q = Q
        except ValueError:
            raise IndexError
        n -= 1
    return Q, stack


@inscribe
@SimpleFunctionWrapper
def take(stack):
    '''
    Expects an integer and a quote on the stack and returns the quote with
    just the top n items in reverse order (because that's easier and you can
    use reverse if needed.)
    ::

           [a b c d] 2 take
        ----------------------
               [b a]

    '''
    n, (Q, stack) = stack
    x = ()
    while n > 0:
        try:
            item, Q = Q
        except ValueError:
            raise IndexError
        x = item, x
        n -= 1
    return x, stack


@inscribe
@FunctionWrapper
def gcd2(stack, expression, dictionary):
    '''Compiled GCD function.'''
    (v1, (v2, stack)) = stack
    tos = True
    while tos:
        v3 = v2 % v1
        tos = v3 > 0
        (v1, (v2, stack)) = (v3, (v1, stack))
    return (v2, stack), expression, dictionary


@inscribe
@SimpleFunctionWrapper
def choice(stack):
    '''
    Use a Boolean value to select one of two items.
    ::

           A B false choice
        ----------------------
           A


           A B true choice
        ---------------------
             B

    '''
    (if_, (then, (else_, stack))) = stack
    assert isinstance(if_, bool), repr(if_)
    return then if if_ else else_, stack


@inscribe
@SimpleFunctionWrapper
def select(stack):
    '''
    Use a Boolean value to select one of two items from a sequence.
    ::

           [A B] false select
        ------------------------
            A


           [A B] true select
        -----------------------
              B

    The sequence can contain more than two items but not fewer.
    Currently Python semantics are used to evaluate the "truthiness" of the
    Boolean value (so empty string, zero, etc. are counted as false, etc.)
    '''
    (flag, (choices, stack)) = stack
    (else_, (then, _)) = choices
    return then if flag else else_, stack


@inscribe
@SimpleFunctionWrapper
def max_(S):
    '''Given a list find the maximum.'''
    tos, stack = S
    return max(iter_stack(tos)), stack


@inscribe
@SimpleFunctionWrapper
def min_(S):
    '''Given a list find the minimum.'''
    tos, stack = S
    return min(iter_stack(tos)), stack


@inscribe
@SimpleFunctionWrapper
def sum_(S):
    '''
    Given a quoted sequence of numbers return the sum.
    ::

        sum == 0 swap [+] step

    '''
    tos, stack = S
    return sum(iter_stack(tos)), stack


@inscribe
@SimpleFunctionWrapper
def remove(S):
    '''
    Expects an item on the stack and a quote under it and removes that item
    from the the quote.  The item is only removed once.  If the list is
    empty or the item isn't in the list then the list is unchanged.
    ::

           [1 2 3 1] 1 remove
        ------------------------
             [2 3 1]

    '''
    (item, (quote, stack)) = S
    return _remove(item, quote), stack


def _remove(item, quote):
    try: head, tail = quote
    except ValueError: return quote
    return tail if head == item else (head, _remove(item, tail))


@inscribe
@SimpleFunctionWrapper
def unique(S):
    '''Given a list remove duplicate items.'''
    tos, stack = S
    I = list(iter_stack(tos))
    return list_to_stack(sorted(set(I), key=I.index)), stack


@inscribe
@SimpleFunctionWrapper
def sort_(S):
    '''Given a list return it sorted.'''
    tos, stack = S
    return list_to_stack(sorted(iter_stack(tos))), stack


@inscribe
@SimpleFunctionWrapper
def disenstacken(stack):
    '''
    The disenstacken operator expects a list on top of the stack and makes that
    the stack discarding the rest of the stack.
    '''
    return stack[0]


@inscribe
@SimpleFunctionWrapper
def reverse(S):
    '''
    Reverse the list on the top of the stack.
    ::

        reverse == [] swap shunt
    '''
    (tos, stack) = S
    res = ()
    for term in iter_stack(tos):
        res = term, res
    return res, stack


@inscribe
@SimpleFunctionWrapper
def shunt(stack):
    '''
    Like concat but reverses the top list into the second.
    ::

        shunt == [swons] step == reverse swap concat

           [a b c] [d e f] shunt
        ---------------------------
               [f e d a b c] 

    '''
    (tos, (second, stack)) = stack
    while tos:
        term, tos = tos
        second = term, second
    return second, stack


@inscribe
@SimpleFunctionWrapper
def zip_(S):
    '''
    Replace the two lists on the top of the stack with a list of the pairs
    from each list.  The smallest list sets the length of the result list.
    '''
    (tos, (second, stack)) = S
    accumulator = [
        (a, (b, ()))
        for a, b in zip(iter_stack(tos), iter_stack(second))
        ]
    return list_to_stack(accumulator), stack


@inscribe
@SimpleFunctionWrapper
def succ(S):
    '''Increment TOS.'''
    (tos, stack) = S
    return tos + 1, stack


@inscribe
@SimpleFunctionWrapper
def pred(S):
    '''Decrement TOS.'''
    (tos, stack) = S
    return tos - 1, stack


@inscribe
@SimpleFunctionWrapper
def pm(stack):
    '''
    Plus or minus
    ::

           a b pm
        -------------
           a+b a-b

    '''
    a, (b, stack) = stack
    p, m, = b + a, b - a
    return m, (p, stack)


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



@inscribe
@FunctionWrapper
def sharing(stack, expression, dictionary):
    '''Print redistribution information.'''
    print("You may convey verbatim copies of the Program's source code as"
    ' you receive it, in any medium, provided that you conspicuously'
    ' and appropriately publish on each copy an appropriate copyright'
    ' notice; keep intact all notices stating that this License and'
    ' any non-permissive terms added in accord with section 7 apply'
    ' to the code; keep intact all notices of the absence of any'
    ' warranty; and give all recipients a copy of this License along'
    ' with the Program.'
    ' You should have received a copy of the GNU General Public License'
    ' along with Thun.  If not see <http://www.gnu.org/licenses/>.')
    return stack, expression, dictionary


@inscribe
@FunctionWrapper
def warranty(stack, expression, dictionary):
    '''Print warranty information.'''
    print('THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY'
    ' APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE'
    ' COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM'
    ' "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR'
    ' IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES'
    ' OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE'
    ' ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS'
    ' WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE'
    ' COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.')
    return stack, expression, dictionary


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
def i(stack, expression, dictionary):
    '''
    The i combinator expects a quoted program on the stack and unpacks it
    onto the pending expression for evaluation.
    ::

           [Q] i
        -----------
            Q

    '''
    try:
        quote, stack = stack
    except ValueError:
        raise StackUnderflowError('Not enough values on stack.')
    return stack, concat(quote, expression), dictionary


@inscribe
@FunctionWrapper
def x(stack, expression, dictionary):
    '''
    ::

        x == dup i

        ... [Q] x = ... [Q] dup i
        ... [Q] x = ... [Q] [Q] i
        ... [Q] x = ... [Q]  Q

    '''
    quote, _ = stack
    return stack, concat(quote, expression), dictionary


@inscribe
@FunctionWrapper
def b(stack, expression, dictionary):
    '''
    ::

        b == [i] dip i

        ... [P] [Q] b == ... [P] i [Q] i
        ... [P] [Q] b == ... P Q

    '''
    q, (p, (stack)) = stack
    return stack, concat(p, concat(q, expression)), dictionary


@inscribe
@FunctionWrapper
def ii(stack, expression, dictionary):
    '''
    ::

           ... a [Q] ii
        ------------------
            ... Q a Q

    '''
    quote, (a, stack) = stack
    expression = concat(quote, (a, concat(quote, expression)))
    return stack, expression, dictionary


@inscribe
@FunctionWrapper
def dupdip(stack, expression, dictionary):
    '''
    ::

        [F] dupdip == dup [F] dip

        ... a [F] dupdip
        ... a dup [F] dip
        ... a a   [F] dip
        ... a F a

    '''
    F, stack = stack
    a = stack[0]
    return stack, concat(F, (a,  expression)), dictionary


@inscribe
@FunctionWrapper
def infra(stack, expression, dictionary):
    '''
    Accept a quoted program and a list on the stack and run the program
    with the list as its stack.  Does not affect the rest of the stack.
    ::

           ... [a b c] [Q] . infra
        -----------------------------
            c b a . Q [...] swaack

    '''
    (quote, (aggregate, stack)) = stack
    return aggregate, concat(quote, (stack, (S_swaack, expression))), dictionary


@inscribe
@FunctionWrapper
def genrec(stack, expression, dictionary):
    '''
    General Recursion Combinator.
    ::

                  [if] [then] [rec1] [rec2] genrec
        ---------------------------------------------------------------------
           [if] [then] [rec1 [[if] [then] [rec1] [rec2] genrec] rec2] ifte

    From "Recursion Theory and Joy" (j05cmp.html) by Manfred von Thun:
    "The genrec combinator takes four program parameters in addition to
    whatever data parameters it needs. Fourth from the top is an if-part,
    followed by a then-part. If the if-part yields true, then the then-part
    is executed and the combinator terminates. The other two parameters are
    the rec1-part and the rec2-part. If the if-part yields false, the
    rec1-part is executed. Following that the four program parameters and
    the combinator are again pushed onto the stack bundled up in a quoted
    form. Then the rec2-part is executed, where it will find the bundled
    form. Typically it will then execute the bundled form, either with i or
    with app2, or some other combinator."

    The way to design one of these is to fix your base case [then] and the
    test [if], and then treat rec1 and rec2 as an else-part "sandwiching"
    a quotation of the whole function.

    For example, given a (general recursive) function 'F':
    ::

        F == [I] [T] [R1] [R2] genrec

    If the [I] if-part fails you must derive R1 and R2 from:
    ::

        ... R1 [F] R2

    Just set the stack arguments in front, and figure out what R1 and R2
    have to do to apply the quoted [F] in the proper way.  In effect, the
    genrec combinator turns into an ifte combinator with a quoted copy of
    the original definition in the else-part:
    ::

        F == [I] [T] [R1]   [R2] genrec
          == [I] [T] [R1 [F] R2] ifte

    Primitive recursive functions are those where R2 == i.
    ::

        P == [I] [T] [R] tailrec
          == [I] [T] [R [P] i] ifte
          == [I] [T] [R P] ifte

    '''
    (rec2, (rec1, stack)) = stack
    (then, (if_, _)) = stack
    F = (if_, (then, (rec1, (rec2, (S_genrec, ())))))
    else_ = concat(rec1, (F, rec2))
    return (else_, stack), (S_ifte, expression), dictionary


@inscribe
@FunctionWrapper
def map_(S, expression, dictionary):
    '''
    Run the quoted program on TOS on the items in the list under it, push a
    new list with the results in place of the program and original list.
    '''
    # (quote, (aggregate, stack)) = S
    # results = list_to_stack([
    # joy((term, stack), quote, dictionary)[0][0]
    # for term in iter_stack(aggregate)
    # ])
    # return (results, stack), expression, dictionary
    (quote, (aggregate, stack)) = S
    if not aggregate:
        return (aggregate, stack), expression, dictionary
    batch = ()
    for term in iter_stack(aggregate):
        s = term, stack
        batch = (s, (quote, (S_infra, (S_first, batch))))
    stack = (batch, ((), stack))
    return stack, (S_infra, expression), dictionary


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


