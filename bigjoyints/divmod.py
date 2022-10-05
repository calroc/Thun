'''
Let's grok division (and modulus.)

For now we will deal with both positive
(or at least of the same sign.)


'''

def div_mod(A, B):
    '''
    A and B are lists of digits, LSB->MSB.
    '''
    if not A:
        return [], []
    if not B:
        raise ZeroDivisionError()
    a_len, b_len = len(A), len(B)
    if a_len < b_len or (a_len == b_len and  A[-1] < B[-1]):
        return [], A

    # Whew! Okay, we got all that out of the way.
    # A > B

    A, A_digits = A[:-b_len], A[-b_len:]
    if -1 == cmp_digits(A_digits, B):
        # Because we know
        #   A > B  AND a_len >= b_len (aka len(A_digits))
        # if A_digits < B there must be at least one more
        # digit in A:
        assert a_len > b_len
        A_digits.insert(0, A.pop())
        assert -1 < cmp_digits(A_digits, B)
    q, R = lil_divmod(A_digits, B)
    # So we have divided a prefix of A by B
    # resulting in a digit q of the answer Q
    # and a remainder R that must be extended
    # with the more digits of A to make a new
    # number N >= B
    # what I want to do here is...
    '''

      ___2___
    72)145000
      -144     = 72 * 2
       ---
         1

    B = 72
    A_digits = 145
    A = 000
    q = 2
    R = 1

    '''
    Q, Remainder = foo(A, B, R)
    Q.append(q)
    return Q, Remainder


def foo(Digits, Divisor, Prefix):
    '''

      ___2___
    72)145000
      -144     = 72 * 2
       ---
         1

    Digits = 000
    Divisor = 72
    Prefix = 1

    Q = []
    while Prefix < Divisor and Digits isn't empty:
        take a digit from Digits
        append it to Prefix
        add a zero to Q       <--  but only if new Prefix < Divisor

    One iteration through the while loop:

      ___20__
    72)145000
      -144|    = 72 * 2
       ---|
         10

    Another iteration through the while loop:

      ___20?_
    72)145000
      -144||   = 72 * 2
       ---||
         100  >= 72

    Digits = 0
    Divisor = 72 (still)
    Prefix = 100

    At this point Prefix >= Divisor OR Digits == []  OR BOTH
    if Prefix < Divisor AND not Digits:
        the remainder is Prefix
        return Q, Prefix
    if Prefix < Divisor AND Digits:
        Can't get here
    assert Prefix >= Divisor

    q, R = lil_divmod(Prefix, Divisor)

      ___200q
    72)145000
      -144||   = 72 * 2
       ---||
         100  >= 72
           N   = 72 * q
         ---
          28   = (100 - N) = (100 - 72q)

    R = 28
    New_Prefix = R
    Digits = 0 (still)
    Divisor = 72 (still)

    '''


##    Q = []
##    N = R
##    while A and -1 == cmp_digits(N, B):
##        N.insert(0, A.pop())
##        Q.insert(0, 0)
##    Q.append(q)
##    if not A:
##        return Q, N
##
##    Qz, R = div_mod(N, B)
##    return Qz + Q, R

def lil_divmod(A, B):
    assert -1 < cmp_digits(A, B)
    assert A and B
    # There is a greatest digit between 0..9 such that:
    # B * digit <= A
    # The obvious thing to do here is a bisect search,
    # if we were really just doing 0..9 we could go linear.
    digit = 9
    Q = mul_digit_by_list_of_digits(digit, B)
    while 1 == cmp_digits(Q, A):
        digit = digit - 1
        if not digit:
            raise ValueError('huh?')
        Q = mul_digit_by_list_of_digits(digit, B)
    return digit, subtract(A, Q)

def mul_digit_by_list_of_digits(digit, A):
    assert 0 <= digit <= 9
    for n in A:
        assert 0 <= n <= 9
    return int_to_list(list_to_int(A) * digit)

def int_to_list(i):
    assert i >= 0
    return list(map(int, str(i)[::-1]))

def list_to_int(A):
    if not A: return 0
    i = int(''.join(map(str, A[::-1])))
    assert i >= 0
    return i

def cmp_digits(A, B):
    if len(A) > len(B):
        return 1
    if len(A) < len(B):
        return -1
    for a, b in zip(reversed(A), reversed(B)):
        if a > b: return 1
        if a < b: return -1
    else:
        return 0


def subtract(A, B):
    return int_to_list(list_to_int(A) - list_to_int(B))


##A = int_to_list(123)
##B = int_to_list(72)
##print(A, B)
##Q = mul_digit_by_list_of_digits(9, A)
##print(Q)

A = int_to_list(145)
B = int_to_list(72)

##q, R = lil_divmod(A, B)
##print(f'divmod({list_to_int(A)}, {list_to_int(B)}) = ',
##      q, list_to_int(R))

def try_it(a, b):
    A = int_to_list(a)
    B = int_to_list(b)
    Q, R = div_mod(A, B)
    print(f'divmod({list_to_int(A)}, {list_to_int(B)}) = {list_to_int(Q)}, {list_to_int(R)}')

try_it(145, 72)
try_it(1450, 72)
try_it(145000, 72)


##print(cmp_digits([], []))
##print(cmp_digits([], [1]))
##print(cmp_digits([1], []))
##print(cmp_digits([1], [1]))
##print(cmp_digits([0,1], [1]))








