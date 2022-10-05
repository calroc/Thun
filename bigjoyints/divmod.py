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
    if a_len < b_len or A[-1] < B[-1]:
        return [], A

    # Whew! Okay, we got all that out of the way.
    # A > B

    A_digits = A[-b_len:]
    if -1 == cmp_digits(A_digits, B):
        # Because we know
        #   A > B  AND a_len >= b_len (aka len(A_digits))
        # if A_digits < B there must be at least one more
        # digit in A:
        assert a_len > b_len
        A_digits = A[-(b_len + 1):]
        assert -1 < cmp_digits(A_digits, B)
    q, r = lil_divmod(A_digits, B)

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
q, R = lil_divmod(A, B)
print(f'divmod({list_to_int(A)}, {list_to_int(B)}) = ',
      q, list_to_int(R))


##print(cmp_digits([], []))
##print(cmp_digits([], [1]))
##print(cmp_digits([1], []))
##print(cmp_digits([1], [1]))
##print(cmp_digits([0,1], [1]))








