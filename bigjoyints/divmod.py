#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#    Copyright © 2022 Simon Forman
#
#    This file is part of Thun
#
#    Thun is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    Thun is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Thun.  If not see <http://www.gnu.org/licenses/>.
#
'''
Let's grok division (and modulus.)

For now we will deal with both positive
(or at least of the same sign.)


'''
from random import randint


def div_mod(A, B):
    '''
    A and B are lists of digits, LSB->MSB.
    '''
    if not A:
        return [], []
    if not B:
        raise ZeroDivisionError()
    a_len, b_len = len(A), len(B)
    if -1 == cmp_digits(A, B):  # A < B
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

    assert -1 < cmp_digits(A_digits, B)  # A_digits >= B

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
    '''E.g.:

      ___2___
    72)145000
      -144     = 72 * 2
       ---
         1

    Digits = 000
    Divisor = 72
    Prefix = 1
    '''

    Quotient = []
    while Digits:
        Prefix.insert(0, Digits.pop())
        if -1 < cmp_digits(Prefix, Divisor):  # Prefix >= Divisor
            break
        Quotient.insert(0, 0)

    '''
    One iteration through the while loop:

      ___20__
    72)145000
      -144|    = 72 * 2
       ---|
         10

    Another iteration through the while loop:

      ___20__
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
        return Quotient, Prefix
    '''

    if -1 == cmp_digits(Prefix, Divisor) and not Digits:
        return Quotient, Prefix

    '''
    if Prefix < Divisor AND Digits:
        Can't get here
    '''

    assert -1 < cmp_digits(Prefix, Divisor)  # Prefix >= Divisor

    q, Remainder = lil_divmod(Prefix, Divisor)
    Quotient.insert(0, q)

    '''
      ___20q_
    72)145000
      -144||   = 72 * 2
       ---||
         100  >= 72
           N   = 72 * q
         ---
          28   = (100 - N) = (100 - 72q)

    Remainder = 28
    New_Prefix = Remainder
    Digits = 0 (still)
    Divisor = 72 (still)
    '''

    if Digits:
        Q, Remainder = foo(Digits, Divisor, Remainder)
        Quotient = Q + Quotient
    return Quotient, Remainder


def lil_divmod(A, B):
    assert -1 < cmp_digits(A, B)  # A >= B
    assert A and B
    # There is a greatest digit in 1..9 such that:
    # B * digit <= A
    # The obvious thing to do here is a bisect search,
    # if we were really just doing 1..9 we could go linear.
    # Maybe drive it by the bits in digit?
    digit = 9
    Q = mul_digit_by_list_of_digits(digit, B)
    while 1 == cmp_digits(Q, A):  # Q > A
        digit = digit - 1
        if not digit:
            raise ValueError('huh?')
        Q = mul_digit_by_list_of_digits(digit, B)
    assert -1 < cmp_digits(A, Q)
    assert list_to_int(A) >= list_to_int(Q)
    remainder = subtract(A, Q)
    assert -1 == cmp_digits(remainder, B)  # Remainder < Divisor
    return digit, remainder

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
    a, b = list_to_int(A), list_to_int(B)
    return 1 if a > b else 0 if a == b else -1

def subtract(A, B):
    return int_to_list(list_to_int(A) - list_to_int(B))

def try_it(a, b):
    A = int_to_list(a)
    B = int_to_list(b)
    print(f'divmod({list_to_int(A)}, {list_to_int(B)}) = ', end='')
    Q, R = div_mod(A, B)
    q, r = divmod(a, b)
    assert q == list_to_int(Q)
    assert r == list_to_int(R)
    print(f'{list_to_int(Q)}, {list_to_int(R)}')

for _ in range(20):
    try_it(
        randint(0, 10**30),
        randint(0, 10**4)
        )
