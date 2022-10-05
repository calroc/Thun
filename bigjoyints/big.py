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
from copy import copy
from random import randint
from itertools import zip_longest
from pprint import pprint as P
import  unittest


class BigInt:

    def __init__(self, initial=0):
        # We store a sign bit (True == non-negative)
        # and a list of OberonInt, least significant digit
        # to most.
        self.sign = initial >= 0
        if not self.sign:
            initial = -initial
        self.digits = list(self.digitize(initial))  # List of OberonInt.

    def __repr__(self):
        return f'BigInt({self.to_int()})'

    @staticmethod
    def digitize(n):
        if n < 0:
            raise ValueError(f'Non-negative only: {n}')
        while n:
            n, digit = divmod(n, 2**31)
            yield OberonInt(digit)

    def __str__(self):
        return str(self.to_int())

    def to_int(self):
        power = 1
        n = 0
        for digit in self.digits:
            n += digit.value * power
            power <<= 31
        if not self.sign:
            n = -n
        return n

    def negate(self):
        result = BigInt()
        result.sign = not self.sign
        result.digits = [
            OberonInt(digit.value ^ 2**31 - 1)
            for digit in self.digits
            ]
        return result

    def __add__(self, other):
        if not isinstance(other, BigInt):
            other = BigInt(other)
        if self.sign == other.sign:
            return self.add_like_signs(other)
        return self.add_unlike_signs(other)

    def __sub__(self, other):
        if not isinstance(other, BigInt):
            z = BigInt(other)
        else:
            z = copy(other)
        z.sign = not z.sign
        return self + z

    def __mul__(self, other):
        if not isinstance(other, BigInt):
            other = BigInt(other)

        if len(self.digits) < len(other.digits):
            return other.__mul__(self)

        # We now multiple the digits of self by the digits of other.
        #
        #     128
        #    * 12
        #   ------
        #
        #     128
        #    *  2
        #   ------
        #       8
        #       2
        #       -
        #      16
        #      2|
        #      2|
        #      -|
        #      46
        # carry1
        #      56
        #     1||
        #     2||
        #     -||
        #     256
        #
        # Hmm...

        acc = BigInt()
        for i, digit in enumerate(other.digits):
            acc = acc + self._mul_one_digit(i, digit)
        acc.sign = not (self.sign ^ other.sign)
        return acc

    def _mul_one_digit(self, power, n):
        # Some of this should go in a method of OberonInt?
        digits = [zero] * power
        carry = zero
        for digit in self.digits:
            high, low = digit * n
            c, p = low + carry
            digits.append(p)
            carry = high
            if c:
                z, carry = carry + one
                assert not z, repr(z)
        if carry.value:
            assert carry.value > 0
            digits.append(carry)
        result = BigInt()
        result.digits = digits
        return result

    def add_like_signs(self, other):
        '''
        Add a BigInt of the same sign as self.
        '''
        assert self.sign == other.sign
        out = []
        carry = 0
        Z = zip_longest(
            self.digits,
            other.digits,
            fillvalue=zero,  # Elegant, but not efficient?
            )
        for a, b in Z:
            carry, digit = a.add_with_carry(b, carry)
            out.append(digit)
        if carry:
            out.append(one)
        result = BigInt()
        result.sign = self.sign
        result.digits = out
        return result

    def add_unlike_signs(self, other):
        '''
        Add a BigInt of unlike sign as self.
        '''
        assert self.sign != other.sign

        # So we have -a and +b
        #         or +a and -b

        a, b = (self, other) if self.sign else (other, self)

        # So now we have:
        #     a + (-b) == a - b

        # I don't know how to subtract a larger (abs) number
        #                        from a smaller (abs) one
        # However:
        #
        # a - b == -(b - a)
        #
        # I.e. 9 - 17 == -(17 - 9)

        return a._subtract_smaller(b) if a.abs_gt_abs(b) else b._subtract_smaller(a)

    def _subtract_smaller(self, other):
        out = []
        carry = 0
        Z = zip_longest(
            self.digits,
            other.digits,
            fillvalue=zero,
            )
        for a, b in Z:
            carry, digit = a.sub_with_carry(b, carry)
            out.append(digit)
        if carry:
            out.append(one)
        result = BigInt()
        result.sign = self.sign
        result.digits = out
        return result

    def abs_gt_abs(self, other):
        a_len, b_len = len(self.digits), len(other.digits)
        if a_len > b_len: return True
        if a_len < b_len: return False
        # a_len == b_len
        if not a_len:  # a == b == 0
            return False
        return self.digits[-1] > other.digits[-1]

    def __eq__(self, other):
        return self.sign == other.sign and self.digits == other.digits


def is_i32(n):
    return -2**31 <= n < 2**31


class OberonInt:
    '''
    Let's model the Oberon RISC integers,
    32-bit, two's complement.
    '''

    def __init__(self, initial=0):
        assert is_i32(initial)
        self.value = initial

    def add_with_carry(self, other, carry):
        '''
        In terms of single base-10 skool arithmetic:

        a, b in {0..9}
        carry in {0..1}

        9 + 9 + 1 = 18 + 1  = 19
        aka       = 1,(8+1) = 1, 9
        '''
        c, digit = self + other
        if carry:
            z, digit = digit + one
            assert not z, repr(z)
        return c, digit

    def sub_with_carry(self, other, carry):
        c, digit = self - other
        if carry:
            z, digit = digit - one
            assert not z, repr(z)
        return c, digit

    def __add__(self, other):
        '''
        Return carry bit and new value.
        '''
        if not isinstance(other, OberonInt):
            other = OberonInt(other)
        n = self.value + other.value
        carry = not is_i32(n)
        if carry:
            n &= 2**31 - 1
        return carry, OberonInt(n)

    __radd__ = __add__

    def negate(self):
        # Instead of binary ops, just cheat:
        return OberonInt(
            0  # Handle negation of obmin.
            if self.value == -(2**31)
            else -self.value
            )

    def __sub__(self, other):
        if not isinstance(other, OberonInt):
            other = OberonInt(other)
        return self + other.negate()

    __rsub__ = __sub__

    def __repr__(self):
        return f'OberonInt({self.value})'

    def __eq__(self, other):
        assert isinstance(other, OberonInt)
        return self.value == other.value

    def __gt__(self, other):
        assert isinstance(other, OberonInt)
        return self.value > other.value

    def __mul__(self, other):
        assert isinstance(other, OberonInt)
        product = self.value * other.value
        high = OberonInt(product >> 31)
        low = OberonInt(product & (2**31 - 1))
        return high, low

##            # I think we want to put the 32nd bit of product
##            # into the first bit of H, left-shifting H by one first.
##            c = (H << 1) & (product >> 31)  # What about H[32]?
##            product &= 0x7fffffff  # Zero out that 32nd bit.
##
##            if carry:
##                digit += one


        ##    >>> n = obmax.value
        ##    >>> n*n
        ##    4611686014132420609
        ##    >>> bin(n*n)
        ##    '0b11111111111111111111111111111100000000000000000000000000000001'
        ##    >>> bin(n)
        ##    '0b1111111111111111111111111111111'
        ##    >>> bin(0b1111111111111111111111111111111 * 0b1111111111111111111111111111111)
        ##    '0b11111111111111111111111111111100000000000000000000000000000001'

        ##    >>> '0b00_111111 11111111 11111111 11111111|00000000 00000000 00000000 00000001'
        # So we can see that multiplying obmax by itself leave two empty bits in the top half
        # If we perform the above c = (H << 1) & (product >> 31) we get:
        # c = 0b0_1111111 11111111 11111111 11111110
        # p = 0b_00000000 00000000 00000000 00000001'

obmin, zero, one, obmax = map(OberonInt, (
    -(2**31),
    0,
    1,
    2**31-1,
    ))


class OberonIntTest(unittest.TestCase):

    def test_Addition(self):
        carry, z = obmax + one
        self.assertTrue(carry)
        self.assertEqual(z, zero)

    def test_Negation(self):
        negative_one = one.negate()
        carry, m = obmin + negative_one
        self.assertTrue(carry)
        self.assertEqual(m, obmax)

    def test_Subtraction(self):
        # Ergo, subtraction.
        carry, m = obmin - one
        self.assertTrue(carry)
        self.assertEqual(m, obmax)

    def test_twice_max(self):
        carry, hmm = obmax + obmax
        self.assertTrue(carry)
        self.assertEqual(hmm.value, 2**31 - 2)

        carry, eh = obmax - hmm
        self.assertFalse(carry)
        self.assertEqual(eh, one)
        self.assertEqual( (hmm + one)[1] , obmax )

    def test_twice_min(self):
        carry, n = obmin + obmin
        self.assertTrue(carry)
        self.assertEqual(n, zero)

    def test_mul(self):
        h, l = obmax * obmax
        B = BigInt(obmax.value * obmax.value)
        self.assertEqual([l, h], B.digits)


N = 100
rand = lambda: randint(0, 10**N) - (10**N)//2
# For some reason randint(-(10**100), 10**100) wasn't returning negative numbers.
# Above my pay grade.  I don't even know if that's a bug,
# there are a /lot/ of numbers up around ten-to-the-hundreth-power, eh?


class BigIntTest(unittest.TestCase):

    def test_to_int(self):
        n = 12345678901234567898090123445678990
        x = BigInt(n).to_int()
        self.assertEqual(n, x)

    def test_2_to_100th_power(self):
        digits = list(BigInt.digitize(2**100))
        self.assertEqual(
            digits,
            [OberonInt(0), OberonInt(0), OberonInt(0), OberonInt(128)],
            )

    def test_Addition(self):
        n = 12345678901234567898090123445678990
        m = 901234567898090
        self._test_add(n, m)

    def test_Addition_of_two_negatives(self):
        n = -12345678901234567898090123445678990
        m = -901234567898090
        self._test_add(n, m)

    def test_Addition_of_unlike_signs(self):
        n = 12345678901234567898090123445678990
        m = -901234567898090
        self._test_add(n, m)

    def _test_invert(self):
        n = 7 * (2**16)
        x = BigInt(n)
        y = x.negate()
        print()
        print(y.to_int(), bin(y.to_int()), y.digits)
        print(x.to_int(), bin(x.to_int()), x.digits)
        print()
        print(x + y)

    def test_Subtraction_small_from_large(self):
        n = 12345678901234567898090123445678990
        m = 901234567898090
        self._test_sub(n, m)

    def test_Subtraction_large_from_small(self):
        n = 901234567898090
        m = 12345678901234567898090123445678990
        self._test_sub(n, m)

    def test_Subtraction_neg_small_from_large(self):
        n = 12345678901234567898090123445678990
        m = -901234567898090
        self._test_sub(n, m)

    def test_Subtraction_neg_large_from_small(self):
        n = 901234567898090
        m = -12345678901234567898090123445678990
        self._test_sub(n, m)

    def test_Subtraction_small_from_neg_large(self):
        n = -12345678901234567898090123445678990
        m = 901234567898090
        self._test_sub(n, m)

    def test_Subtraction_large_from_neg_small(self):
        n = -901234567898090
        m = 12345678901234567898090123445678990
        self._test_sub(n, m)

    def test_Subtraction_neg_small_from_neg_large(self):
        n = -12345678901234567898090123445678990
        m = -901234567898090
        self._test_sub(n, m)

    def test_Subtraction_neg_large_from_neg_small(self):
        n = -901234567898090
        m = -12345678901234567898090123445678990
        self._test_sub(n, m)

    def _test_add(self, n, m):
        x = BigInt(n)
        y = BigInt(m)
        z = x + y
        t = z.to_int()
        self.assertEqual(t, n + m, f'{x} + {y}')

    def _test_sub(self, n, m):
        x = BigInt(n)
        y = BigInt(m)
        z = x - y
        t = z.to_int()
        self.assertEqual(t, n - m, f'{x} - {y}')

    def _test_mul(self, n, m):
        x = BigInt(n)
        y = BigInt(m)
        z = x * y
        t = z.to_int()
        self.assertEqual(t, n * m, f'{x} * {y}')

    def test_mul(self):
        a = 2063400293
        b = -1483898257
        self._test_mul(a, b)

    def test_random_add_sub(self):
        for _ in range(100):
            a = rand()
            b = rand()
            #print(a, b)
            self._test_add(a, b)
            self._test_sub(a, b)
            self._test_mul(a, b)


if __name__ == '__main__':
    unittest.main()
