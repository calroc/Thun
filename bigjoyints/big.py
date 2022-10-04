from itertools import zip_longest
from pprint import pprint as P
import  unittest


def is_i32(n):
    return -2**31 <= n < 2**31


class BigInt:

    def __init__(self, initial=0):
        # We store a sign bit (True == non-negative)
        # and a list of OberonInt, least significant digit
        # to most.
        self.sign = initial >= 0
        if not self.sign:
            initial = -initial
        self.digits = list(self.digitize(initial))  # List of OberonInt.

    @staticmethod
    def digitize(n):
        if n < 0:
            raise ValueError(f'Non-negative only: {n}')
        if not n:
            yield OberonInt(0)
            return  # Not strictly needed as the following while
                    # will not do anything for n == 0.
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

    def __add__(self, other):
        if not isinstance(other, BigInt):
            other = BigInt(other)
        if self.sign == other.sign:
            return self.add_like_signs(other)

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


class OberonInt:
    '''
    Let's model the Oberon RISC integers,
    32-bit, two's complement.
    '''

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

    def __init__(self, initial=0):
        assert is_i32(initial)
        self.value = initial

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
        return int(carry), OberonInt(n)

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
        #b = bin(self.value.value & (2**32-1))
        return f'OberonInt({self.value})'

    def __eq__(self, other):
        assert isinstance(other, OberonInt)
        return self.value == other.value


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
        x = BigInt(n)
        y = BigInt(m)
        z = x + y
        t = z.to_int()
        self.assertEqual(t, n + m)



if __name__ == '__main__':
    unittest.main()






##        if initial >= 2**31:
##            raise ValueError(f'too big: {initial!r}')
##        if initial < -2**31:
##            raise ValueError(f'too small: {initial!r}')


