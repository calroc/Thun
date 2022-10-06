import  unittest


def lil_divmod(A, B):
    assert A >= B
    assert A and B
    # There is a greatest digit in 1..9 such that:
    # B * digit <= A
    # The obvious thing to do here is a bisect search,
    # if we were really just doing 1..9 we could go linear.
    # Maybe drive it by the bits in digit?
    digit = 9
    Q = digit * B
    while Q > A:
        digit = digit - 1
        if not digit:
            raise ValueError('huh?')
        Q = digit * B
    assert A >= Q
    remainder = A - Q
    assert remainder < B
    return digit, remainder


class BigIntTest(unittest.TestCase):

    def test_to_int(self):
        a = 123
        b = 45
        digit, remainder = lil_divmod(a, b)
        self.assertLessEqual(b * digit, a)
        self.assertGreater(b * (digit + 1), a)
        self.assertEqual(b * digit + remainder, a)


if __name__ == '__main__':
    unittest.main()
