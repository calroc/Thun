import  unittest

max_digit = 2**31 - 1

def lil_divmod(A, B):
    '''
    Return the greatest digit in 1..max_digit such that B * digit <= A
    and the remainder A - B * digit.
    '''
    assert A > 0 and B > 0
    assert A >= B
    assert B * (max_digit + 1) > A
    predicate = lambda n: B * n <= A
    digit = find_greatest(1, max_digit, predicate)
    remainder = A - digit * B
    return digit, remainder


def find_greatest(low, high, f):
    '''
    Return the highest number n: low <= n <= high for which:

        for all i in low..n f(i) is True
        -and-
        for all i in (n+1)..high f(i) is False.

    If the function f::int->bool doesn't behave like above
    then I don't know what this function will do.

    There must be some number in the range or this function will
    fail with an assertion error.  (If you turn off assertions
    it will succeed and return low in such a case.)
    '''
    assert low <= high
    if f(high):
        return high
    pivot = (low + high) >> 1
    # If there isn't a pivot between low and high that means there's only
    # two numbers it could be: low or high, and we already know it isn't
    # high from the test above so it must be low.
    if low == pivot:
        assert f(low) and not f(low + 1)
        return low
    assert low < pivot < high
    return (
        find_greatest(pivot, high - 1, f)
        if f(pivot) else
        find_greatest(low, pivot - 1, f)
        )


class find_greatest_Test(unittest.TestCase):

    def test_find_greatest(self):
        k = 23300
        a = 1
        b = 2**31-1
        f = lambda n: n <= k
        n = find_greatest(a, b, f)
        self.assertEqual(n, k)


class BigIntTest(unittest.TestCase):

    def test_to_int(self):
        a = 10*max_digit-3
        b = 123
        a = (max_digit-3) * b
        digit, remainder = lil_divmod(a, b)
        #print(f'divmod({a}, {b}) == ({digit}, {remainder})  # ? ')
        self.assertEqual((digit, remainder), divmod(a, b))


if __name__ == '__main__':
    unittest.main()
