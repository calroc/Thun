import ctypes


def is_i32(n):
    return -2**31 <= n < 2**31


class OberonInt:
    '''
    Let's model the Oberon RISC integers,
    32-bit, two's complement.
    '''

    def __init__(self, initial=0):
        assert is_i32(initial)
        self.value = ctypes.c_int32(initial)
        assert self.value.value == initial

    def __add__(self, other):
        '''
        Return carry bit and new value.
        '''
        assert isinstance(other, OberonInt)
        n = self.value.value + other.value.value
        carry = not is_i32(n)
        if carry:
            n &= (2**31-1)
        return int(carry), OberonInt(n)

    def negate(self):
        # Instead of binary ops, just cheat:
        return OberonInt(-self.value.value)

    def __sub__(self, other):
        assert isinstance(other, OberonInt)
        return self + other.negate()

    def __repr__(self):
        #b = bin(self.value.value & (2**32-1))
        return f'OberonInt({self.value.value})'

    def __eq__(self, other):
        assert isinstance(other, OberonInt)
        return self.value.value == other.value.value


obmin, zero, one, obmax = map(OberonInt, (
    -(2**31),
    0,
    1,
    2**31-1,
    ))


# Addition
carry, z = obmax + one
assert carry
assert z == zero

# Negation
negative_one = one.negate()
carry, m = obmin + negative_one
assert carry
assert m == obmax

# Ergo, subtraction.
carry, m = obmin - one
assert carry
assert m == obmax


carry, hmm = obmax + obmax
assert carry
assert hmm.value.value == 2**31 - 2
carry, eh = obmax - hmm
assert not carry
assert eh == one
assert (hmm + one)[1] == obmax


##        if initial >= 2**31:
##            raise ValueError(f'too big: {initial!r}')
##        if initial < -2**31:
##            raise ValueError(f'too small: {initial!r}')
