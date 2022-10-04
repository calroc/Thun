import ctypes


class OberonInt:
    '''
    Let's model the Oberon RISC integers,
    32-bit, two's complement.
    '''

    def __init__(self, initial=0):
        assert -2**31 <= initial < 2**31
        self.value = ctypes.c_int32(initial)
        assert self.value.value == initial

    def add(self, other):
        '''
        Return carry bit and new value.
        '''
        assert isinstance(other, OberonInt)
        n = self.value.value + other.value.value
        carry = not (-2**31 <= n < 2**31)
        if carry:
            n &= (2**31-1)
        return carry, OberonInt(n)

    def negate(self):
        # Instead of binary ops, just cheat:
        return OberonInt(-self.value.value)

    def __repr__(self):
        #b = bin(self.value.value & (2**32-1))
        return f'OberonInt({self.value.value})'

    def __eq__(self, other):
        assert isinstance(other, OberonInt)
        return self.value.value == other.value.value


one = OberonInt(1)
obmin, zero, obmax = map(OberonInt, (
    -(2**31),
    0,
    2**31-1,
    ))


carry, z = obmax.add(one)
assert carry
assert z == zero

negative_one = one.negate()
carry, m = obmin.add(negative_one)
assert carry
assert m == obmax

##        if initial >= 2**31:
##            raise ValueError(f'too big: {initial!r}')
##        if initial < -2**31:
##            raise ValueError(f'too small: {initial!r}')
