
def abs(stack, expression, dictionary):
    (v1, stack) = stack
    v2 = abs(v1)
    return (v2, stack), expression, dictionary


def ccons(stack, expression, dictionary):
    (s1, (v1, (v2, stack))) = stack
    return ((v2, (v1, s1)), stack), expression, dictionary


def cons(stack, expression, dictionary):
    (s1, (v1, stack)) = stack
    return ((v1, s1), stack), expression, dictionary


def decr(stack, expression, dictionary):
    (i1, stack) = stack
    i2 = i1 - 1
    return (i2, stack), expression, dictionary


def dup(stack, expression, dictionary):
    (v1, stack) = stack
    return (v1, (v1, stack)), expression, dictionary


def dupd(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    return (v1, (v2, (v2, stack))), expression, dictionary


def dupdd(stack, expression, dictionary):
    (v1, (v2, (v3, stack))) = stack
    return (v1, (v2, (v3, (v3, stack)))), expression, dictionary


def first(stack, expression, dictionary):
    ((v1, s1), stack) = stack
    return (v1, stack), expression, dictionary


def fourth(stack, expression, dictionary):
    ((v1, (v2, (v3, (v4, s1)))), stack) = stack
    return (v4, stack), expression, dictionary


def incr(stack, expression, dictionary):
    (i1, stack) = stack
    i2 = i1 + 1
    return (i2, stack), expression, dictionary


def non_negative(stack, expression, dictionary):
    (i1, stack) = stack
    v1 = i1 >= 0
    return (v1, stack), expression, dictionary


def pop(stack, expression, dictionary):
    (v1, stack) = stack
    return stack, expression, dictionary


def popd(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    return (v1, stack), expression, dictionary


def popop(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    return stack, expression, dictionary


def popopd(stack, expression, dictionary):
    (v1, (v2, (v3, stack))) = stack
    return (v1, stack), expression, dictionary


def quoted(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    return (v1, ((v2, ()), stack)), expression, dictionary


def reco(stack, expression, dictionary):
    ((v1, s1), (v2, stack)) = stack
    return ((v2, s1), stack), expression, dictionary


def rest(stack, expression, dictionary):
    ((v1, s1), stack) = stack
    return (s1, stack), expression, dictionary


def rrest(stack, expression, dictionary):
    ((v1, (v2, s1)), stack) = stack
    return (s1, stack), expression, dictionary


def second(stack, expression, dictionary):
    ((v1, (v2, s1)), stack) = stack
    return (v2, stack), expression, dictionary


def shift(stack, expression, dictionary):
    ((v1, s1), (s2, stack)) = stack
    return (s1, ((v1, s2), stack)), expression, dictionary


def sqr(stack, expression, dictionary):
    (i1, stack) = stack
    i2 = i1 * i1
    return (i2, stack), expression, dictionary


def stackd(stack, expression, dictionary):
    (v1, stack) = stack
    stack = (stack, stack)
    return (v1, stack), expression, dictionary


def swons(stack, expression, dictionary):
    (v1, (s1, stack)) = stack
    return ((v1, s1), stack), expression, dictionary


def third(stack, expression, dictionary):
    ((v1, (v2, (v3, s1))), stack) = stack
    return (v3, stack), expression, dictionary


def truthy(stack, expression, dictionary):
    (v1, stack) = stack
    v2 = bool(v1)
    return (v2, (v1, stack)), expression, dictionary


def tuckl(stack, expression, dictionary):
    (v1, stack) = stack
    return (v1, ((), stack)), expression, dictionary


def tuckld(stack, expression, dictionary):
    (v1, (v2, stack)) = stack
    return (v1, (v2, ((), stack))), expression, dictionary


def uncons(stack, expression, dictionary):
    ((v1, s1), stack) = stack
    return (s1, (v1, stack)), expression, dictionary


def unit(stack, expression, dictionary):
    (v1, stack) = stack
    return ((v1, ()), stack), expression, dictionary


def unswons(stack, expression, dictionary):
    ((v1, s1), stack) = stack
    return (v1, (s1, stack)), expression, dictionary


def gcd(stack, expression, dictionary):
    (i1, (i2, stack)) = stack
    v1 = True
    while v1:
        i3 = i2 % i1
        v1 = i3 > 0
        (i1, (i2, stack)) = (i3, (i1, stack))
    return (i2, stack), expression, dictionary


def sum(stack, expression, dictionary):
    (s1, stack) = stack
    (i1, stack) = (0, stack)
    while s1:
        (i2, s1) = s1
        i3 = i1 + i2
        (i1, stack) = (i3, stack)
    return (i1, stack), expression, dictionary


def product(stack, expression, dictionary):
    (s1, stack) = stack
    (i1, stack) = (1, stack)
    while s1:
        (i2, s1) = s1
        i3 = i1 * i2
        (i1, stack) = (i3, stack)
    return (i1, stack), expression, dictionary


1 ?- 

