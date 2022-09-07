from itertools import chain
from joy.utils.stack import _s, iter_stack


class Expression:
    '''
    As elegant as it is to model the expression as a stack, it's not very
    efficient, as concatenating definitions and other quoted programs to
    the expression is a common and expensive operation.

    Instead, let's keep a stack of sub-expressions, reading from them
    one-by-one, and prepending new sub-expressions to the stack rather than
    concatenating them.
    '''

    def __init__(self, initial_expression=()):
        self.current = initial_expression
        self.stack = []

    def __iter__(self):
        return self

    def __next__(self):
        if self.current: (item, self.current) = self.current
        elif self.stack: (item, self.current) = self.stack.pop()
        else: raise StopIteration
        return item

    def prepend(self, quoted_program):
        if not quoted_program: return
        if self.current: self.stack.append(self.current)
        self.current = quoted_program

    def __bool__(self):
        return bool(self.current or self.stack)

    def __str__(self):
        return ' '.join(
            map(
                _s,
                chain.from_iterable(
                    map(
                        iter_stack,
                        reversed(self.stack + [self.current])
                        )
                    )
                )
            )


class E(Expression):

    def __iter__(self):
        return iter((self.__next__(), self))


if __name__ == '__main__':
    from joy.parser import text_to_expression as j

    e = Expression(j('23 18'))
    e.prepend(j('88 19'))
    e.prepend(j('foo fie feum'))
    print(e)
    for i in e:
        print(i, e.stack, e.current)
        if i == 88:
            print('prepending "hello world"')
            e.prepend(j('hello world'))
        if i == 19:
            print('prepending "good bye"')
            e.prepend(j('good bye'))
    print('-'*20)
    e = E(j('23 18'))
    e.prepend(j('88 19'))
    e.prepend(j('foo fie feum'))
    print(e)
    while e:
        i, e = e
        print(i, e.stack, e.current)
        if i == 88:
            print('prepending "hello world"')
            e.prepend(j('hello world'))
        if i == 19:
            print('prepending "good bye"')
            e.prepend(j('good bye'))

