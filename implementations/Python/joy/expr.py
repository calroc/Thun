'''
Not this one!
Use the other file!  derp
'''

class Expression:

    def __init__(self, initial_expression=()):
        self.current = initial_expression
        self.stack = []

    def __next__(self):
        if self.current:
            item, self.current = self.current
            return item
        if self.stack:
            self.current = self.stack.pop()
            return self.__next__()
        raise StopIteration

    def prepend(self, quoted_program):
        if self.current:
            self.stack.append(self.current)
        self.current = quoted_program


from parser import text_to_expression as j
e = Expression(j('23 18'))
