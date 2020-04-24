from builtins import str
from joy.parser import Symbol


def _names():
	n = 0
	while True:
		yield Symbol('a' + str(n))
		n += 1


class InfiniteStack(tuple):

	names = lambda n=_names(): next(n)

	def __iter__(self):
		if not self:
			return iter((self.names(), self))


i = InfiniteStack()

a, b = i

lambda u: (lambda fu, u: fu * fu * u)(
	(lambda u: (lambda fu, u: fu * fu)(
		(lambda u: (lambda fu, u: fu * fu * u)(
			(lambda u: 1)(u), u))(u), u))(u),
	u)

lambda u: (lambda fu, u: fu * fu * u)((lambda u: (lambda fu, u: fu * fu)((lambda u: (lambda fu, u: fu * fu * u)((lambda u: 1)(u), u))(u), u))(u), u)
