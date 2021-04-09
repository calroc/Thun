# GENERATED FILE. DO NOT EDIT.
# The code that generated these functions is in the repo history
# at the v0.4.0 tag.
from .errors import NotAListError, StackUnderflowError


def _Tree_add_Ee(stack):
  """
  ::

    ([a4 a5 ...1] a3 a2 a1 -- [a2 a3 ...1])

  """
  (a1, (a2, (a3, ((a4, (a5, s1)), s2)))) = stack
  return ((a2, (a3, s1)), s2)


def _Tree_delete_R0(stack):
  """
  ::

    ([a2 ...1] a1 -- [a2 ...1] a2 a1 a1)

  """
  (a1, ((a2, s1), s2)) = stack
  return (a1, (a1, (a2, ((a2, s1), s2))))


def _Tree_delete_clear_stuff(stack):
  """
  ::

    (a3 a2 [a1 ...1] -- [...1])

  """
  ((a1, s1), (a2, (a3, s2))) = stack
  return (s1, s2)


def _Tree_get_E(stack):
  """
  ::

    ([a3 a4 ...1] a2 a1 -- a4)

  """
  (a1, (a2, ((a3, (a4, s1)), s2))) = stack
  return (a4, s2)


def ccons(stack):
  """
  ::

    (a2 a1 [...1] -- [a2 a1 ...1])

  """
  (s1, (a1, (a2, s2))) = stack
  return ((a2, (a1, s1)), s2)


def cons(stack):
  """
  ::

    (a1 [...0] -- [a1 ...0])

  """
  try: s0, stack = stack
  except ValueError: raise StackUnderflowError
  if not isinstance(s0, tuple): raise NotAListError
  try: a1, s23 = stack
  except ValueError: raise StackUnderflowError
  return ((a1, s0), s23)


def dup(stack):
  """
  ::

    (a1 -- a1 a1)

  """
  (a1, s23) = stack
  return (a1, (a1, s23))


def dupd(stack):
  """
  ::

    (a2 a1 -- a2 a2 a1)

  """
  (a1, (a2, s23)) = stack
  return (a1, (a2, (a2, s23)))


def dupdd(stack):
  """
  ::

    (a3 a2 a1 -- a3 a3 a2 a1)

  """
  (a1, (a2, (a3, s23))) = stack
  return (a1, (a2, (a3, (a3, s23))))


def first(stack):
  """
  ::

    ([a1 ...1] -- a1)

  """
  ((a1, s1), s23) = stack
  return (a1, s23)


def first_two(stack):
  """
  ::

    ([a1 a2 ...1] -- a1 a2)

  """
  ((a1, (a2, s1)), s2) = stack
  return (a2, (a1, s2))


def fourth(stack):
  """
  ::

    ([a1 a2 a3 a4 ...1] -- a4)

  """
  ((a1, (a2, (a3, (a4, s1)))), s2) = stack
  return (a4, s2)


def over(stack):
  """
  ::

    (a2 a1 -- a2 a1 a2)

  """
  (a1, (a2, s23)) = stack
  return (a2, (a1, (a2, s23)))


def pop(stack):
  """
  ::

    (a1 --)

  """
  (a1, s23) = stack
  return s23


def popd(stack):
  """
  ::

    (a2 a1 -- a1)

  """
  (a1, (a2, s23)) = stack
  return (a1, s23)


def popdd(stack):
  """
  ::

    (a3 a2 a1 -- a2 a1)

  """
  (a1, (a2, (a3, s23))) = stack
  return (a1, (a2, s23))


def popop(stack):
  """
  ::

    (a2 a1 --)

  """
  (a1, (a2, s23)) = stack
  return s23


def popopd(stack):
  """
  ::

    (a3 a2 a1 -- a1)

  """
  (a1, (a2, (a3, s23))) = stack
  return (a1, s23)


def popopdd(stack):
  """
  ::

    (a4 a3 a2 a1 -- a2 a1)

  """
  (a1, (a2, (a3, (a4, s23)))) = stack
  return (a1, (a2, s23))


def rest(stack):
  """
  ::

    ([a1 ...0] -- [...0])

  """
  ((a1, s0), s23) = stack
  return (s0, s23)


def rolldown(stack):
  """
  ::

    (a1 a2 a3 -- a2 a3 a1)

  """
  (a3, (a2, (a1, s23))) = stack
  return (a1, (a3, (a2, s23)))


def rollup(stack):
  """
  ::

    (a1 a2 a3 -- a3 a1 a2)

  """
  (a3, (a2, (a1, s23))) = stack
  return (a2, (a1, (a3, s23)))


def rrest(stack):
  """
  ::

    ([a1 a2 ...1] -- [...1])

  """
  ((a1, (a2, s1)), s2) = stack
  return (s1, s2)


def second(stack):
  """
  ::

    ([a1 a2 ...1] -- a2)

  """
  ((a1, (a2, s1)), s2) = stack
  return (a2, s2)


def stack(stack):
  """
  ::

    (... -- ... [...])

  """
  s0 = stack
  return (s0, s0)


def stuncons(stack):
  """
  ::

    (... a1 -- ... a1 a1 [...])

  """
  (a1, s1) = stack
  return (s1, (a1, (a1, s1)))


def stununcons(stack):
  """
  ::

    (... a2 a1 -- ... a2 a1 a1 a2 [...])

  """
  (a1, (a2, s1)) = stack
  return (s1, (a2, (a1, (a1, (a2, s1)))))


def swaack(stack):
  """
  ::

    ([...1] -- [...0])

  """
  (s1, s0) = stack
  return (s0, s1)


def swap(stack):
  """
  ::

    (a1 a2 -- a2 a1)

  """
  (a2, (a1, s23)) = stack
  return (a1, (a2, s23))


def swons(stack):
  """
  ::

    ([...1] a1 -- [a1 ...1])

  """
  (a1, (s1, s2)) = stack
  return ((a1, s1), s2)


def third(stack):
  """
  ::

    ([a1 a2 a3 ...1] -- a3)

  """
  ((a1, (a2, (a3, s1))), s2) = stack
  return (a3, s2)


def tuck(stack):
  """
  ::

    (a2 a1 -- a1 a2 a1)

  """
  (a1, (a2, s23)) = stack
  return (a1, (a2, (a1, s23)))


def uncons(stack):
  """
  ::

    ([a1 ...0] -- a1 [...0])

  """
  ((a1, s0), s23) = stack
  return (s0, (a1, s23))


def unit(stack):
  """
  ::

    (a1 -- [a1 ])

  """
  (a1, s23) = stack
  return ((a1, ()), s23)


def unswons(stack):
  """
  ::

    ([a1 ...1] -- [...1] a1)

  """
  ((a1, s1), s2) = stack
  return (a1, (s1, s2))

