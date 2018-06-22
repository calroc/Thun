# GENERATED FILE. DO NOT EDIT.


def _Tree_add_Ee(stack):
  """
  ::

    ([a3 a4 ...0] a2 a1 a0 -- [a1 a2 ...0])

  """
  (a0, (a1, (a2, ((a3, (a4, s0)), s1)))) = stack
  return ((a1, (a2, s0)), s1)


def _Tree_delete_R0(stack):
  """
  ::

    ([a1 ...0] a0 -- [a1 ...0] a1 a0 a0)

  """
  (a0, ((a1, s0), s1)) = stack
  return (a0, (a0, (a1, ((a1, s0), s1))))


def _Tree_delete_clear_stuff(stack):
  """
  ::

    (a2 a1 [a0 ...0] -- [...0])

  """
  ((a0, s0), (a1, (a2, s1))) = stack
  return (s0, s1)


def _Tree_get_E(stack):
  """
  ::

    ([a2 a3 ...0] a1 a0 -- a3)

  """
  (a0, (a1, ((a2, (a3, s0)), s1))) = stack
  return (a3, s1)


def ccons(stack):
  """
  ::

    (a1 a0 [...0] -- [a1 a0 ...0])

  """
  (s0, (a0, (a1, s1))) = stack
  return ((a1, (a0, s0)), s1)


def cons(stack):
  """
  ::

    (a1 [...0] -- [a1 ...0])

  """
  (s0, (a1, s23)) = stack
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

    ([a0 a1 ...0] -- a0 a1)

  """
  ((a0, (a1, s0)), s1) = stack
  return (a1, (a0, s1))


def fourth(stack):
  """
  ::

    ([a0 a1 a2 a3 ...0] -- a3)

  """
  ((a0, (a1, (a2, (a3, s0)))), s1) = stack
  return (a3, s1)


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

    ([a0 a1 ...0] -- [...0])

  """
  ((a0, (a1, s0)), s1) = stack
  return (s0, s1)


def second(stack):
  """
  ::

    ([a0 a1 ...0] -- a1)

  """
  ((a0, (a1, s0)), s1) = stack
  return (a1, s1)


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

    (... a0 -- ... a0 a0 [...])

  """
  (a0, s0) = stack
  return (s0, (a0, (a0, s0)))


def stununcons(stack):
  """
  ::

    (... a1 a0 -- ... a1 a0 a0 a1 [...])

  """
  (a0, (a1, s0)) = stack
  return (s0, (a1, (a0, (a0, (a1, s0)))))


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

    ([...0] a0 -- [a0 ...0])

  """
  (a0, (s0, s1)) = stack
  return ((a0, s0), s1)


def third(stack):
  """
  ::

    ([a0 a1 a2 ...0] -- a2)

  """
  ((a0, (a1, (a2, s0))), s1) = stack
  return (a2, s1)


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


def unswons(stack):
  """
  ::

    ([a0 ...0] -- [...0] a0)

  """
  ((a0, s0), s1) = stack
  return (a0, (s0, s1))

