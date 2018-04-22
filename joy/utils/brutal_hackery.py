# -*- coding: utf-8 -*-
#
#    Copyright © 2018 Simon Forman
#
#    This file is part of Thun
#
#    Thun is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    Thun is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Thun.  If not see <http://www.gnu.org/licenses/>.
#
'''
I really want tracebacks to show which function was being executed when
an error in the wrapper function happens.  In order to do that, you have
to do this (the function in this module.)

Here's what it looks like when you pass too few arguments to e.g. "mul".

    >>> from joy.library import _dictionary
    >>> m = _dictionary['*']
    >>> m((), (), {})

    Traceback (most recent call last):
      File "<pyshell#49>", line 1, in <module>
        m((), (), {})
      File "joy/library.py", line 185, in mul:inner
        (a, (b, stack)) = stack
    ValueError: need more than 0 values to unpack
    >>> 


Notice that line 185 in the library.py file is (as of this writing) in
the BinaryBuiltinWrapper's inner() function, but this hacky code has
managed to insert the name of the wrapped function ("mul") along with a
colon into the wrapper function's reported name.

Normally I would frown on this sort of mad hackery, but...  this is in
the service of ease-of-debugging!  Very valuable.  And note that all the
hideous patching is finished in the module-load-stage, it shouldn't cause
issues of its own at runtime.

The main problem I see with this is that people coming to this code later
might be mystified if they just see a traceback with a ':' in the
function name!  Hopefully they will discover this documentation.
'''


def rename_code_object(new_name):
  '''
  If you want to wrap a function in another function and have the wrapped
  function's name show up in the traceback when an exception occurs in
  the wrapper function, you must do this brutal hackery to change the
  func.__code__.co_name attribute.  Just functools.wraps() is not enough.

  See:

  https://stackoverflow.com/questions/29919804/function-decorated-using-functools-wraps-raises-typeerror-with-the-name-of-the-w

  https://stackoverflow.com/questions/29488327/changing-the-name-of-a-generator/29488561#29488561

  I'm just glad it's possible.
  '''
  def inner(func):
    name = new_name + ':' + func.__name__
    code_object = func.__code__
    return type(func)(
      type(code_object)(
          code_object.co_argcount,
          code_object.co_nlocals,
          code_object.co_stacksize,
          code_object.co_flags,
          code_object.co_code,
          code_object.co_consts,
          code_object.co_names,
          code_object.co_varnames,
          code_object.co_filename,
          name,
          code_object.co_firstlineno,
          code_object.co_lnotab,
          code_object.co_freevars,
          code_object.co_cellvars
          ),
      func.__globals__,
      name,
      func.__defaults__,
      func.__closure__
      )
  return inner
