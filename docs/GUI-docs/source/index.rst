.. Xerblin documentation master file, created by
   sphinx-quickstart on Mon May 25 20:33:57 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Xerblin Documentation
===================================

A Graphical User Interface for `Thun <https://joypy.osdn.io>`_.

History
----------------------------

A long ago I found a copy of `"System Design from Provably Correct Constructs" <https://archive.org/details/systemdesignfrom00mart>`_
at the Seattle public library. I didn't realize it at the time but I now
know that it is basically a presentation of Dr. Margaret
Hamilton's Higher-Order Software (HOS).

Dr. Hamilton coined the term "software engineering" while developing the
software for Apollo 11.  She went on to design a system that permitted
bug-free software development and
attempted to market it.

In essence it's a thin AST that is only modified by operations that
preserve certain kinds of correctness (i.e. type safety), with a tiny
core of essential combinators that are combined (in "provably correct"
ways) to form control-flow constructs. I was struck by the essential
simplicity and spent time on and off trying to get a implementation
working. (It never amounted to much but it DID get me my first job as a
programmer!)





Edsger Dijkstra panned it: `EWD852: Judging "HOS" from a distance <https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD852.html>`_.
I don't think he was right, but I include a link to it here for
completeness.


It is described in `Universal Systems Language <https://en.wikipedia.org/wiki/Universal_Systems_Language>`_.


Eventually, I found Manfred von Thun's Joy language and realized that it was better than my thing and now I've implemented that in Python in Continuation-Passing Style.

Structure
----------------------------

This simple GUI takes innovations from
`the Oberon OS <http://www.projectoberon.com>`_
and
`Jef Raskin's "Humane Interface". <https://archive.org/details/humaneinterfacen00rask>`_
There are no separate "apps" or programs, only one
body of commands that can be "scripted" by the user.  Mouse interaction
and CLI are unified.  For example, you can click on commands, and then
make a "script" (a new command) out of the commands after the fact.

All files are kept in a VCS including the current system state, and
autosaved after every change (so you can literally kick out the plug,
plug it back in, and keep going from where you left off.) Never lose
work.


Commands
----------------------------

Mouse Chords
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Mouse button chords (to cancel a chord, click all three mouse buttons.)

+--------+--------+------------------------------------------------+
| First  | Second | Action                                         |
+========+========+================================================+
| Left   |        | Point, sweep selection.                        |
+--------+--------+------------------------------------------------+
| Left   | Middle | Copy selection to stack.                       |
+--------+--------+------------------------------------------------+
| Left   | Right  | Run the selection as a Joy expression.         |
+--------+--------+------------------------------------------------+
| Middle |        | Paste selection (bypassing stack).             |
|        |        | Click and drag to scroll.                      |
+--------+--------+------------------------------------------------+
| Middle | Left   | Paste copy as text from top of stack.          |
+--------+--------+------------------------------------------------+
| Middle | Right  | Pop from top of stack, paste as test.          |
+--------+--------+------------------------------------------------+
| Right  |        | Execute command.                               |
+--------+--------+------------------------------------------------+
| Right  | Left   | Print docs of command.                         |
+--------+--------+------------------------------------------------+
| Right  | Middle | Lookup word (kinda useless now)                |
+--------+--------+------------------------------------------------+


Keyboard
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- ``Control-Enter`` - Run the selection as Joy code, or if there's no selection the line containing the cursor.
- ``F3`` - Copy selection to stack.
- ``Shift-F3`` - Cut selection to stack.
- ``F4`` - Paste item on top of stack to insertion cursor.
- ``Shift-F4`` - Pop and paste top of stack to insertion cursor.

There are some commands bound to keys in the config file:

- ``F5`` - ``swap``
- ``F6`` - ``dup``
- ``Shift-F5`` - ``roll<``
- ``Shift-F6`` - ``roll>``
- ``F7`` - ``over``
- ``Shift-F7`` - ``tuck``
- ``F8`` - ``parse``
- ``F12`` - ``words``
- ``F1`` - ``reset_log show_log``
- ``Escape`` - ``clear reset_log show_log``
- ``Control-Delete`` - ``pop``
- ``Control-i`` - ``i``

You can edit the ``thun.config`` file to change these.


Modules
----------------------------

.. toctree::
   :maxdepth: 2

   world
   textwidget


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
