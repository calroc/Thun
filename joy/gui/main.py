#!/usr/bin/env python
# -*- coding: utf-8 -*-
('''\
Joypy - Copyright © 2018 Simon Forman
'''
'This program comes with ABSOLUTELY NO WARRANTY; for details right-click "warranty".'
' This is free software, and you are welcome to redistribute it under certain conditions;'
' right-click "sharing" for details.'
' Right-click on these commands to see docs on UI commands: key_bindings mouse_bindings')
import os, pickle, sys
from textwrap import dedent

from joy.gui.textwidget import TextViewerWidget, tk, get_font, TEXT_BINDINGS
from joy.gui.utils import init_home, FileFaker
from joy.gui.world import StackDisplayWorld
from joy.library import initialize
from joy.utils.stack import stack_to_string


tb = TEXT_BINDINGS.copy()
tb.update({
  '<F3>': lambda tv: tv.copy_selection_to_stack,
  '<Shift-F3>': lambda tv: tv.cut,
  '<F4>': lambda tv: tv.copyto,
  '<Shift-F4>': lambda tv: tv.pastecut,
  })
defaults = dict(text_bindings=tb, width=80, height=25)


GLOBAL_COMMANDS = {
  '<F5>': 'swap',
  '<F6>': 'dup',
  '<Shift-F5>': 'roll<',
  '<Shift-F6>': 'roll>',
  '<F7>': 'over',
  '<Shift-F7>': 'tuck',
  '<F8>': 'parse',
  '<F12>': 'words',
  '<F1>': 'reset_log show_log',
  '<Escape>': 'clear reset_log show_log',
  '<Control-Delete>': 'pop',
  }


def repo_relative_path(path):
  return os.path.relpath(
    path,
    os.path.commonprefix((repo.controldir(), path))
    )


def key_bindings(*args):
  commands = [
    'Control-Enter - Run the selection as Joy code.',
    'F3 - Copy selection to stack.',
    'Shift-F3 - Cut selection to stack.',
    'F4 - Paste item on top of stack to insertion cursor.',
    'Shift-F4 - Pop and paste top of stack to insertion cursor.',
    ]
  for key, command in GLOBAL_COMMANDS.iteritems():
    commands.append('%s - %s' % (key.strip('<>'), command))
  print '\n'.join([''] + sorted(commands))
  return args


def mouse_bindings(*args):
  print dedent('''
    Mouse button chords (to cancel a chord, click the third mouse button.)

    Left - Point, sweep selection
    Left-Middle - Copy the selection, place text on stack
    Left-Right - Run the selection as Joy code

    Middle - Paste selection (bypass stack); click and drag to scroll.
    Middle-Left - Paste from top of stack, preserve
    Middle-Right - Paste from top of stack, pop

    Right - Execute command word under mouse cursor
    Right-Left - Print docs of command word under mouse cursor
    Right-Middle - Lookup word (kinda useless now)
    ''')
  return args


def reset_log(*args):
  log.delete('0.0', tk.END)
  print __doc__
  return args


def show_log(*args):
  log_window.wm_deiconify()
  log_window.update()
  return args


def grand_reset(s, e, d):
  stack = world.load_stack() or ()
  log.reset()
  t.reset()
  return stack, e, d


JOY_HOME, repo = init_home()
STACK_FN = os.path.join(JOY_HOME, 'stack.pickle')
REL_STACK_FN = repo_relative_path(STACK_FN)
JOY_FN = os.path.join(JOY_HOME, 'scratch.txt')
LOG_FN = os.path.join(JOY_HOME, 'log.txt')
D = initialize()
for func in (
  reset_log,
  show_log,
  grand_reset,
  key_bindings,
  mouse_bindings,
  ):
  D[func.__name__] = func
world = StackDisplayWorld(repo, STACK_FN, REL_STACK_FN, dictionary=D)
t = TextViewerWidget(world, **defaults)
log_window = tk.Toplevel()
log_window.protocol("WM_DELETE_WINDOW", log_window.withdraw)
log = TextViewerWidget(world, log_window, **defaults)
FONT = get_font('Iosevka', size=14)  # Requires Tk root already set up.
log.init('Log', LOG_FN, repo_relative_path(LOG_FN), repo, FONT)
t.init('Joy - ' + JOY_HOME, JOY_FN, repo_relative_path(JOY_FN), repo, FONT)
for event, command in GLOBAL_COMMANDS.items():
  t.bind(event, lambda _, _command=command: world.interpret(_command))
  log.bind(event, lambda _, _command=command: world.interpret(_command))


def main():
  sys.stdout, old_stdout = FileFaker(log), sys.stdout
  try:
    t.mainloop()
  finally:
    sys.stdout = old_stdout
  return 0


if __name__ == '__main__':
  main()
