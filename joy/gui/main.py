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
from joy.gui.world import World
from joy.library import initialize
from joy.utils.stack import stack_to_string


JOY_HOME, repo = init_home()


def repo_relative_path(path):
  return os.path.relpath(
    path,
    os.path.commonprefix((repo.controldir(), path))
    )


STACK_FN = os.path.join(JOY_HOME, 'stack.pickle')
JOY_FN = os.path.join(JOY_HOME, 'scratch.txt')
LOG_FN = os.path.join(JOY_HOME, 'log.txt')


class StackDisplayWorld(World):

  relative_STACK_FN = repo_relative_path(STACK_FN)

  def interpret(self, command):
    print '\njoy?', command
    super(StackDisplayWorld, self).interpret(command)

  def print_stack(self):
    print '\n%s <-' % stack_to_string(self.stack)

  def save(self):
    with open(STACK_FN, 'wb') as f:
      os.chmod(STACK_FN, 0600)
      pickle.dump(self.stack, f)
      f.flush()from dulwich.errors import NotGitRepository
from dulwich.repo import Repo


      os.fsync(f.fileno())
    repo.stage([self.relative_STACK_FN])
    commit_id = repo.do_commit(
      'message',
      committer='Simon Forman <forman.simon@gmail.com>',
      )
    print >> sys.stderr, commit_id


def key_bindings(*args):
  print dedent('''
    Ctrl-Enter - Run the selection as Joy code.

    F1 - Reset and show (if hidden) the log.
    Esc - Like F1 but also clears the stack.
    ...
    F12 - print a list of all command words, or right-click "words".
    ''')
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
  stack = load_stack() or ()
  reset_text(log, LOG_FN)
  reset_text(t, JOY_FN)
  return stack, e, d


def reset_text(t, filename):
  if os.path.exists(filename):
    with open(filename) as f:
      data = f.read()
    if data:
      t.delete('0.0', tk.END)
      t.insert(tk.END, data)


def load_stack():
  if os.path.exists(STACK_FN):
    with open(STACK_FN) as f:
      return pickle.load(f)


tb = TEXT_BINDINGS.copy()
tb.update({
  '<F4>': lambda tv: tv.cut,
  '<F3>': lambda tv: tv.copy_selection_to_stack,
#  '<F-->': lambda tv: tv.pastecut,
  '<F6>': lambda tv: tv.copyto,
  })
defaults = dict(text_bindings=tb, width=80, height=25)


D = initialize()
for func in (
  reset_log,
  show_log,
  grand_reset,
  key_bindings,
  mouse_bindings,
  ):
  D[func.__name__] = func
stack = load_stack()
if stack is None:
  world = StackDisplayWorld(dictionary=D)
else:
  world = StackDisplayWorld(stack=stack, dictionary=D)
t = TextViewerWidget(world, **defaults)
log_window = tk.Toplevel()
log_window.protocol("WM_DELETE_WINDOW", log_window.withdraw)
log = TextViewerWidget(world, log_window, **defaults)
FONT = get_font('Iosevka', size=14)  # Requires Tk root already set up.
log.init('Log', LOG_FN, repo_relative_path(LOG_FN), repo, FONT)
t.init('Joy - ' + JOY_HOME, JOY_FN, repo_relative_path(JOY_FN), repo, FONT)


GLOBAL_COMMANDS = {
  '<F5>': 'swap',
  '<F6>': 'dup',

  '<Shift-F5>': 'roll<',
  '<Shift-F6>': 'roll>',

  '<F7>': 'over',
  '<Shift-F7>': 'tuck',

  '<Shift-F3>': 'parse',

  '<F12>': 'words',
  '<F1>': 'reset_log show_log',
  '<Escape>': 'clear reset_log show_log',
  '<Control-Delete>': 'pop',
  '<Control-Shift-Delete>': 'popd',
  }
for event, command in GLOBAL_COMMANDS.items():
  t.bind_all(event, lambda _, _command=command: world.interpret(_command))


def main():
  sys.stdout, old_stdout = FileFaker(log), sys.stdout
  try:
    t.mainloop()
  finally:
    sys.stdout = old_stdout
  return 0


if __name__ == '__main__':
  main()
