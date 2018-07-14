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

from dulwich.errors import NotGitRepository
from dulwich.repo import Repo

from joy.gui.textwidget import TextViewerWidget, tk, get_font, TEXT_BINDINGS
from joy.gui.world import World
from joy.library import initialize
from joy.utils.stack import stack_to_string


JOY_HOME = os.environ.get('JOY_HOME')
if JOY_HOME is None:
  JOY_HOME = os.path.expanduser('~/.joypy')
  if not os.path.isabs(JOY_HOME):
    JOY_HOME = os.path.abspath('./JOY_HOME')
  #print 'JOY_HOME=' + JOY_HOME

if not os.path.exists(JOY_HOME):
  #print 'creating...'
  os.makedirs(JOY_HOME, 0700)
  #print 'initializing git repository...'
  repo = Repo.init(JOY_HOME)

else:  # path does exist
  try:
    repo = Repo(JOY_HOME)
  except NotGitRepository:
    #print 'initializing git repository...'
    repo = Repo.init(JOY_HOME)
  #else:
    #print 'opened git repository.'


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
      f.flush()
      os.fsync(f.fileno())
    repo.stage([self.relative_STACK_FN])
    commit_id = repo.do_commit(
      'message',
      committer='Simon Forman <forman.simon@gmail.com>',
      )
    #print >> sys.stderr, commit_id


def init_text(t, title, filename):
  t.winfo_toplevel().title(title)
  if os.path.exists(filename):
    with open(filename) as f:
      data = f.read()
    t.insert(tk.END, data)
    # Prevent this from triggering a git commit.
    t.update()
    t._cancelSave()
  t.pack(expand=True, fill=tk.BOTH)
  t.filename = filename
  t.repo_relative_filename = repo_relative_path(filename)
  t.repo = repo
  t['font'] = FONT  # See below.


def key_bindings(*args):
  print dedent('''
    Ctrl-Enter - Run the selection as Joy code.
    F1 - Reset and show (if hidden) the log.
    Esc - Like F1 but also clears the stack.
    F5 - Copy the selection to text on the stack.
    Shift-F5 - As F5 but cuts the selection.
    F6 - Paste as text from top of stack.
    Shift-F6 - As F6 but pops the item.
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
  '<Shift-F5>': lambda tv: tv.cut,
  '<F5>': lambda tv: tv.copy_selection_to_stack,
  '<Shift-F6>': lambda tv: tv.pastecut,
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
  w = StackDisplayWorld(dictionary=D)
else:
  w = StackDisplayWorld(stack=stack, dictionary=D)


t = TextViewerWidget(w, **defaults)


log_window = tk.Toplevel()
log_window.protocol("WM_DELETE_WINDOW", log_window.withdraw)
log = TextViewerWidget(w, log_window, **defaults)


FONT = get_font('Iosevka', size=14)  # Requires Tk root already set up.


init_text(log, 'Log', LOG_FN)
init_text(t, 'Joy - ' + JOY_HOME, JOY_FN)


GLOBAL_COMMANDS = {
  '<F12>': 'words',
  '<F1>': 'reset_log show_log',
  '<Escape>': 'clear reset_log show_log',
  }
for event, command in GLOBAL_COMMANDS.items():
  t.bind_all(event, lambda _, _command=command: w.interpret(_command))


class FileFaker(object):

  def __init__(self, T):
    self.T = T

  def write(self, text):
    self.T.insert('end', text)
    self.T.see('end')

  def flush(self):
    pass


def main():
  sys.stdout, old_stdout = FileFaker(log), sys.stdout
  try:
    t.mainloop()
  finally:
    sys.stdout = old_stdout
  return 0


if __name__ == '__main__':
  main()
