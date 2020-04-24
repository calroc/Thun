#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import print_function
from future import standard_library
standard_library.install_aliases()
('''\
Joypy - Copyright © 2018 Simon Forman
'''
'This program comes with ABSOLUTELY NO WARRANTY; for details right-click "warranty".'
' This is free software, and you are welcome to redistribute it under certain conditions;'
' right-click "sharing" for details.'
' Right-click on these commands to see docs on UI commands: key_bindings mouse_bindings')
import logging, os, pickle, sys
from textwrap import dedent
from configparser import RawConfigParser

from joy.gui.utils import init_home, argparser, FileFaker

args = argparser.parse_args()
JOY_HOME = args.joy_home
repo = init_home(JOY_HOME)


_log = logging.getLogger(__name__)
logging.basicConfig(
  format='%(asctime)-15s %(levelname)s %(name)s %(message)s',
  filename=os.path.join(JOY_HOME, 'thun.log'),
  level=logging.INFO,
  )
_log.info('Starting with JOY_HOME=%s', JOY_HOME)


from joy.gui.textwidget import TextViewerWidget, tk, get_font
from joy.gui.world import StackDisplayWorld
from joy.library import initialize, DefinitionWrapper
from joy.utils.stack import stack_to_string


cp = RawConfigParser()
cp.optionxform = str  # Don't mess with uppercase.
with open(os.path.join(args.joy_home, 'thun.config')) as f:
  cp.readfp(f)


GLOBAL_COMMANDS = dict(cp.items('key bindings'))


def repo_relative_path(path):
  return os.path.relpath(
    path,
    os.path.commonprefix((repo.controldir(), path))
    )

def commands():
  # pylint: disable=unused-variable

  def key_bindings(*args):
    commands = [  # These are bound in the TextViewerWidget.
      'Control-Enter - Run the selection as Joy code, or if there\'s no selection the line containing the cursor.',
      'F3 - Copy selection to stack.',
      'Shift-F3 - Cut selection to stack.',
      'F4 - Paste item on top of stack to insertion cursor.',
      'Shift-F4 - Pop and paste top of stack to insertion cursor.',
      ]
    for key, command in GLOBAL_COMMANDS.items():
      commands.append('%s - %s' % (key.lstrip('<').rstrip('>'), command))
    print('\n'.join([''] + sorted(commands)))
    return args


  def mouse_bindings(*args):
    print(dedent('''
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
      '''))
    return args


  def reset_log(*args):
    log.delete('0.0', tk.END)
    print(__doc__)
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

  return locals()


STACK_FN = os.path.join(JOY_HOME, 'stack.pickle')
REL_STACK_FN = repo_relative_path(STACK_FN)
JOY_FN = os.path.join(JOY_HOME, 'scratch.txt')
LOG_FN = os.path.join(JOY_HOME, 'log.txt')
D = initialize()
D.update(commands())
DefinitionWrapper.load_definitions(os.path.join(JOY_HOME, 'definitions.txt'), D)
world = StackDisplayWorld(repo, STACK_FN, REL_STACK_FN, dictionary=D)
defaults = dict(width=80, height=25)
t = TextViewerWidget(world, **defaults)
log_window = tk.Toplevel()
log_window.protocol("WM_DELETE_WINDOW", log_window.withdraw)
log = TextViewerWidget(world, log_window, **defaults)
FONT = get_font('Iosevka', size=14)  # Requires Tk root already set up.
log.init('Log', LOG_FN, repo_relative_path(LOG_FN), repo, FONT)
t.init('Joy - ' + JOY_HOME, JOY_FN, repo_relative_path(JOY_FN), repo, FONT)
for event, command in GLOBAL_COMMANDS.items():
  callback = lambda _, _command=command: world.interpret(_command)
  t.bind(event, callback)
  log.bind(event, callback)


def main():
  sys.stdout, old_stdout = FileFaker(log), sys.stdout
  try:
    t.mainloop()
  finally:
    sys.stdout = old_stdout
  return 0


if __name__ == '__main__':
  main()
