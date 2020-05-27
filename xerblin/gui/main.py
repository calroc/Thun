#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# This is a script, the module namespace is used as a kind of singleton
# for organizing the moving parts of the system.  I forget why I didn't
# use a more typical class.
#
# This docstring doubles as the log header that the system prints when
# the log is reset.

('''\
Joypy - Copyright © 2018 Simon Forman
'''
'This program comes with ABSOLUTELY NO WARRANTY; for details right-click "warranty".'
' This is free software, and you are welcome to redistribute it under certain conditions;'
' right-click "sharing" for details.'
' Right-click on these commands to see docs on UI commands: key_bindings mouse_bindings')

import logging, os, pickle, sys
from datetime import datetime
from textwrap import dedent
from configparser import RawConfigParser

from xerblin.gui.utils import init_home, argparser, FileFaker
from xerblin.gui.textwidget import TextViewerWidget, tk, get_font
from xerblin.gui.world import StackWorld
from xerblin.gui.controllerlistbox import StackListbox
from joy.library import initialize, DefinitionWrapper
from joy.utils.stack import stack_to_string


DATETIME_FORMAT = 'Thun • %B %d %a • %I:%M %p'
VIEWER_DEFAULTS = dict(width=80, height=25)


args = argparser.parse_args()
JOY_HOME = args.joy_home
repo = init_home(JOY_HOME)
homed = lambda fn: os.path.join(JOY_HOME, fn)


cp = RawConfigParser()
# Don't mess with uppercase.  We need it for Tk event binding.
cp.optionxform = str
with open(os.path.join(args.joy_home, 'thun.config')) as f:
	cp.readfp(f)


GLOBAL_COMMANDS = dict(cp.items('key bindings'))


def repo_relative_path(path):
	return os.path.relpath(
		path,
		os.path.commonprefix((repo.controldir(), path))
		)

def commands():
	'''
	We define a bunch of meta-interpreter command functions here and
	return them in a dictionary.  They have all the contents of this
	module in their scope so they can e.g. modify the log viewer window.
	'''
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
		print(datetime.now().strftime(DATETIME_FORMAT))
		return args


	def Thun(*args):
		print(__doc__)
		return args


	def show_log(*args):
		log_window.wm_deiconify()
		log_window.update()
		return args


	def show_stack(*args):
		stack_window.wm_deiconify()
		stack_window.update()
		return args


	def grand_reset(s, e, d):
		stack = world.load_stack() or ()
		log.reset()
		t.reset()
		return stack, e, d

	return locals()


# Identify the system core files.
DEFS_FN = homed('definitions.txt')
JOY_FN = homed('scratch.txt')
LOG_FN = homed('log.txt')
STACK_FN = homed('stack.pickle')
REL_STACK_FN = repo_relative_path(STACK_FN)

# Initialize the Joy dictionary.
D = initialize()
D.update(commands())
DefinitionWrapper.load_definitions(DEFS_FN, D)

world = StackWorld(repo, STACK_FN, REL_STACK_FN, dictionary=D)

t = TextViewerWidget(world, **VIEWER_DEFAULTS)

log_window = tk.Toplevel()
# Make it so that you can't actually close the log window, if you try it
# will just "withdraw" (which is like minifying but without a entry in
# the taskbar or icon or whatever.)
log_window.protocol("WM_DELETE_WINDOW", log_window.withdraw)
log = TextViewerWidget(world, log_window, **VIEWER_DEFAULTS)

FONT = get_font('Iosevka', size=14)  # Requires Tk root already set up.

stack_window = tk.Toplevel()
stack_window.title("Stack")
stack_window.protocol("WM_DELETE_WINDOW", log_window.withdraw)
stack_viewer = StackListbox(world, stack_window, items=[], font=FONT)
stack_viewer.pack(expand=True, fill=tk.BOTH)
world.set_viewer(stack_viewer)


log.init('Log', LOG_FN, repo_relative_path(LOG_FN), repo, FONT)
t.init('Joy - ' + JOY_HOME, JOY_FN, repo_relative_path(JOY_FN), repo, FONT)

for event, command in GLOBAL_COMMANDS.items():
	callback = lambda _, _command=command: world.interpret(_command)
	t.bind_all(event, callback)


def main():
	sys.stdout, old_stdout = FileFaker(log), sys.stdout
	try:
		t.mainloop()
	finally:
		sys.stdout = old_stdout
	return 0


if __name__ == '__main__':
	main()
