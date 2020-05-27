# -*- coding: utf-8 -*-
#
#    Copyright © 2014, 2015 Simon Forman
#
#    This file is part of Xerblin
#
#    Xerblin is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    Xerblin is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Xerblin.  If not see <http://www.gnu.org/licenses/>.
#
'''

World
===================================

A world incorporates the Joy stack, dictionary, and interpreter, as well
as an "oracle" for information like type-checking and a versioned repo of
documents.

'''
import os, pickle, sys
from inspect import getdoc

from joy.joy import run
from joy.library import HELP_TEMPLATE
from joy.parser import Symbol
from joy.utils.stack import stack_to_string
from .utils import is_numerical


def type_check(name, stack):
	return  # None indicating "I dunno."


class World:
	'''
	The base class for worlds, you give it an initial stack and
	dictionary and provides an API for the UI to use.
	'''

	def __init__(self, stack=(), dictionary=None):
		self.stack = stack
		self.dictionary = dictionary or {}
		self.check_cache = {}

	def check(self, name):
		'''
		Return `True` or `False` or `None` indicating whether a command
		named `name` can evaluate with the current stack.  If the command
		type-checks return `True`, if it fails return `False`, and if the
		type-checker can't determine the answer return `None`.
		'''
		try:
			res = self.check_cache[name]
		except KeyError:
			res = self.check_cache[name] = type_check(name, self.stack)
		return res

	def do_lookup(self, name):
		if name in self.dictionary:
			self.stack = (Symbol(name), ()), self.stack
			self.print_stack()
			self.check_cache.clear()
		else:
			assert is_numerical(name)
			self.interpret(name)

	def do_opendoc(self, name):
		'''
		Print docs on the command named `name`.
		'''
		if is_numerical(name):
			doc = 'The number ' + str(name)
		else:
			try:
				word = self.dictionary[name]
			except KeyError:
				doc = 'Unknown: ' + repr(name)
			else:
				doc = getdoc(word)
		print(HELP_TEMPLATE % (name, doc, name))
		self.print_stack()

	def pop(self):
		'''
		Remove and discard the top item on the stack, if any.
		'''
		if self.stack:
			self.stack = self.stack[1]
			self.print_stack()
			self.check_cache.clear()

	def push(self, it):
		'''
		Push a string onto the stack.
		'''
		it = it.encode('utf8')
		self.stack = it, self.stack
		self.print_stack()
		self.check_cache.clear()

	def peek(self):
		'''
		Return the top item on the stack if any, otherwise `None`.  Does
		not affect the stack.
		'''
		if self.stack:
			return self.stack[0]

	def interpret(self, command):
		'''
		Interpret command and update the world's stack.
		'''
		command = command.strip()
		if self.has(command) and self.check(command) == False:  # not in {True, None}:
			return
		old_stack = self.stack
		self.print_command(command)
		try:
			self.stack, _, self.dictionary = run(
				command,
				self.stack,
				self.dictionary,
				)
		finally:
			self.print_stack()
		if old_stack != self.stack:
			self.check_cache.clear()

	def has(self, name):
		'''
		Boolean predicate indicating if the command named `name` is in
		the world's dictionary.
		'''
		return name in self.dictionary

	def save(self):
		'''Save the current stack.'''
		pass

	def print_command(self, command):
		'''Print the command.'''
		pass

	def print_stack(self):
		'''Print the stack.'''
		pass


class StackDisplayWorld(World):
	'''
	This world subclass saves its stack to a git repo and prints its
	stack and the command.
	'''

	def __init__(self, repo, filename, rel_filename, dictionary=None):
		self.filename = filename
		stack = self._load_stack() or ()
		World.__init__(self, stack, dictionary)
		self.repo = repo
		self.relative_STACK_FN = rel_filename

	def print_command(self, command):
		'''Print the command.'''
		print(command)

	def print_stack(self):
		'''Print the stack.'''
		print('\n%s <-' % stack_to_string(self.stack))

	def save(self):
		'''
		Save the current stack to the repo and commit.
		'''
		with open(self.filename, 'wb') as f:
			os.chmod(self.filename, 0o600)
			pickle.dump(self.stack, f, protocol=2)
			f.flush()
			os.fsync(f.fileno())
		self.repo.stage([self.relative_STACK_FN])
		self.repo.do_commit(
			b'auto-save',
			committer=b'thun-auto-save <nobody@example.com>',
			)

	def _load_stack(self):
		if os.path.exists(self.filename):
			with open(self.filename, 'rb') as f:
				return pickle.load(f)


class StackWorld(StackDisplayWorld):
	'''
	This subclass of `StackDisplayWorld` updates a `StackListbox` when
	the stack is updated.
	'''

	viewer = None

	def set_viewer(self, viewer):
		'''
		After initialization use this method to "attach" the world to a
		`StackListbox`.
		'''
		self.viewer = viewer
		self.viewer.update_stack(self.stack)

	def print_stack(self):
		'''Print the stack and update the `StackListbox` if any.'''
		print(stack_to_string(self.stack), '•', end=' ')
		if self.viewer:
			self.viewer.update_stack(self.stack)

