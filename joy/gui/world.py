# -*- coding: utf-8 -*-
#
#    Copyright © 2014, 2015 Simon Forman
#
#    This file is part of joy.py
#
#    joy.py is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    joy.py is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with joy.py.  If not see <http://www.gnu.org/licenses/>.
#
from __future__ import print_function
from builtins import object
from logging import getLogger

_log = getLogger(__name__)

import os, pickle, sys
from inspect import getdoc

from joy.joy import run
from joy.library import HELP_TEMPLATE
from joy.parser import Symbol
from joy.utils.stack import stack_to_string
from joy.utils.types import type_check
from .utils import is_numerical


class World(object):

	def __init__(self, stack=(), dictionary=None, text_widget=None):
		self.stack = stack
		self.dictionary = dictionary or {}
		self.text_widget = text_widget
		self.check_cache = {}

	def check(self, name):
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
		if self.stack:
			self.stack = self.stack[1]
		self.print_stack()
		self.check_cache.clear()

	def push(self, it):
		it = it.encode('utf8')
		self.stack = it, self.stack
		self.print_stack()
		self.check_cache.clear()

	def peek(self):
		if self.stack:
			return self.stack[0]

	def interpret(self, command):
		if self.has(command) and self.check(command) == False:  # not in {True, None}:
			return
		old_stack = self.stack
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
		return name in self.dictionary

	def save(self):
		pass

	def print_stack(self):
		stack_out_index = self.text_widget.search('<' 'STACK', 1.0)
		if stack_out_index:
			self.text_widget.see(stack_out_index)
			s = stack_to_string(self.stack) + '\n'
			self.text_widget.insert(stack_out_index, s)


class StackDisplayWorld(World):

	def __init__(self, repo, filename, rel_filename, dictionary=None, text_widget=None):
		self.filename = filename
		stack = self.load_stack() or ()
		World.__init__(self, stack, dictionary, text_widget)
		self.repo = repo
		self.relative_STACK_FN = rel_filename

	def interpret(self, command):
		command = command.strip()
		if self.has(command) and self.check(command) == False:  # not in {True, None}:
			return
		print('\njoy?', command)
		super(StackDisplayWorld, self).interpret(command)

	def print_stack(self):
		print('\n%s <-' % stack_to_string(self.stack))

	def save(self):
		with open(self.filename, 'wb') as f:
			os.chmod(self.filename, 0o600)
			pickle.dump(self.stack, f, protocol=2)
			f.flush()
			os.fsync(f.fileno())
		self.repo.stage([self.relative_STACK_FN])
		commit_id = self.repo.do_commit(
			b'auto-save',
			committer=b'thun-auto-save <nobody@example.com>',
			)
		_log.info('commit %s', commit_id)

	def load_stack(self):
		if os.path.exists(self.filename):
			with open(self.filename, 'rb') as f:
				return pickle.load(f)


class StackWorld(StackDisplayWorld):

	viewer = None

	def set_viewer(self, viewer):
		self.viewer = viewer
		self.viewer.update_stack(self.stack)

	def print_stack(self):
		StackDisplayWorld.print_stack(self)
		if self.viewer:
			self.viewer.update_stack(self.stack)

