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

Utilities
===================================


'''
from __future__ import print_function
from builtins import object
import argparse, os, sys
from os import listdir, mkdir
from os.path import abspath, exists, expanduser, isfile, join

from dulwich.errors import NotGitRepository
from dulwich.repo import Repo


COMMITTER = b'Joy <auto-commit@example.com>'
DEFAULT_JOY_HOME = expanduser(join('~', '.joypy'))


def is_numerical(s):
	try:
		float(s)
	except ValueError:
		return False
	return True


def home_dir(path):
	'''Return the absolute path of an existing directory.'''

	fullpath = expanduser(path) if path.startswith('~') else abspath(path)

	if not exists(fullpath):
		if path == DEFAULT_JOY_HOME:
			print('Creating JOY_HOME', repr(fullpath))
			mkdir(fullpath, 0o700)
		else:
			print(repr(fullpath), "doesn't exist.", file=sys.stderr)
			raise ValueError(path)

	return fullpath


def init_home(fullpath):
	'''
	Open or create the Repo.
	If there are contents in the dir but it's not a git repo, quit.
	'''
	try:
		repo = Repo(fullpath)
	except NotGitRepository:
		print(repr(fullpath), "no repository", file=sys.stderr)

		if listdir(fullpath):
			print(repr(fullpath), "has contents\nQUIT.", file=sys.stderr)
			sys.exit(2)

		print('Initializing repository in', fullpath)
		repo = init_repo(fullpath)

	print('Using repository in', fullpath)
	return repo


def init_repo(repo_dir):
	'''
	Create a repo, load the initial content, and make the first commit.
	Return the Repo object.
	'''
	repo = Repo.init(repo_dir)
	import joy.gui.init_joy_home
	joy.gui.init_joy_home.initialize(repo_dir)
	repo.stage([fn for fn in listdir(repo_dir) if isfile(join(repo_dir, fn))])
	repo.do_commit(b'Initial commit.', committer=COMMITTER)
	return repo


argparser = argparse.ArgumentParser(
	prog='xerblin',
	description='Experimental Brutalist UI for Joy.',
	)


argparser.add_argument(
	'-j', '--joy-home',
	help='Use a directory other than %s as JOY_HOME' % DEFAULT_JOY_HOME,
	default=DEFAULT_JOY_HOME,
	dest='joy_home',
	type=home_dir,
	)


class FileFaker(object):

	def __init__(self, T):
		self.T = T

	def write(self, text):
		self.T.insert('end', text)
		self.T.see('end')

	def flush(self):
		pass
