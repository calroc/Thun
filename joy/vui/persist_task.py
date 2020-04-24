# -*- coding: utf-8 -*-
#
#    Copyright © 2019 Simon Forman
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

Persist Task
===========================

This module deals with persisting the "resources" (text files and the
stack) to the git repo in the ``JOY_HOME`` directory.

'''
from __future__ import print_function
import os, pickle, traceback
from collections import Counter
from dulwich.errors import NotGitRepository
from dulwich.repo import Repo
from joy.vui import core, init_joy_home


def open_repo(repo_dir=None, initialize=False):
    '''
    Open, or create, and return a Dulwich git repo object for the given
    directory.  If the dir path doesn't exist it will be created.  If it
    does exist but isn't a repo the result depends on the ``initialize``
    argument.  If it is ``False`` (the default) a ``NotGitRepository``
    exception is raised, otherwise ``git init`` is effected in the dir.
    '''
    if not os.path.exists(repo_dir):
        os.makedirs(repo_dir, 0o700)
        return init_repo(repo_dir)
    try:
        return Repo(repo_dir)
    except NotGitRepository:
        if initialize:
            return init_repo(repo_dir)
        raise


def init_repo(repo_dir):
    '''
    Initialize a git repository in the directory.  Stage and commit all
    files (toplevel, not those in subdirectories if any) in the dir.
    '''
    repo = Repo.init(repo_dir)
    init_joy_home.initialize(repo_dir)
    repo.stage([
        fn
        for fn in os.listdir(repo_dir)
        if os.path.isfile(os.path.join(repo_dir, fn))
        ])
    repo.do_commit('Initial commit.', committer=core.COMMITTER)
    return repo


def make_repo_relative_path_maker(repo):
    '''
    Helper function to return a function that returns a path given a path,
    that's relative to the repository.
    '''
    c = repo.controldir()
    def repo_relative_path(path):
        return os.path.relpath(path, os.path.commonprefix((c, path)))
    return repo_relative_path


class Resource(object):
    '''
    Handle the content of a text files as a list of lines, deal with
    saving it and staging the changes to a repo.
    '''

    def __init__(self, filename, repo_relative_filename, thing=None):
        self.filename = filename
        self.repo_relative_filename = repo_relative_filename
        self.thing = thing or self._from_file(open(filename))

    def _from_file(self, f):
        return f.read().splitlines()

    def _to_file(self, f):
        for line in self.thing:
            print(line, file=f)

    def persist(self, repo):
        '''
        Save the lines to the file and stage the file in the repo.
        '''
        with open(self.filename, 'w') as f:
            os.chmod(self.filename, 0o600)
            self._to_file(f)
            f.flush()
            os.fsync(f.fileno())
            # For goodness's sake, write it to the disk already!
        repo.stage([self.repo_relative_filename])


class PickledResource(Resource):
    '''
    A ``Resource`` subclass that uses ``pickle`` on its file/thing.
    '''

    def _from_file(self, f):
        return [pickle.load(f)]

    def _to_file(self, f):
        pickle.dump(self.thing[0], f)


class PersistTask(object):
    '''
    This class deals with saving changes to the git repo.
    '''

    LIMIT = 10
    MAX_SAVE = 10

    def __init__(self, home):
        self.home = home
        self.repo = open_repo(home)
        self._r = make_repo_relative_path_maker(self.repo)
        self.counter = Counter()
        self.store = {}

    def open(self, name):
        '''
        Look up the named file in home and return its content_id and data.
        '''
        fn = os.path.join(self.home, name)
        content_id = name # hash(fn)
        try:
            resource = self.store[content_id]
        except KeyError:
            R = PickledResource if name.endswith('.pickle') else Resource
            resource = self.store[content_id] = R(fn, self._r(fn))
        return content_id, resource.thing

    def handle(self, message):
        '''
        Handle messages, dispatch to ``handle_FOO()`` methods.
        '''
        if isinstance(message, core.OpenMessage):
            self.handle_open(message)
        elif isinstance(message, core.ModifyMessage):
            self.handle_modify(message)
        elif isinstance(message, core.PersistMessage):
            self.handle_persist(message)
        elif isinstance(message, core.ShutdownMessage):
            for content_id in self.counter:
                self.store[content_id].persist(self.repo)
            self.commit('shutdown')

    def handle_open(self, message):
        '''
        Foo.
        '''
        try:
            message.content_id, message.thing = self.open(message.name)
        except:
            message.traceback = traceback.format_exc()
            message.status = core.ERROR
        else:
            message.status = core.SUCCESS

    def handle_modify(self, message):
        '''
        Foo.
        '''
        try:
            content_id = message.details['content_id']
        except KeyError:
            return
        if not content_id:
            return
        self.counter[content_id] += 1
        if self.counter[content_id] > self.LIMIT:
            self.persist(content_id)
            self.commit('due to activity')

    def handle_persist(self, message):
        '''
        Foo.
        '''
        try:
            resource = self.store[message.content_id]
        except KeyError:
            resource = self.handle_persist_new(message)
        resource.persist(self.repo)
        self.commit('by request from %r' % (message.sender,))

    def handle_persist_new(self, message):
        '''
        Foo.
        '''
        name = message.content_id
        check_filename(name)
        fn = os.path.join(self.home, name)
        thing = message.details['thing']
        R = PickledResource if name.endswith('.pickle') else Resource # !!! refactor!
        resource = self.store[name] = R(fn, self._r(fn), thing)
        return resource

    def persist(self, content_id):
        '''
        Persist a resource.
        '''
        del self.counter[content_id]
        self.store[content_id].persist(self.repo)

    def task_run(self):
        '''
        Stage any outstanding changes.
        '''
        if not self.counter:
            return
        for content_id, _ in self.counter.most_common(self.MAX_SAVE):
            self.persist(content_id)
        self.commit()

    def commit(self, message='auto-commit'):
        '''
        Commit.
        '''
        return self.repo.do_commit(message, committer=core.COMMITTER)

    def scan(self):
        '''
        Return a sorted list of all the files in the home dir.
        '''
        return sorted([
            fn
            for fn in os.listdir(self.home)
            if os.path.isfile(os.path.join(self.home, fn))
            ])


def check_filename(name):
    '''
    Sanity checks for filename.
    '''
    # TODO: improve this...
    if len(name) > 64:
        raise ValueError('bad name %r' % (name,))
    left, dot, right = name.partition('.')
    if not left.isalnum() or dot and not right.isalnum():
        raise ValueError('bad name %r' % (name,))


if __name__ == '__main__':
    JOY_HOME = os.path.expanduser('~/.thun')
    pt = PersistTask(JOY_HOME)
    content_id, thing = pt.open('stack.pickle')
    pt.persist(content_id)
    print(pt.counter)
    mm = core.ModifyMessage(None, None, content_id=content_id)
    pt.handle(mm)
    print(pt.counter)
