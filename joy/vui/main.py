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
import os, sys, traceback
import pygame
from joy.library import initialize, DefinitionWrapper, SimpleFunctionWrapper
from joy.vui import core, display, persist_task


FULLSCREEN = '-f' in sys.argv


JOY_HOME = os.environ.get('JOY_HOME')
if JOY_HOME is None:
    JOY_HOME = os.path.expanduser('~/.thun')
    if not os.path.isabs(JOY_HOME):
        raise ValueError('what directory?')


def load_definitions(pt, dictionary):
    lines = pt.open('definitions.txt')[1]
    for line in lines:
        if '==' in line:
            DefinitionWrapper.add_def(line, dictionary)


def load_primitives(home, name_space):
    fn = os.path.join(home, 'library.py')
    if os.path.exists(fn):
        execfile(fn, name_space)


def init():
    print 'Initializing Pygame...'
    pygame.init()
    print 'Creating window...'
    if FULLSCREEN:
        screen = pygame.display.set_mode()
    else:
        screen = pygame.display.set_mode((1024, 768))
    clock = pygame.time.Clock()
    pygame.event.set_allowed(None)
    pygame.event.set_allowed(core.ALLOWED_EVENTS)
    pt = persist_task.PersistTask(JOY_HOME)
    return screen, clock, pt


def init_context(screen, clock, pt):
    D = initialize()
    d = display.Display(
        screen,
        D.__contains__,
        *((144 - 89, 144, 89) if FULLSCREEN else (89, 144))
        )
    log = d.init_text(pt, 0, 0, 'log.txt')
    tho = d.init_text(pt, 0, d.h / 3, 'menu.txt')
    t = d.init_text(pt, d.w / 2, 0, 'scratch.txt')
    loop = core.TheLoop(d, clock)
    stack_id, stack_holder = pt.open('stack.pickle')
    world = core.World(stack_id, stack_holder, D, d.broadcast, log)
    loop.install_task(pt.task_run, 10000)  # save files every ten seconds
    d.handlers.append(pt.handle)
    d.handlers.append(world.handle)
    load_definitions(pt, D)
    return locals()


def error_guard(loop, n=10):
    error_count = 0
    while error_count < n:
        try:
            loop()
            break
        except:
            traceback.print_exc(file=sys.stderr)
            error_count += 1


class FileFaker(object):

    def __init__(self, log):
        self.log = log

    def write(self, text):
        self.log.append(text)

    def flush(self):
        pass


def main(screen, clock, pt):
    name_space = init_context(screen, clock, pt)
    load_primitives(pt.home, name_space.copy())

    @SimpleFunctionWrapper
    def evaluate(stack):
        '''Evaluate the Python code text on the top of the stack.'''
        code, stack = stack
        exec code in name_space.copy()
        return stack

    name_space['D']['evaluate'] = evaluate


    sys.stdout, old_stdout = FileFaker(name_space['log']), sys.stdout
    try:
        error_guard(name_space['loop'].loop)
    finally:
        sys.stdout = old_stdout

    return name_space['d']
