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

Core
=====================

The core module defines a bunch of system-wide "constants" (some colors
and PyGame event groups), the message classes for Oberon-style message
passing, a "world" class that holds the main context for the system, and
a mainloop class that manages the, uh, main loop (the PyGame event queue.)

'''
from sys import stderr
from traceback import format_exc
import pygame
from joy.joy import run
from joy.utils.stack import stack_to_string


COMMITTER = 'Joy <auto-commit@example.com>'


BLACK = FOREGROUND = 0, 0, 0
GREY = 127, 127, 127
WHITE = BACKGROUND = 255, 255, 255
BLUE = 100, 100, 255
GREEN = 70, 200, 70


MOUSE_EVENTS = frozenset({
    pygame.MOUSEMOTION,
    pygame.MOUSEBUTTONDOWN,
    pygame.MOUSEBUTTONUP
    })
'PyGame mouse events.'

ARROW_KEYS = frozenset({
    pygame.K_UP,
    pygame.K_DOWN,
    pygame.K_LEFT,
    pygame.K_RIGHT
    })
'PyGame arrow key events.'


TASK_EVENTS = tuple(range(pygame.USEREVENT, pygame.NUMEVENTS))
'Keep track of all possible task events.'

AVAILABLE_TASK_EVENTS = set(TASK_EVENTS)
'Task IDs that have not been assigned to a task.'

ALLOWED_EVENTS = [pygame.QUIT, pygame.KEYUP, pygame.KEYDOWN]
ALLOWED_EVENTS.extend(MOUSE_EVENTS)
ALLOWED_EVENTS.extend(TASK_EVENTS)
'Event "mask" for PyGame event queue, we are only interested in these event types.'


ERROR = -1
PENDING = 0
SUCCESS = 1
# 'Message status codes...  dunno if this is a good idea or not...


class Message(object):
    '''Message base class.  Contains ``sender`` field.'''
    def __init__(self, sender):
        self.sender = sender


class CommandMessage(Message):
    '''For commands, adds ``command`` field.'''
    def __init__(self, sender, command):
        Message.__init__(self, sender)
        self.command = command


class ModifyMessage(Message):
    '''
    For when resources are modified, adds ``subject`` and ``details``
    fields.
    '''
    def __init__(self, sender, subject, **details):
        Message.__init__(self, sender)
        self.subject = subject
        self.details = details


class OpenMessage(Message):
    '''
    For when resources are modified, adds ``name``, content_id``,
    ``status``, and ``traceback`` fields.
    '''
    def __init__(self, sender, name):
        Message.__init__(self, sender)
        self.name = name
        self.content_id = self.thing = None
        self.status = PENDING
        self.traceback = None


class PersistMessage(Message):
    '''
    For when resources are modified, adds ``content_id`` and ``details``
    fields.
    '''
    def __init__(self, sender, content_id, **details):
        Message.__init__(self, sender)
        self.content_id = content_id
        self.details = details


class ShutdownMessage(Message):
    '''Signals that the system is shutting down.'''


# Joy Interpreter & Context


class World(object):
    '''
    This object contains the system context, the stack, dictionary, a
    reference to the display broadcast method, and the log.
    '''

    def __init__(self, stack_id, stack_holder, dictionary, notify, log):
        self.stack_holder = stack_holder
        self.dictionary = dictionary
        self.notify = notify
        self.stack_id = stack_id
        self.log = log.lines
        self.log_id = log.content_id

    def handle(self, message):
        '''
        Deal with updates to the stack and commands.
        '''
        if (isinstance(message, ModifyMessage)
            and message.subject is self.stack_holder
            ):
            self._log_lines('', '%s <-' % self.format_stack())
        if not isinstance(message, CommandMessage):
            return
        c, s, d = message.command, self.stack_holder[0], self.dictionary
        self._log_lines('', '-> %s' % (c,))
        self.stack_holder[0], _, self.dictionary = run(c, s, d)
        mm = ModifyMessage(self, self.stack_holder, content_id=self.stack_id)
        self.notify(mm)

    def _log_lines(self, *lines):
        self.log.extend(lines)
        self.notify(ModifyMessage(self, self.log, content_id=self.log_id))

    def format_stack(self):
        try:
            return stack_to_string(self.stack_holder[0])
        except:
            print >> stderr, format_exc()
            return str(self.stack_holder[0])


def push(sender, item, notify, stack_name='stack.pickle'):
    '''
    Helper function to push an item onto the system stack with message.
    '''
    om = OpenMessage(sender, stack_name)
    notify(om)
    if om.status == SUCCESS:
        om.thing[0] = item, om.thing[0]
        notify(ModifyMessage(sender, om.thing, content_id=om.content_id))
    return om.status


def open_viewer_on_string(sender, content, notify):
    '''
    Helper function to open a text viewer on a string.
    Typically used to show tracebacks.
    '''
    push(sender, content, notify)
    notify(CommandMessage(sender, 'good_viewer_location open_viewer'))


# main loop


class TheLoop(object):
    '''
    The main loop manages tasks and the PyGame event queue
    and framerate clock.
    '''

    FRAME_RATE = 24

    def __init__(self, display, clock):
        self.display = display
        self.clock = clock
        self.tasks = {}
        self.running = False

    def install_task(self, F, milliseconds):
        '''
        Install a task to run every so many milliseconds.
        '''
        try:
            task_event_id = AVAILABLE_TASK_EVENTS.pop()
        except KeyError:
            raise RuntimeError('out of task ids')
        self.tasks[task_event_id] = F
        pygame.time.set_timer(task_event_id, milliseconds)
        return task_event_id

    def remove_task(self, task_event_id):
        '''
        Remove an installed task.
        '''
        assert task_event_id in self.tasks, repr(task_event_id)
        pygame.time.set_timer(task_event_id, 0)
        del self.tasks[task_event_id]
        AVAILABLE_TASK_EVENTS.add(task_event_id)

    def __del__(self):
        # Best effort to cancel all running tasks.
        for task_event_id in self.tasks:
            pygame.time.set_timer(task_event_id, 0)

    def run_task(self, task_event_id):
        '''
        Give a task its time to shine.
        '''
        task = self.tasks[task_event_id]
        try:
            task()
        except:
            traceback = format_exc()
            self.remove_task(task_event_id)
            print >> stderr, traceback
            print >> stderr, 'TASK removed due to ERROR', task
            open_viewer_on_string(self, traceback, self.display.broadcast)

    def loop(self):
        '''
        The actual main loop machinery.

        Maintain a ``running`` flag, pump the PyGame event queue and
        handle the events (dispatching to the display), tick the clock.

        When the loop is exited (by clicking the window close button or
        pressing the ``escape`` key) it broadcasts a ``ShutdownMessage``.
        '''
        self.running = True
        while self.running:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
                elif event.type == pygame.KEYUP and event.key == pygame.K_ESCAPE:
                    self.running = False
                elif event.type in self.tasks:
                    self.run_task(event.type)
                else:
                    self.display.dispatch_event(event)
            pygame.display.update()
            self.clock.tick(self.FRAME_RATE)
        self.display.broadcast(ShutdownMessage(self))
