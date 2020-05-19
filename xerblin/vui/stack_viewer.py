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

Stack Viewer
=================

'''
from builtins import map, str
from joy.utils.stack import expression_to_string, iter_stack
from joy.vui import core, text_viewer


MAX_WIDTH = 64


def fsi(item):
	'''Format Stack Item'''
	if isinstance(item, tuple):
		res = '[%s]' % expression_to_string(item)
	elif isinstance(item, str):
		res = '"%s"' % item
	else:
		res = str(item)
	if len(res) > MAX_WIDTH:
		return res[:MAX_WIDTH - 3] + '...'
	return res


class StackViewer(text_viewer.TextViewer):

	def __init__(self, surface):
		super(StackViewer, self).__init__(surface)
		self.stack_holder = None
		self.content_id = 'stack viewer'

	def _attach(self, display):
		if self.stack_holder:
			return
		om = core.OpenMessage(self, 'stack.pickle')
		display.broadcast(om)
		if om.status != core.SUCCESS:
			raise RuntimeError('stack unavailable')
		self.stack_holder = om.thing

	def _update(self):
		self.lines[:] = list(map(fsi, iter_stack(self.stack_holder[0]))) or ['']

	def focus(self, display):
		self._attach(display)
		super(StackViewer, self).focus(display)

	def handle(self, message):
		if (isinstance(message, core.ModifyMessage)
			and message.subject is self.stack_holder
			):
			self._update()
			self.draw_body()
