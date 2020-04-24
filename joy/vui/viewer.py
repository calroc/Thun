# -*- coding: utf-8 -*-
#
#    Copyright © 2019 Simon Forman
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
'''

Viewer
=================

'''
from __future__ import print_function
from __future__ import division
from builtins import chr, object
from past.utils import old_div
import pygame
from joy.vui.core import BACKGROUND, FOREGROUND


class Viewer(object):
	'''
	Base Viewer class
	'''

	MINIMUM_HEIGHT = 11

	def __init__(self, surface):
		self.resurface(surface)
		self.last_touch = 0, 0

	def resurface(self, surface):
		self.w, self.h = surface.get_width(), surface.get_height()
		self.surface = surface

	def split(self, y):
		'''
		Split the viewer at the y coordinate (which is relative to the
		viewer's surface and must be inside it somewhere) and return the
		remaining height.  The upper part of the viewer remains (and gets
		redrawn on a new surface) and the lower space is now available
		for e.g. a new viewer.
		'''
		assert y >= self.MINIMUM_HEIGHT
		new_viewer_h = self.h - y
		self.resurface(self.surface.subsurface((0, 0, self.w, y)))
		if y <= self.last_touch[1]: self.last_touch = 0, 0
		self.draw()
		return new_viewer_h

	def handle(self, message):
		assert self is not message.sender
		pass

	def draw(self):
		'''Draw the viewer onto its surface.'''
		self.surface.fill(BACKGROUND)
		x, y, h = self.w - 1, self.MINIMUM_HEIGHT, self.h - 1
		# Right-hand side.
		pygame.draw.line(self.surface, FOREGROUND, (x, 0), (x, h))
		# Between header and body.
		pygame.draw.line(self.surface, FOREGROUND, (0, y), (x, y))
		# Bottom.
		pygame.draw.line(self.surface, FOREGROUND, (0, h), (x, h))

	def close(self):
		'''Close the viewer and release any resources, etc...'''

	def focus(self, display):
		pass

	def unfocus(self):
		pass

	# Event handling.

	def mouse_down(self, display, x, y, button):
		self.last_touch = x, y

	def mouse_up(self, display, x, y, button):
		pass

	def mouse_motion(self, display, x, y, dx, dy, button0, button1, button2):
		pass

	def key_up(self, display, key, mod):
		if key == pygame.K_q and mod & pygame.KMOD_CTRL: # Ctrl-q
			display.close_viewer(self)
			return True
		if key == pygame.K_g and mod & pygame.KMOD_CTRL: # Ctrl-g
			display.grow_viewer(self)
			return True

	def key_down(self, display, uch, key, mod):
		pass


class MenuViewer(Viewer):

	'''
	MenuViewer class
	'''

	MINIMUM_HEIGHT = 26

	def __init__(self, surface):
		Viewer.__init__(self, surface)
		self.resizing = 0
		self.bg = 100, 150, 100

	def resurface(self, surface):
		Viewer.resurface(self, surface)
		n = self.MINIMUM_HEIGHT - 2
		self.close_rect = pygame.rect.Rect(self.w - 2 - n, 1, n, n)
		self.grow_rect = pygame.rect.Rect(1, 1, n, n)
		self.body_rect = pygame.rect.Rect(
			0, self.MINIMUM_HEIGHT + 1,
			self.w - 1, self.h - self.MINIMUM_HEIGHT - 2)

	def draw(self):
		'''Draw the viewer onto its surface.'''
		Viewer.draw(self)
		if not self.resizing:
			self.draw_menu()
			self.draw_body()

	def draw_menu(self):
		# menu buttons
		pygame.draw.rect(self.surface, FOREGROUND, self.close_rect, 1)
		pygame.draw.rect(self.surface, FOREGROUND, self.grow_rect, 1)

	def draw_body(self):
		self.surface.fill(self.bg, self.body_rect)

	def mouse_down(self, display, x, y, button):
		Viewer.mouse_down(self, display, x, y, button)
		if y <= self.MINIMUM_HEIGHT:
			self.menu_click(display, x, y, button)
		else:
			bx, by = self.body_rect.topleft
			self.body_click(display, x - bx, y - by, button)

	def body_click(self, display, x, y, button):
		if button == 1:
			self.draw_an_a(x, y)

	def menu_click(self, display, x, y, button):
		if button == 1:
			self.resizing = 1
		elif button == 3:
			if self.close_rect.collidepoint(x, y):
				display.close_viewer(self)
				return True
			elif self.grow_rect.collidepoint(x, y):
				display.grow_viewer(self)
				return True

	def mouse_up(self, display, x, y, button):

		if button == 1 and self.resizing:
			if self.resizing == 2:
				self.resizing = 0
				self.draw()
				display.done_resizing()
			self.resizing = 0
			return True

	def mouse_motion(self, display, x, y, rel_x, rel_y, button0, button1, button2):
		if self.resizing and button0:
			self.resizing = 2
			display.change_viewer(self, rel_y, relative=True)
			return True
		else:
			self.resizing = 0
			#self.draw_an_a(x, y)

	def key_up(self, display, key, mod):
		if Viewer.key_up(self, display, key, mod):
			return True

	def draw_an_a(self, x, y):
		# Draw a crude letter A.
		lw, lh = 10, 14
		try: surface = self.surface.subsurface((x - lw, y - lh, lw, lh))
		except ValueError: return
		draw_a(surface, blend=1)


class SomeViewer(MenuViewer):

	def __init__(self, surface):
		MenuViewer.__init__(self, surface)

	def resurface(self, surface):
		MenuViewer.resurface(self, surface)

	def draw_menu(self):
		MenuViewer.draw_menu(self)

	def draw_body(self):
		pass

	def body_click(self, display, x, y, button):
		pass

	def menu_click(self, display, x, y, button):
		if MenuViewer.menu_click(self, display, x, y, button):
			return True

	def mouse_up(self, display, x, y, button):
		if MenuViewer.mouse_up(self, display, x, y, button):
			return True

	def mouse_motion(self, display, x, y, rel_x, rel_y, button0, button1, button2):
		if MenuViewer.mouse_motion(self, display, x, y, rel_x, rel_y,
							   button0, button1, button2):
			return True

	def key_down(self, display, uch, key, mod):
		try:
			print(chr(key), end=' ')
		except ValueError:
			pass


# Note that Oberon book says that if you split at the exact top of a viewer
# it should close, and I think this implies the new viewer gets the old
# viewer's whole height.  I haven't implemented that yet, so the edge-case
# in the code is broken by "intent" for now..


def draw_a(surface, color=FOREGROUND, blend=False):
	w, h = surface.get_width() - 2, surface.get_height() - 2
	pygame.draw.aalines(surface, color, False, (
		(1, h), (old_div(w, 2), 1), (w, h), (1, old_div(h, 2))
		), blend)
