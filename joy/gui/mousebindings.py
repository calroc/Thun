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


#Do-nothing event handler.
nothing = lambda event: None


class MouseBindingsMixin:
  """TextViewerWidget mixin class to provide mouse bindings."""

  def __init__(self):

    #Remember our mouse button state
    self.B1_DOWN = False
    self.B2_DOWN = False
    self.B3_DOWN = False

    #Remember our pending action.
    self.dothis = nothing

    #We'll need to remember whether or not we've been moving B2.
    self.beenMovingB2 = False

    #Unbind the events we're interested in.
    for sequence in (
      "<Button-1>", "<B1-Motion>", "<ButtonRelease-1>",
      "<Button-2>", "<B2-Motion>", "<ButtonRelease-2>",
      "<Button-3>", "<B3-Motion>", "<ButtonRelease-3>",
      "<B1-Leave>", "<B2-Leave>", "<B3-Leave>", "<Any-Leave>", "<Leave>"
      ):
      self.unbind(sequence)
      self.unbind_all(sequence)

    self.event_delete('<<PasteSelection>>') #I forgot what this was for! :-P  D'oh!

    #Bind our event handlers to their events.
    self.bind("<Button-1>", self.B1d)
    self.bind("<B1-Motion>", self.B1m)
    self.bind("<ButtonRelease-1>", self.B1r)

    self.bind("<Button-2>", self.B2d)
    self.bind("<B2-Motion>", self.B2m)
    self.bind("<ButtonRelease-2>", self.B2r)

    self.bind("<Button-3>", self.B3d)
    self.bind("<B3-Motion>", self.B3m)
    self.bind("<ButtonRelease-3>", self.B3r)

    self.bind("<Any-Leave>", self.leave)

  def B1d(self, event):
    '''button one pressed'''
    self.B1_DOWN = True

    if self.B2_DOWN:

      self.unhighlight_command()

      if self.B3_DOWN :
        self.dothis = self.cancel

      else:
        #copy TOS to the mouse (instead of system selection.)
        self.dothis = self.copyto #middle-left-interclick

    elif self.B3_DOWN :
      self.unhighlight_command()
      self.dothis = self.opendoc #right-left-interclick

    else:
      ##button 1 down, set insertion and begin selection.
      ##Actually, do nothing. Tk Text widget defaults take care of it.
      self.dothis = nothing
      return

    #Prevent further event handling by returning "break".
    return "break"

  def B2d(self, event):
    '''button two pressed'''
    self.B2_DOWN = 1

    if self.B1_DOWN :

      if self.B3_DOWN :
        self.dothis = self.cancel

      else:
        #left-middle-interclick - cut selection to stack
        self.dothis = self.cut

    elif self.B3_DOWN :
      self.unhighlight_command()
      self.dothis = self.lookup #right-middle-interclick - lookup

    else:
      #middle-click - paste X selection to mouse pointer
      self.set_insertion_point(event)
      self.dothis = self.paste_X_selection_to_mouse_pointer
      return

    return "break"

  def B3d(self, event):
    '''button three pressed'''
    self.B3_DOWN = 1

    if self.B1_DOWN :

      if self.B2_DOWN :
        self.dothis = self.cancel

      else:
        #left-right-interclick - run selection
        self.dothis = self.run_selection

    elif self.B2_DOWN :
      #middle-right-interclick - Pop/Cut from TOS to insertion cursor
      self.unhighlight_command()
      self.dothis = self.pastecut

    else:
      #right-click
      self.CommandFirstDown(event)

    return "break"

  def B1m(self, event):
    '''button one moved'''
    if self.B2_DOWN or self.B3_DOWN:
      return "break"

  def B2m(self, event):
    '''button two moved'''
    if self.dothis == self.paste_X_selection_to_mouse_pointer and \
       not (self.B1_DOWN or self.B3_DOWN):

      self.beenMovingB2 = True
      return

    return "break"

  def B3m(self, event):
    '''button three moved'''
    if self.dothis == self.do_command and \
       not (self.B1_DOWN or self.B2_DOWN):

      self.update_command_word(event)

    return "break"

  def B1r(self, event):
    '''button one released'''
    self.B1_DOWN = False

    if not (self.B2_DOWN or self.B3_DOWN):
      self.dothis(event)

    return "break"

  def B2r(self, event):
    '''button two released'''
    self.B2_DOWN = False

    if not (self.B1_DOWN or self.B3_DOWN or self.beenMovingB2):
      self.dothis(event)

    self.beenMovingB2 = False

    return "break"

  def B3r(self, event):
    '''button three released'''
    self.B3_DOWN = False

    if not (self.B1_DOWN or self.B2_DOWN) :
      self.dothis(event)

    return "break"

  def InsertFirstDown(self, event):
    self.focus()
    self.dothis = nothing
    self.set_insertion_point(event)

  def CommandFirstDown(self, event):
    self.dothis = self.do_command
    self.update_command_word(event)
