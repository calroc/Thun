'''
    Copyright (C) 2004 - 2008 Simon Forman

    This file is part of Xerblin.

    Xerblin is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

'''
from Tkinter import Listbox, SINGLE
from Tkdnd import dnd_start
from joy.utils.stack import iter_stack, list_to_stack, expression_to_string


class SourceWrapper:
    '''
    Helper object for drag and drop.
    '''
    def __init__(self, source, widget, index=None):
        '''
        source is the object being dragged, widget is the container that's
        initialing the drag operation, and index s thu index of the item
        in the widget's model object (which presumably is a ListModel
        containing the source object.)
        '''
        self.source = source
        self.widget = widget
        self.index = index

    def dnd_end(self, target, event):
        try:
            self.widget.clear()
        except AttributeError:
            pass


class DraggyListbox(Listbox):

    def __init__(self, master=None, **kw):

        # Get our stack.
        self.stack = kw.pop('items')

        # Override any passed in selectmode.
        kw['selectmode'] = SINGLE

        Listbox.__init__(self, master, **kw)

        self.bind('<Button-1>', self.startDrag)
        self.bind('<ButtonRelease-1>', self.clear)

    def clear(self, event=None):
        i = self.curselection()
        if i:
            i = int(i[0])
            self.selection_clear(i)

    def startDrag(self, event):
        i = self.nearest(event.y)
        if i >= 0:
            self.selection_set(i)
            source = self.stack[i]
            source = SourceWrapper(source, self, i)
            event.num = 1 # Don't ask. (See Tkdnd.py)
            dnd_start(source, event)
        return "break"


class ControllerListbox(DraggyListbox):

    def __init__(self, master=None, **kw):
        DraggyListbox.__init__(self, master, **kw)
        self._dragIndex = -1

    def dnd_accept(self, source, event):
        self.focus_force()
        return self

    def dnd_enter(self, source, event):
        pass

    def dnd_motion(self, source, event):
        I = self.nearest(event.y_root - self.winfo_rooty())
        if self._dragIndex >= 0:
            self.delete(self._dragIndex)
        self._dragIndex = I
        self.insert(I, '---')

    def dnd_leave(self, source, event):
        if self._dragIndex >= 0:
            self.delete(self._dragIndex)
            self._dragIndex = -1

    def dnd_commit(self, source, event):
        i = self._dragIndex

        if i >= 0:
            self.delete(i)
            self._dragIndex = -1

        try:
            if self is source.widget:

                # Don't duplicate something by dropping it on itself.
                if i == source.index:
                    return

                # Instead, move it by removing it before the pending append.
                del self.stack[source.index]
                if i > source.index:
                    i -= 1

            self.stack.insert(i, source.source)

        finally:
            self.clear()


class StackListbox(ControllerListbox):

    def __init__(self, world, master=None, **kw):
        ControllerListbox.__init__(self, master, **kw)
        self.world = world

    def _update(self):
        self.delete(0, 'end')
        self.insert(0, *map(self.format, self.stack))

    def update_stack(self, stack):
        self.stack = list(iter_stack(stack))
        self._update()

    def dnd_commit(self, source, event):
        ControllerListbox.dnd_commit(self, source, event)
        self._update()
        self.world.stack = list_to_stack(self.stack)

    @staticmethod
    def format(item):
        if isinstance(item, tuple):
            return '[%s]' % expression_to_string(item)
        return str(item)
