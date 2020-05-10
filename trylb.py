from Tkinter import Tk, mainloop, BOTH
from joy.gui.controllerlistbox import ControllerListbox


class StackListbox(ControllerListbox):

    def _update(self):
        self.delete(0, 'end')
        self.insert(0, *self.stack)

    def dnd_commit(self, source, event):
        ControllerListbox.dnd_commit(self, source, event)
        self._update()

    
T = Tk()
T.title("Hello there.")
stack = [1, 2, 3]
lb = FooListbox(T, items=stack)
lb.pack(expand=True, fill=BOTH)
lb._update()
