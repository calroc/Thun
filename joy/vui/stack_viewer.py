from joy.utils.stack import expression_to_string, iter_stack
import core, text_viewer


MAX_WIDTH = 64


def fsi(item):
    '''Format Stack Item'''
    if isinstance(item, tuple):
        res = '[%s]' % expression_to_string(item)
    elif isinstance(item, str):
        res = '"%s"' % item
    else:
        assert not isinstance(item, unicode), repr(item)
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
        self.lines[:] = map(fsi, iter_stack(self.stack_holder[0])) or ['']

    def focus(self, display):
        self._attach(display)
        super(StackViewer, self).focus(display)

    def handle(self, message):
        if (isinstance(message, core.ModifyMessage)
            and message.subject is self.stack_holder
            ):
            self._update()
            self.draw_body()
