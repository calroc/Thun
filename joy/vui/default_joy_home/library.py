'''
This file is execfile()'d with a namespace containing:

  D - the Joy dictionary
  d - the Display object
  pt - the PersistTask object
  log - the log.txt viewer
  loop - the TheLoop main loop object
  stack_holder - the Python list object that holds the Joy stack tuple
  world - the Joy environment

'''
from joy.library import (
    DefinitionWrapper,
    FunctionWrapper,
    SimpleFunctionWrapper,
    )
from joy.utils.stack import list_to_stack, concat
from vui import core, text_viewer, stack_viewer


def install(command): D[command.name] = command


@install
@SimpleFunctionWrapper
def list_resources(stack):
    '''
    Put a string on the stack with the names of all the known resources
    one-per-line.
    '''
    return '\n'.join(pt.scan()), stack


@install
@SimpleFunctionWrapper
def open_stack(stack):
    '''
    Given a coordinate pair [x y] (in pixels) open a StackViewer there.
    '''
    (x, (y, _)), stack = stack
    V = d.open_viewer(x, y, stack_viewer.StackViewer)
    V.draw()
    return stack


@install
@SimpleFunctionWrapper
def open_resource(stack):
    '''
    Given a coordinate pair [x y] (in pixels) and the name of a resource
    (from list_resources command) open a viewer on that resource at that
    location.
    '''
    ((x, (y, _)), (name, stack)) = stack
    om = core.OpenMessage(world, name)
    d.broadcast(om)
    if om.status == core.SUCCESS:
        V = d.open_viewer(x, y, text_viewer.TextViewer)
        V.content_id, V.lines = om.content_id, om.thing
        V.draw()
    return stack


@install
@SimpleFunctionWrapper
def name_viewer(stack):
    '''
    Given a string name on the stack, if the currently focused viewer is
    anonymous, name the viewer and persist it in the resource store under
    that name.
    '''
    name, stack = stack
    assert isinstance(name, str), repr(name)
    if d.focused_viewer and not d.focused_viewer.content_id:
        d.focused_viewer.content_id = name
        pm = core.PersistMessage(world, name, thing=d.focused_viewer.lines)
        d.broadcast(pm)
        d.focused_viewer.draw_menu()
    return stack


##@install
##@SimpleFunctionWrapper
##def persist_viewer(stack):
##    if self.focused_viewer:
##        
##        self.focused_viewer.content_id = name
##        self.focused_viewer.draw_menu()
##    return stack


@install
@SimpleFunctionWrapper
def inscribe(stack):
    '''
    Create a new Joy function definition in the Joy dictionary.  A
    definition is given as a string with a name followed by a double
    equal sign then one or more Joy functions, the body. for example:

        sqr == dup mul

    If you want the definition to persist over restarts, enter it into
    the definitions.txt resource.
    '''
    definition, stack = stack
    DefinitionWrapper.add_def(definition, D)
    return stack


@install
@SimpleFunctionWrapper
def open_viewer(stack):
    '''
    Given a coordinate pair [x y] (in pixels) and a string, open a new
    unnamed viewer on that string at that location.
    '''
    ((x, (y, _)), (content, stack)) = stack
    V = d.open_viewer(x, y, text_viewer.TextViewer)
    V.lines = content.splitlines()
    V.draw()
    return stack


@install
@SimpleFunctionWrapper
def good_viewer_location(stack):
    '''
    Leave a coordinate pair [x y] (in pixels) on the stack that would
    be a good location at which to open a new viewer.  (The heuristic
    employed is to take up the bottom half of the currently open viewer
    with the greatest area.)
    '''
    viewers = list(d.iter_viewers())
    if viewers:
        viewers.sort(key=lambda (V, x, y): V.w * V.h)
        V, x, y = viewers[-1]
        coords = (x + 1, (y + V.h / 2, ()))
    else:
        coords = (0, (0, ()))
    return coords, stack


@install
@FunctionWrapper
def cmp_(stack, expression, dictionary):
    '''
    The cmp combinator takes two values and three quoted programs on the
    stack and runs one of the three depending on the results of comparing
    the two values:

           a b [G] [E] [L] cmp
        ------------------------- a > b
                G

           a b [G] [E] [L] cmp
        ------------------------- a = b
                    E

           a b [G] [E] [L] cmp
        ------------------------- a < b
                        L

    '''
    L, (E, (G, (b, (a, stack)))) = stack
    expression = concat(G if a > b else L if a < b else E, expression)
    return stack, expression, dictionary


@install
@SimpleFunctionWrapper
def list_viewers(stack):
    '''
    Put a string on the stack with some information about the currently
    open viewers, one-per-line.  This is kind of a demo function, rather
    than something really useful.
    '''
    lines = []
    for x, T in d.tracks:
        #lines.append('x: %i, w: %i, %r' % (x, T.w, T))
        for y, V in T.viewers:
            lines.append('x: %i y: %i h: %i %r %r' % (x, y, V.h, V.content_id, V))
    return '\n'.join(lines), stack


@install
@SimpleFunctionWrapper
def splitlines(stack):
    '''
    Given a string on the stack replace it with a list of the lines in
    the string.
    '''
    text, stack = stack
    assert isinstance(text, str), repr(text)
    return list_to_stack(text.splitlines()), stack


@install
@SimpleFunctionWrapper
def hiya(stack):
    '''
    Demo function to insert "Hi World!" into the current viewer, if any.
    '''
    if d.focused_viewer:
        d.focused_viewer.insert('Hi World!')
    return stack
