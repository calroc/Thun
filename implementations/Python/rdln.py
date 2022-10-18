from pathlib import Path
import atexit
import os
import readline

NAMES = set(dir(__builtins__))

# Note that we WANT names here to be part of the closure of the
# function so it retains its value from call to call.
def completer(text, state, names=[]):
    if 0 == state:
        names[:] = filter(lambda name: name.startswith(text), NAMES)
    try:
        return names[state]
    except IndexError:
        return


readline.set_completer(completer)
readline.parse_and_bind('TAB: complete')

hfname = str(Path.home() / '.joy_history')
try:
    readline.read_history_file(hfname)
except FileNotFoundError:
    pass
atexit.register(readline.write_history_file, hfname)

while True:
    try:
        i = input('> ')
    except (EOFError, KeyboardInterrupt):
        i = ''
        break
    print(i)
