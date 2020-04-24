from __future__ import absolute_import
import sys, traceback

# To enable "hot" reloading in the IDLE shell.
for name in 'core main display viewer text_viewer stack_viewer persist_task'.split():
    try:
        del sys.modules[name]
    except KeyError:
        pass

from . import main

try:
    A = A # (screen, clock, pt), three things that we DON'T want to recreate
          # each time we restart main().
except NameError:
    A = main.init()

d = main.main(*A)
