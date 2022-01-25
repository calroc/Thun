# -*- coding: utf-8 -*-
#
#    Copyright © 2014, 2015, 2017 Simon Forman
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
import argparse, io, pkg_resources
from .library import initialize, inscribe, Def
from .joy import repl, interp
from .utils.pretty_print import trace


inscribe(trace)


argparser = argparse.ArgumentParser(
    prog='thun',
    description='Joy Interpreter',
    )
argparser.add_argument(
    '-d', '--defs',
    action='append',
    default=[
        io.TextIOWrapper(
            pkg_resources.resource_stream(__name__, 'defs.txt'),
            encoding='UTF_8',
            )
        ],
    type=argparse.FileType('r', encoding='UTF_8'),
    help=(
        'Add additional definition files.'
        ' Can be used more than once.'
        ),
    )
argparser.add_argument(
    '-q', '--quiet',
    help='Don\'t show the prompt.',
    default=False,
    action='store_true',
    )


args = argparser.parse_args()


if args.quiet:
    j = interp
else:
    j = repl
    print('''\
Thun - Copyright © 2017 Simon Forman
This program comes with ABSOLUTELY NO WARRANTY; for details type "warranty".
This is free software, and you are welcome to redistribute it
under certain conditions; type "sharing" for details.
Type "words" to see a list of all words, and "[<name>] help" to print the
docs for a word.
''')

dictionary=initialize()
for def_stream in args.defs:
    Def.load_definitions(def_stream, dictionary)
stack = j(dictionary=dictionary)
