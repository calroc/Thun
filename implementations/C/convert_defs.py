'''
It's cheap, but it works.

I'd really like to replace this with, like, Awk or something, Python
is a big dependency for what is really a small text munging task, eh?


Because the strings are parsed at start time, rather than compile time,
it's basically the same as implementing an inscribe command
and using it to write a simple Joy script to load the defs:

    for line in defs:
        print(f'[{line}] inscribe')

Eh?

But this way the defs get entered into the Gperf wordlist at compile time
at least.

I wonder if it would be possible, and worth it, to render the definition
bodies as C literals?  To avoid parsing them at start time?  We don't want
to GC them anyway, so the fact that they wouldn't be allocated by the GC
shouldn't matter.

'''
import sys, unicodedata


#"&", def_and
#"|", def_or

SKIP = '''\
clear
cmp
first
mod
rest'''.splitlines()


def filt(name):
    '''
    Pass alphanumeric chars and underscores, convert other chars
    to their Unicode names so they can work as (portions of) C
    identifiers.
    '''
    alnum = True
    for i, ch in enumerate(name):
        if alnum:
            if ch.isalnum() or ch == '_':
                yield ch
            else:
                alnum = False
                if i:
                    yield '_'
                yield (
                    unicodedata
                    .name(ch)
                    .replace(' ', '_')
                    .replace('-', '_')
                    )
        else:
            if ch.isalnum() or ch == '_':
                alnum = True
                yield '_'
                yield ch
            else:
                yield '_'
                yield (
                    unicodedata
                    .name(ch)
                    .replace(' ', '_')
                    .replace('-', '_')
                    )


def convert_name(name):
    return name if name.isidentifier() else ''.join(filt(name))


defs = [line.strip() for line in open('./defs.txt')]



print(f'''\
/*
Auto-generated file by {' '.join(sys.argv)}
Do not edit.
*/
''')


if sys.argv[-1] == '--header':
    for line in defs:
        name, body = line.split(None, 1)
        if name in SKIP:
          continue
        name = convert_name(name)
        print(f'void def_{name}(JoyListPtr stack, JoyListPtr expression);')

elif sys.argv[-1] == '--keywords':
    sys.stdout.write(open('KEYWORDS.in').read())
    for line in defs:
        name, body = line.split(None, 1)
        if name in SKIP:
          continue
        print(f'{name}, def_{convert_name(name)}')

else:
    print('''\
#include "joy.h"
#include "definitions.h"


/*
Declare a bunch of list pointers to eventually hold the body expressions
of the definitions.
*/
    ''')
    for line in defs:
        name, body = line.split(None, 1)
        if name in SKIP:
          continue
        name = convert_name(name)
        print(f'JoyList def_{name}_body;')



    print('''

/*
Next, we want an initializer function to fill out the body pointers.
*/

void
init_defs(void)
{
    ''')
    for line in defs:
        name, body = line.split(None, 1)
        if name in SKIP:
          continue
        name = convert_name(name)
        print(f'\tdef_{name}_body = text_to_expression("{body}");')
    print('}')



    print('''

/*
Last, a set of functions to go in the wordlist, one for each definition.
*/
    ''')
    for line in defs:
        name, body = line.split(None, 1)
        if name in SKIP:
          continue
        name = convert_name(name)
        print(f'void def_{name}(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) {{ push_quote_onto_expression(def_{name}_body, expression); }}')
