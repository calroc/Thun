'''
It's cheap, but it works.

Doesn't handle non-alnum names.

Because the strings are parsed at start time, rather than compile time,
it's basically the same as implementing an inscribe command
and using it to write a simple Joy script to load the defs:

    for line in defs:
        print(f'[{line}] inscribe')

Eh?

'''
import sys

defs = [line.strip() for line in open('./defs.txt')]
##defs = '''\
##abs dup 0 < [] [neg] branch
##anamorphism [pop []] swap [dip swons] genrec
##app1 grba infrst
##app2 [grba swap grba swap] dip [infrst] cons ii
##app3 3 appN
##appN [grabN] codi map reverse disenstacken
##at drop first
##average [sum] [size] cleave /
##b [i] dip i
##binary unary popd
##ccccons ccons ccons
##ccons cons cons
##cleave fork popdd
##clop cleave popdd
##codi cons dip
##codireco codi reco
##dinfrirst dip infrst
##dipd [dip] codi
##disenstacken ? [uncons ?] loop pop
##swons swap cons
##infra swons swaack [i] dip swaack'''.splitlines()


print(f'''\
/*
Auto-generated file by {' '.join(sys.argv)}
Do not edit.
*/
''')


if sys.argv[-1] == '--header':
    for line in defs:
        name, body = line.split(None, 1)
        print(f'void def_{name}(JoyListPtr stack, JoyListPtr expression);')

elif sys.argv[-1] == '--keywords':
    sys.stdout.write(open('KEYWORDS.in').read())
    for line in defs:
        name, body = line.split(None, 1)
        print(f'{name}, def_{name}')

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
        print(f'\tdef_{name}_body = text_to_expression("{body}");')
    print('}')



    print('''

/*
Last, a set of functions to go in the wordlist, one for each definition.
*/
    ''')
    for line in defs:
        name, body = line.split(None, 1)
        print(f'void def_{name}(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) {{ push_quote_onto_expression(def_{name}_body, expression); }}')