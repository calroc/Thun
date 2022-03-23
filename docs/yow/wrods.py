'''
A brutal little script to rip out the ReST docstrings and emit
ReST files for Pandoc to transform into Markdown.

This is a one-off script.

'''
from pathlib import Path
from string import ascii_letters, digits
from textwrap import dedent
from unicodedata import name

from joy.library import initialize, default_defs


refdir = Path('../reference')
okay = set(ascii_letters + digits + '_')

# Initialize the Joy dictionary.
D = initialize()
default_defs(D)

words = sorted(
    name
    for name in D
    if not name.startswith('_')
    )

# words = '!- != % & && * + ++ - -- / // /floor < << <<{} <= <> <{} = > >= >> ? ^ _Tree_add_Ee _Tree_delete_R0 _Tree_delete_clear_stuff _Tree_get_E _map0 _map1 _map2 _map? _mape _step0 _step1 _stept _times0 _times1 _timest abs add anamorphism and app1 app2 app3 appN at average b binary bool branch ccccons ccons choice clear cleave clop cmp codi codireco concat cond cons dinfrirst dip dipd dipdd disenstacken div divmod down_to_zero drop dup dupd dupdd dupdip dupdipd enstacken eq first first_two flatten floor floordiv fork fourth gcd gcd2 ge genrec getitem grabN grba gt help hypot i id ifte ii infra infrst inscribe le loop lshift lt make_generator map max min mod modulus mul ne neg not nulco nullary of or over pam pick pm pop popd popdd popop popopd popopdd popopop pow pred primrec product quoted range range_to_zero reco rem remainder remove rest reverse roll< roll> rolldown rollup round rrest rshift run second select sharing shift shunt size sort spiral_next split_at split_list sqr sqrt stack stackd step step_zero stuncons stununcons sub succ sum swaack swap swapd swoncat swons tailrec take ternary third times trace truthy tuck unary uncons unique unit unquoted unswons void warranty while words x xor zip || •'.split()


def units_of_filename(fn):
    unit = ''
    for char in fn:
        if char not in okay:
            if unit:
                yield unit
                unit = ''
            yield name(char).replace(' ', '-')
        else:
            unit += char
    if unit:
        yield unit


def safe_filename(fn):
    return '•'.join(units_of_filename(fn))

done_manually = '''\
app1.md
b.md
binary.md
ccons.md
cons.md
i.md
infra.md
nullary.md
ternary.md
unary.md
uncons.md
x.md'''.split()

for w in words:

    ww = safe_filename(w)                    
    rst = f'{ww}.rst'

    fn = f'{w}.md'
    if fn in done_manually:
        continue

##    rf = (refdir / fn)
##    if rf.exists():
##        print(f'copying existing {fn}')
##        Path(fn).write_bytes(rf.read_bytes())
##        continue

    Path(rst).write_text(f'''\
--------------

{w}
{'^' * (len(w) + 2)}

Basis Function Combinator

{dedent(D[w].__doc__)}

Gentzen diagram.


Definition
~~~~~~~~~~

if not basis.


Derivation
~~~~~~~~~~

if not basis.


Source
~~~~~~~~~~

if basis


Discussion
~~~~~~~~~~

Lorem ipsum.


Crosslinks
~~~~~~~~~~

Lorem ipsum.


''', encoding='UTF_8')


##    ww = w.replace('/', '∕')  # U+2215 ∕ DIVISION SLASH
##    Path(rst).touch()
##    print(D[w].__doc__)

