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

#list(open('../defs.txt'))
defs = '''\
abs dup 0 < [] [neg] branch
anamorphism [pop []] swap [dip swons] genrec
app1 grba infrst
app2 [grba swap grba swap] dip [infrst] cons ii
app3 3 appN
appN [grabN] codi map reverse disenstacken
at drop first
average [sum] [size] cleave /
b [i] dip i
binary unary popd
ccccons ccons ccons
ccons cons cons
cleave fork popdd
clop cleave popdd
cmp [[>] swap] dipd [ifte] ccons [=] swons ifte
codi cons dip
codireco codi reco
dinfrirst dip infrst
dipd [dip] codi
disenstacken ? [uncons ?] loop pop'''.splitlines()


for line in defs:
    name, body = line.split(None, 1)
    print(f'{name}, def_{name}')

print()
print()
print('void')
print('init_defs(void)')
print('{')
for line in defs:
    name, body = line.split(None, 1)
    print(f'\tdef_{name}_body = text_to_expression("{body}");')
print('}')



print()
print()
for line in defs:
    name, body = line.split(None, 1)
    print(f'JoyList def_{name}_body;')


print()
print()
for line in defs:
    name, body = line.split(None, 1)
    print(f'void def_{name}(JoyListPtr stack, JoyListPtr expression);')

print()
print()
for line in defs:
    name, body = line.split(None, 1)
    print(f'void def_{name}(__attribute__((unused)) JoyListPtr stack, JoyListPtr expression) {{ push_quote(def_{name}_body, expression); }}')

