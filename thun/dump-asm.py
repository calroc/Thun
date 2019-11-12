s = ' '.join('''\
'''.splitlines()).replace('), ', '),\n')

with open('./asm-dump.txt', 'wb') as f:
    f.write(s)