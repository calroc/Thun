s = ''''''
s = s.splitlines()
s = ' '.join(s)
s = s.replace('), ', '),\n')
with open('thun/asm-dump.txt', 'wb') as f:
    f.write(s)