import sys

SEP = '•'

lines = sys.stdin.readlines()

indicies = [line.index(SEP) for line in lines if SEP in line]

MAX = max(indicies)

prefix_counts = [MAX - i for i in indicies]

for count, line in zip(prefix_counts, lines):
    print(' ' * count, line, sep='', end = '')
