import sys

SEP = '•'

lines = sys.stdin.readlines()
indicies = [line.find(SEP) for line in lines]

MAX = max(indicies)

prefix_counts = (MAX - i for i in indicies)

for count, line in zip(prefix_counts, lines):
    print(' ' * count, line, sep='', end = '')
