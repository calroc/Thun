from collections import namedtuple
from re import compile as RE

Snippet = namedtuple('Snippet', 'sha offset length')
fmt = '{%s %i %i}'
pat = (
    '{'
    '\s*'
    '(?P<sha>[a-f0-9]+)'
    '\s+'
    '(?P<offset>\d+)'
    '\s+'
    '(?P<length>\d+)'
    '\s*'
    '}'
    )
PAT = RE(pat)


def to_string(snip):
    return fmt % _ts(*snip)

def _ts(sha, offset, length):
    return sha.decode('ascii'), offset, length

def from_string(text):
    m = PAT.match(text)
    if not m:
        raise ValueError
    return _fs(**m.groupdict())

def _fs(sha, offset, length):
    return Snippet(sha.encode('ascii'), int(offset), int(length))

